# Logistic regression model with Tidymodels Example (GLMNET)

# Please note that tidymodels and tidyverse should be loaded
# before running the code below
library(tidymodels)
library(tidyverse)

# Load data
model_data <- read_rds("Data/model_data.rds")
model_data_1_tibble <- bind_rows(model_data$Train, model_data$Test) |> 
  mutate(TeamID_A = WTeamID,
         TeamID_B = LTeamID,
         gid = pmap_chr(list(TeamID_A, TeamID_B, Season), ~ str_c(sort(c(...)), collapse = "_"))) |> 
  select(-WTeamID, -LTeamID)

model_data_1_tibble |> 
  group_by(gid) |>
  summarise(n = n()) |>
  count(n < 2)


# Read in evan_miya_full_names
evan_miya_full_names <- read_rds("Data/evan_miya_full.rds") |> 
  select(-team)

glimpse(model_data_1_tibble)

# Merge the data
model_data_1_tibble <- model_data_1_tibble |> 
  left_join(evan_miya_full_names, by = c("TeamID_A" = "TeamID", "Season" = "season")) |> 
  left_join(evan_miya_full_names, by = c("TeamID_B" = "TeamID", "Season" = "season"
                                         ), suffix = c("_A", "_B")) |> 
  drop_na()

set.seed(77)
data_split <- group_initial_split(model_data_1_tibble, prop = 0.75, group = gid)
test_data <- testing(data_split)
train_data <- training(data_split)

# Create a cross-validation folds
cv_folds <- group_vfold_cv(train_data, v = 4, group = gid)


# Create a metric set
metrics <- metric_set(brier_class, accuracy, mn_log_loss)

# Set Control
ctrl <- control_resamples(save_pred = TRUE)
ctrl_bayes <- control_bayes(verbose_iter = TRUE)

# Create a recipe
glmnet_rec <- recipe(win ~ relative_rating_A + relative_rating_B
                       ,
                       # good_wins_A + good_wins_B + POM_A + POM_B +
                       # off_rating_A + off_rating_B + def_rating_A + def_rating_B,
                 data = train_data) |> 
  # step_num2factor(all_integer_predictors(), levels = c("no", "yes")) |> 
  step_normalize(all_numeric_predictors())

# Old commented out produced .733 accuracy, .188 brier, .556 log loss
# New code produced .746 accuracy, .180 brier, .536 log loss

# Create Model Specification
glmnet_spec <- logistic_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("classification")

# Create a workflow
glmnet_wf <- workflow() |> 
  add_recipe(glmnet_rec) |> 
  add_model(glmnet_spec)

# Extract parameter set
glmnet_params <- glmnet_wf |>
  extract_parameter_set_dials()

# Initial results
glmnet_init_res <-
  glmnet_wf |> 
  tune_grid(
    resamples = cv_folds,
    grid = nrow(glmnet_params) + 8,
    param_info = glmnet_params,
    metrics = metrics
  )

# Use Bayesian optimization to search for the best hyperparameters
glmnet_bayes_res <-
  glmnet_wf |> 
  tune_bayes(
    resamples = cv_folds,
    initial = glmnet_init_res,
    iter = 20,
    param_info = glmnet_params,
    control = ctrl_bayes,
    metrics = metrics
  )

# Show the best results
show_best(glmnet_bayes_res, metric = "mn_log_loss") |>  select(-.estimator)  
show_best(glmnet_bayes_res, metric = "accuracy") |>  select(-.estimator)
show_best(glmnet_bayes_res, metric = "brier_class") |>  select(-.estimator)

# Plot the best results
autoplot(glmnet_bayes_res, metric = "mn_log_loss")
autoplot(glmnet_bayes_res, metric = "accuracy")
autoplot(glmnet_bayes_res, metric = "brier_class")


# Select Best parameters
glmnet_best_params <- select_best(glmnet_bayes_res, metric = "brier_class")

# Finalize the workflow
glmnet_final_wflow <- 
  glmnet_wf |> 
  finalize_workflow(glmnet_best_params)

# Last fit
glmnet_final_res <- glmnet_final_wflow |> 
  last_fit(split = data_split, metrics = metrics)

# Collect metrics
glmnet_final_res |> 
  collect_metrics()

glmnet_final_res |> 
  collect_predictions() |> 
  conf_mat(truth = win, estimate = .pred_class)

# Check coefficients
glmnet_final_res |> 
  extract_fit_parsnip() |> 
  tidy()

# Save the final workflow
library(butcher)
cleaned_glmnet_final_res <- glmnet_final_wflow |> 
  fit(data = train_data) |> 
  butcher()

write_rds(cleaned_glmnet_final_res, "glmnet_final_res.rds")

