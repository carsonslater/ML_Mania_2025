# Logistic regression model with Tidymodels Example (GLMNET)

# Please note that tidymodels and tidyverse should be loaded
# before running the code below

# Load data
model_data <- read_rds("Data/model_data.rds")
model_data_1_tibble <- bind_rows(model_data$Train, model_data$Test)

set.seed(77)
data_split <- initial_split(model_data_1_tibble, prop = 0.75, strata = win)
test_data <- testing(data_split)
train_data <- training(data_split)

# Create a cross-validation folds
cv_folds <- vfold_cv(train_data, v = 10, strata = win)

# Create a metric set
metrics <- metric_set(brier_class, accuracy, mn_log_loss)

# Set Control
ctrl <- control_resamples(save_pred = TRUE)
ctrl_bayes <- control_bayes(verbose_iter = TRUE)

# Create a recipe
glmnet_rec <- recipe(win ~ good_wins_A + good_wins_B + bad_loss_A + bad_loss_B +
                   POM_A + POM_B + quad_wins_A + quad_wins_B + Seed_A + Seed_B, 
                 data = train_data)

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
    grid = nrow(glmnet_params) + 2,
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
