# This example shows how to use step_pca() with 
# our glmnet example from earlier. Once again tidymodels should
# be use before running the code below

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
                       POM_A + POM_B + quad_wins_A + quad_wins_B + Seed_A + Seed_B + 
                       Ast_A + Blk_A + DR_A + FGA_A + FGM_A + FTA_A + FTM_A + FGA3_A +
                       FGM3_A + OR_A + PF_A + Score_A + Stl_A + TO_A + avg_win_A + 
                       avg_win_by_A + wins_A + losses_A + Ast_B + Blk_B + DR_B +
                       FGA_B + FGM_B + FTA_B + FTM_B + FGA3_B + FGM3_B + OR_B +
                       PF_B + Score_B + Stl_B + TO_B + avg_win_B + avg_win_by_B +
                       wins_B + losses_B + conf_wins_against_B + conf_loss_against_B +
                       conf_wins_against_A + conf_loss_against_A + MOR_A + 
                       SAG_A + WLK_A + quad_loss_A + MOR_B + SAG_B + WLK_B +
                       quad_loss_B,
                     data = train_data) |> 
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |>
  step_pca(all_predictors(), threshold = 0.875)

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
