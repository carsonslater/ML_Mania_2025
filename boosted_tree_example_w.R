# Tidymodels example for boosted trees
# Have tidyverse, tidymodels, and xgboost loaded before running the code below

# This example uses Women's data
model_data <- read_csv("train_womens.csv") |> 
  mutate(target = factor(ifelse(target == 1, "W", "L"), levels = c("L", "W")))
set.seed(77)
data_split <- initial_split(model_data, prop = 0.75, strata = target)
test_data <- testing(data_split)
train_data <- training(data_split)

# Create a cross-validation folds
cv_folds <- vfold_cv(train_data, v = 10, strata = target)

# Create a metric set
metrics <- metric_set(brier_class, accuracy, mn_log_loss)

# Set Control
ctrl <- control_resamples(save_pred = TRUE)
ctrl_bayes <- control_bayes(verbose_iter = TRUE)

# Create a recipe
xgb_rec <- recipe(target ~ Seed_A + Seed_B + diff_rating + 
                    WLoc + diff_win_rate + diff_gap_avg,
                  data = train_data) |> 
  step_dummy(all_nominal_predictors())

# Create Model Specification
xgb_spec <- boost_tree(trees = tune(), tree_depth = tune(), min_n = tune(),
                       learn_rate = tune(), mtry = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

# Create a workflow
xgb_wf <- workflow() |> 
  add_recipe(xgb_rec) |> 
  add_model(xgb_spec)

# Extract parameter set
xgb_params <- xgb_wf |>
  extract_parameter_set_dials() |> 
  update(mtry = mtry(range = c(2, 5)),
         min_n = min_n(range = c(4, 30)),
         tree_depth = tree_depth(range = c(3, 25)),
         learn_rate = learn_rate(range = c(-2, -.75)),
         trees = trees(range = c(50, 1000)))

# Initial results
xgb_init_res <-
  xgb_wf |> 
  tune_grid(
    resamples = cv_folds,
    grid = nrow(xgb_params) + 8,
    param_info = xgb_params,
    metrics = metrics
  )

# Use Bayesian optimization to search for the best hyperparameters
xgb_bayes_res <-
  xgb_wf |> 
  tune_bayes(
    resamples = cv_folds,
    initial = xgb_init_res,
    iter = 20,
    param_info = xgb_params,
    metrics = metrics,
    control = ctrl_bayes
  )

# Show the best hyperparameters
show_best(xgb_bayes_res, metric = "brier_class") |> select(-.estimator) 

# Plot the results
autoplot(xgb_bayes_res, metric = "brier_class") 

# Select best parameters
xgb_best_params <- select_best(xgb_bayes_res, metric = "brier_class")

# Finalize the workflow
xgb_final_wf <- finalize_workflow(xgb_wf, xgb_best_params)

# Last Fit
final_xgb_res <- xgb_final_wf |> 
  last_fit(split = data_split, metrics = metrics)

# Show the results
final_xgb_res |> collect_metrics()

final_xgb_res |> collect_predictions() |> 
  conf_mat(truth = target, estimate = .pred_class)

# Save the model
library(butcher)
cleaned_model <- xgb_final_wf |> 
  fit(train_data) |>
  butcher()

write_rds(cleaned_model, "xgb_final_res_womens.rds")
