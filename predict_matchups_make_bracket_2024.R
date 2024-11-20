# This predicts each game for 2024 using the glmnet model. 
# Later I will simulate brackets using this as well.

# Load the model
glmnet_model <- read_rds("glmnet_final_res.rds")

# Load the data
data_2024 <- read_rds("Data/team_matchups_2024.rds")

# Predict the games
glmnet_preds <- augment(glmnet_model, new_data = data_2024)

# Cut out the teams that lost the playins
