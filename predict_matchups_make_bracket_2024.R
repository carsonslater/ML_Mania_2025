# This predicts each game for 2024 using the glmnet model. 
# The main target is to see how badly we would have done last year

# Load the model
glmnet_model <- read_rds("glmnet_final_res.rds")
xgb_model_w <- read_rds("xgb_final_res_womens.rds")

# Load the data
data_2024 <- read_rds("Data/team_matchups_2024.rds")
data_2024_w <- read_rds("Data/team_matchups_2024_w.rds")

# Predict the games
glmnet_preds <- augment(glmnet_model, new_data = data_2024)
xgb_preds_w <- augment(xgb_model_w, new_data = data_2024_w)

# This function will make a dataframe of the team locations and 
# a dataframe of the predictions when taking a dataframe consisting
# of all predictions for 64 team tournament
make_team_locs_preds <- function(df, mens = T){
  team_locs <- df |>
    select(team_id = TeamID_A, loc = Seed_A_A) |> 
    distinct() |> 
    mutate(loc = factor(loc, 
                        levels = c("W01", "W16", "W08", "W09", "W05", "W12", "W04", "W13", 
                                   "W06", "W11", "W03", "W14", "W07", "W10", "W02", "W15",
                                   "X01", "X16", "X08", "X09", "X05", "X12", "X04", "X13", 
                                   "X06", "X11", "X03", "X14", "X07", "X10", "X02", "X15",
                                   "Y01", "Y16", "Y08", "Y09", "Y05", "Y12", "Y04", "Y13", 
                                   "Y06", "Y11", "Y03", "Y14", "Y07", "Y10", "Y02", "Y15",
                                   "Z01", "Z16", "Z08", "Z09", "Z05", "Z12", "Z04", "Z13", 
                                   "Z06", "Z11", "Z03", "Z14", "Z07", "Z10", "Z02", "Z15"
                        ))) |> 
    arrange(loc) |> 
    mutate(loc = as.character(loc))
  
  if (mens){
  preds <- df |>
    select(team_A = TeamID_A, team_B = TeamID_B, win_probA = .pred_win)
  }else{
    preds <- df |>
    select(team_A = TeamID_A, team_B = TeamID_B, win_probA = .pred_W)
  }
  return(list(team_locs, preds))
}

# Make the team locations and predictions
team_locs_preds <- make_team_locs_preds(glmnet_preds)
team_locs_preds_w <- make_team_locs_preds(xgb_preds_w, mens = F)

# Use simulate_n_brackets from simulate_n_brackets.R to simulate 1000
# brackets using the glmnet predictions
brackets <- simulate_n_brackets(team_locs_preds[[1]], team_locs_preds[[2]], 1000)
chalk <- simulate_n_brackets(team_locs_preds[[1]], team_locs_preds[[2]], 1, chalk = T)

brackets_w <- simulate_n_brackets(team_locs_preds_w[[1]], team_locs_preds_w[[2]], 1000, 
                                  bracket_type = "W")
chalk_w <- simulate_n_brackets(team_locs_preds_w[[1]], team_locs_preds_w[[2]], 1, chalk = T,
                               bracket_type = "W")
# Evaluation Function
# This shows the way that the brackets are evaluated.
# The probability that each team makes it to each round is calculated
# The brier score is calculated for each round
# The brier score is calculated for the entire bracket as the 
# Average of the brier scores for each round
#Winning numbers R1 = W (1, 9, 5, 13, 11, 3, 7, 2), X (1, 9, 12, 4, 6, 3, 7, 2), 
#                     Y(1, 9, 5, 12, 11, 14, 10, 2), Z(1, 8, 5, 4, 11, 3, 7, 2)
#Winning numbers R2 = W (1, 5, 3, 2), X(1, 4, 6, 2), Y(1, 4, 11, 2), Z(1, 5, 3, 2)
#Winning numbers R3 = W (1, 3), X(4, 6), Y(4, 11), Z(1, 2)
#Winning numbers R4 = W (1), X(4), Y(11), Z(1)
#Winning numbers R5 = W1, Z1
#Winning numbers R6 = W1

true_bracket <- tibble(true_winner = c("W01", "W09", "W05", "W13", "W11", "W03", "W07", "W02", 
         "X01", "X09", "X12", "X04", "X06", "X03", "X07", "X02", 
         "Y01", "Y09", "Y05", "Y12", "Y11", "Y14", "Y10", "Y02", 
         "Z01", "Z08", "Z05", "Z04", "Z11", "Z03", "Z07", "Z02", 
         "W01", "W05", "W03", "W02","X01", "X04", "X06", "X02",
         "Y01", "Y04", "Y11", "Y02","Z01", "Z05", "Z03", "Z02",
         "W01", "W03","X04", "X06","Y04", "Y11","Z01", "Z02",
         "W01","X04","Y11","Z01","W01","Z01","W01", 
         
         "W01", "W08", "W05", "W04", "W06", "W03", "W07", "W02",
         "X01", "X08", "X05", "X04", "X06", "X03", "X07", "X02",
         "Y01", "Y08", "Y05", "Y04", "Y11", "Y03", "Y07", "Y02",
         "Z01", "Z08", "Z05", "Z04", "Z06", "Z03", "Z07", "Z02",
         "W01", "W04", "W03", "W02", "X01", "X04", "X03", "X02",
         "Y01", "Y05", "Y03", "Y02", "Z01", "Z05", "Z03", "Z07",
         "W01", "W03", "X01", "X03", "Y01", "Y03", "Z01", "Z03",
         "W01", "X03", "Y01", "Z03", "W01", "Y01", "W01"),
         Slot = rep(c("R1W1", "R1W8", "R1W5", "R1W4", "R1W6", "R1W3", "R1W7", "R1W2",
                  "R1X1", "R1X8", "R1X5", "R1X4", "R1X6", "R1X3", "R1X7", "R1X2",
                  "R1Y1", "R1Y8", "R1Y5", "R1Y4", "R1Y6", "R1Y3", "R1Y7", "R1Y2",
                  "R1Z1", "R1Z8", "R1Z5", "R1Z4", "R1Z6", "R1Z3", "R1Z7", "R1Z2",
                  "R2W1", "R2W4", "R2W3", "R2W2", "R2X1", "R2X4", "R2X3", "R2X2",
                  "R2Y1", "R2Y4", "R2Y3", "R2Y2", "R2Z1", "R2Z4", "R2Z3", "R2Z2",
                  "R3W1", "R3W2","R3X1", "R3X2","R3Y1", "R3Y2","R3Z1", "R3Z2",
                  "R4W1","R4X1", "R4Y1","R4Z1","R5WX", "R5YZ","R6CH"), 2),
         Tournament = rep(c("M", "W"), each = 63))

# true_bracket_w <- tibble(true_winner = c("W01", "W08", "W05", "W04", "W06", "W03", "W07", "W02",
#                                          "X01", "X08", "X05", "X04", "X06", "X03", "X07", "X02",
#                                          "Y01", "Y08", "Y05", "Y04", "Y11", "Y03", "Y07", "Y02",
#                                          "Z01", "Z08", "Z05", "Z04", "Z06", "Z03", "Z07", "Z02",
#                                          "W01", "W04", "W03", "W02", "X01", "X04", "X03", "X02",
#                                          "Y01", "Y05", "Y03", "Y02", "Z01", "Z05", "Z03", "Z07",
#                                          "W01", "W03", "X01", "X03", "Y01", "Y03", "Z01", "Z03",
#                                          "W01", "X03", "Y01", "Z03", "W01", "Y01", "W01"),
#                          slot = c("R1W1", "R1W8", "R1W5", "R1W4", "R1W6", "R1W3", "R1W7", "R1W2",
#                                   "R1X1", "R1X8", "R1X5", "R1X4", "R1X6", "R1X3", "R1X7", "R1X2",
#                                   "R1Y1", "R1Y8", "R1Y5", "R1Y4", "R1Y6", "R1Y3", "R1Y7", "R1Y2",
#                                   "R1Z1", "R1Z8", "R1Z5", "R1Z4", "R1Z6", "R1Z3", "R1Z7", "R1Z2",
#                                   "R2W1", "R2W4", "R2W3", "R2W2", "R2X1", "R2X4", "R2X3", "R2X2",
#                                   "R2Y1", "R2Y4", "R2Y3", "R2Y2", "R2Z1", "R2Z4", "R2Z3", "R2Z2",
#                                   "R3W1", "R3W2","R3X1", "R3X2","R3Y1", "R3Y2","R3Z1", "R3Z2",
#                                   "R4W1","R4X1", "R4Y1","R4Z1","R5WX", "R5YZ","R6CH"))
#                          

all_brackets <- bind_rows(brackets, brackets_w)
all_chalk <- bind_rows(chalk, chalk_w)


calculate_brier_score <- function(brackets){
  brackets |> 
    group_by(Tournament, Slot) |>
    count(Team) |>
    mutate(prob = n / sum(n)) |>
    full_join(true_bracket, by = c("Slot", "Tournament")) |> 
    filter(Team == true_winner) |> 
    right_join(true_bracket, by = c("Slot", "Tournament")) |> 
    mutate(prob = ifelse(is.na(prob), 0, prob),
           brier = (prob - 1)^2, 
           round = str_extract(Slot, "\\d")) |> 
    group_by(round, Tournament) |> 
    summarize(brier = mean(brier)) |> 
    group_by(Tournament) |>
    summarize(brier = mean(brier)) |> 
    pull(brier)
}

calculate_brier_score(all_brackets)
calculate_brier_score(all_chalk)
