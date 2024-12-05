# Load Tidyverse before running this code

# Repeat the process for each round
make_round_matchups <- function(team_locations){
  team_locations |> 
    filter(row_number() %% 2 == 1) |> 
    rename(team_A = team_id, loc_A = loc) |>
    bind_cols(team_locations |> 
                filter(row_number() %% 2 == 0)) |> 
    rename(loc_B = loc, team_B = team_id)
}

make_round_preds <- function(round_matchups, all_preds, chalk){
  if(!chalk){
  round_matchups |> 
    left_join(all_preds, by = c("team_A", "team_B")) |> 
    mutate(loc = ifelse(runif(1) < win_probA, loc_A, loc_B),
           team_id = ifelse(loc == loc_A, team_A, team_B)) |> 
    select(loc, team_id)
  } else {
    round_matchups |> 
      left_join(all_preds, by = c("team_A", "team_B")) |> 
      mutate(loc = ifelse(win_probA > 0.5, loc_A, loc_B),
             team_id = ifelse(loc == loc_A, team_A, team_B)) |>
      select(loc, team_id)
  }
}

simulate_bracket <- function(team_locations, all_preds, chalk){
  slots <- c("R1W1", "R1W8", "R1W5", "R1W4", "R1W6", "R1W3", "R1W7", "R1W2",
             "R1X1", "R1X8", "R1X5", "R1X4", "R1X6", "R1X3", "R1X7", "R1X2",
             "R1Y1", "R1Y8", "R1Y5", "R1Y4", "R1Y6", "R1Y3", "R1Y7", "R1Y2",
             "R1Z1", "R1Z8", "R1Z5", "R1Z4", "R1Z6", "R1Z3", "R1Z7", "R1Z2",
             "R2W1", "R2W4", "R2W3", "R2W2", "R2X1", "R2X4", "R2X3", "R2X2",
             "R2Y1", "R2Y4", "R2Y3", "R2Y2", "R2Z1", "R2Z4", "R2Z3", "R2Z2",
             "R3W1", "R3W2","R3X1", "R3X2","R3Y1", "R3Y2","R3Z1", "R3Z2",
             "R4W1","R4X1", "R4Y1","R4Z1","R5WX", "R5YZ","R6CH")
  results <- vector("list", 6)
  for(i in 1:6){
    round_matchups <- make_round_matchups(team_locations)
    team_locations <- make_round_preds(round_matchups, all_preds, chalk)
    results[[i]] <- team_locations |> select(Team = loc)
  }
  bracket <- tibble(Slot = slots,
                    Team = bind_rows(results)$Team 
                    )
}

simulate_n_brackets <- function(team_locations, all_preds, n, bracket_type = "M", chalk = F){
  all_brackets <- map(1:n, ~simulate_bracket(team_locations, all_preds, chalk), .progress = T)
  bind_rows(all_brackets) |> 
    mutate(RowID = row_number(),
           Tournament = bracket_type,
           Bracket = rep(1:n, each = 63)) |> 
    select(RowID, Tournament, Bracket, Slot, Team)
  
}

# Example usage
# Team IDs
teams <- tibble(team_id = 1:64)
team_locations <- tibble(team_id = 1:64, loc = rep(c("W", "X", "Y", "Z"), each = 16), 
                         num = rep(c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15), 4)) |> 
  mutate(loc = paste0(loc, num)) |> 
  select(-num)

all_matchups <- expand.grid(team_A = teams$team_id,team_B = teams$team_id) |> 
  filter(team_A != team_B)

all_preds <- all_matchups |> 
  mutate(win_probA = runif(4032))

brackets50 <- simulate_n_brackets(team_locations, all_preds, 50)
