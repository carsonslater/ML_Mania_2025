# Create Women's Data to Train Model
# Load tidyverse and progress before using

library(tidyverse)
library(progress)
# Create features ---------------------------------------------------------
# from https://www.kaggle.com/code/flat831/create-feature-womens

result_regular <- read_csv("Data/WRegularSeasonCompactResults.csv")
seed <- read_csv("Data/WNCAATourneySeeds.csv")
teams <- read_csv("Data/WTeams.csv")


# Create Elo rankings for women -------------------------------------------
# from https://www.kaggle.com/code/flat831/elo-rating
result_tourney_womens <- read_csv("Data/WNCAATourneyCompactResults.csv")
result_regular_womens <- read_csv("Data/WRegularSeasonCompactResults.csv")
teams_womens <- read_csv("Data/WTeams.csv")

result_regular_womens <- result_regular_womens %>% mutate(type = "regular")
result_tourney_womens <- result_tourney_womens %>% mutate(type = "tourney")

result_merged_womens <- bind_rows(result_regular_womens, result_tourney_womens) %>% 
  arrange(Season, DayNum) %>% 
  mutate(
    WRating_before = 50,
    LRating_before = 50,
    WRating_after = 50,
    LRating_after = 50
  )
  
update_elo_rating_womens <- function(df, teams, K) {
  
  n <- nrow(df)
  pb <- progress_bar$new(total = n)
  
  for (i in 1:n) {
    
    WTeamID = df[i,3] %>% as.numeric()
    LTeamID = df[i,5] %>% as.numeric()
    
    WTeamRating_before <- teams %>% filter(TeamID == WTeamID) %>% select(Rating) %>% as.numeric()
    LTeamRating_before <- teams %>% filter(TeamID == LTeamID) %>% select(Rating) %>% as.numeric()
    WTeam_num <- teams %>% filter(TeamID == WTeamID) %>% select(num) %>% as.numeric()
    LTeam_num <- teams %>% filter(TeamID == LTeamID) %>% select(num) %>% as.numeric()
    
    WTeamRating_after <- WTeamRating_before + K*(1/(10^((WTeamRating_before-LTeamRating_before)/10)+1))
    LTeamRating_after <- LTeamRating_before - K*(1/(10^((WTeamRating_before-LTeamRating_before)/10)+1))
    
    df[i,10] <- WTeamRating_before
    df[i,11] <- LTeamRating_before
    df[i,12] <- WTeamRating_after
    df[i,13] <- LTeamRating_after
    
    
    teams[WTeam_num,3] <- WTeamRating_after
    teams[LTeam_num,3] <- LTeamRating_after
    
    pb$tick()
  }
  
  return(df)
}

K <- 1

teams_add_womens <- teams_womens %>% mutate(Rating = 50, num = row_number())

res_womens <- update_elo_rating_womens(result_merged_womens, teams_add_womens, K)
  

res_regular <- res_womens %>% filter(type == "regular")

tmp <- res_regular %>% select(Season, DayNum, WTeamID, WRating_before, WRating_after) %>% rename(TeamID = WTeamID, Rating_before = WRating_before, Rating_after = WRating_after)
tmp2 <- res_regular %>% select(Season, DayNum, LTeamID, LRating_before, LRating_after) %>% rename(TeamID = LTeamID, Rating_before = LRating_before, Rating_after = LRating_after)

tmp3 <- bind_rows(tmp, tmp2)

tmp4 <- tmp3 %>% group_by(Season, TeamID) %>% summarise(DayNum = max(DayNum))

elo_womens <- tmp3 %>% 
  inner_join(tmp4, by = c("Season", "TeamID", "DayNum")) %>% 
  filter(Season >= 2010) %>% 
  select(TeamID, Rating_after, Season) %>% 
  rename(Rating = Rating_after) %>% 
  left_join(teams_womens, by = c("TeamID")) %>% 
  select(TeamID, TeamName, Season, Rating)
  
elo_womens %>% write_csv("Data/EloRating_womens_10.csv")


# Continue Create Features ----------------------------------------------
elo_rating <- read_csv("Data/EloRating_womens_10.csv")

create_features <- function(team_id, result_regular, seed) {
  tmp <- result_regular %>% filter(WTeamID == team_id | LTeamID == team_id)
  seed_tmp <- seed %>% filter(TeamID == team_id) %>% mutate(Seed = as.numeric(substr(Seed, 2, 3))) %>% select(-TeamID)
  
  tmp2 <- tmp %>% rename(TeamID = WTeamID, Score = WScore, Opp_TeamID = LTeamID, Opp_Score = LScore) #%>% relocate(TeamID, Score, Opp_TeamID, Opp_Score)
  tmp3 <- tmp %>% rename(TeamID = LTeamID, Score = LScore, Opp_TeamID = WTeamID, Opp_Score = WScore) #%>% relocate(TeamID, Score, Opp_TeamID, Opp_Score)
  
  tmp4 <- bind_rows(tmp2, tmp3) %>% 
    filter(TeamID == team_id) %>% 
    mutate(
      diff_score = Score - Opp_Score,
      win = ifelse(diff_score>0, 1, 0)
    ) %>% 
    arrange(Season, DayNum)
  
  tmp5 <- tmp4 %>% 
    group_by(Season) %>% 
    summarise(
      count = n(),
      win_count = sum(win),
      win_rate = sum(win)/n(),
      gap_avg = mean(diff_score)
    ) %>% 
    left_join(seed_tmp, by = "Season") %>% 
    mutate(TeamID = team_id)
  
  last3weeks <- tmp4 %>% 
    group_by(Season) %>%
    filter(DayNum >= max(DayNum)-21) %>% 
    summarise(
      count_3w = n(),
      win_count_3w = sum(win),
      win_rate_3w = sum(win)/n(),
      gap_avg_3w = mean(diff_score)
    ) %>% 
    mutate(TeamID = team_id)
  
  tmp6 <- left_join(tmp5, last3weeks, by = c("TeamID", "Season"))
  
  return(tmp6)
}


res <- tibble()

for (team_id in teams$TeamID) {
  tmp <- create_features(team_id, result_regular, seed)
  res <- bind_rows(res, tmp)
}

df_feat <- res %>% 
  left_join(elo_rating, by =c("TeamID", "Season")) %>%
  arrange(Season, Seed)

df_feat %>% write_csv("Data/features_womens.csv")

# Create Train Data -----------------------------------------------------
# from https://www.kaggle.com/code/flat831/create-train-womens
features <- read_csv("Data/features_womens.csv")
result <- read_csv("Data/WNCAATourneyCompactResults.csv")

tmp <- result %>% 
  rename(
    TeamID_A = WTeamID, TeamID_B = LTeamID, Score_A = WScore, Score_B = LScore
  )

tmp2 <- result %>% 
  rename(
    TeamID_A = LTeamID, TeamID_B = WTeamID, Score_A = LScore, Score_B = WScore
  )

features_A <- features %>% 
  select(Season, TeamID, Seed, Rating, win_rate, gap_avg, win_rate_3w, gap_avg_3w) %>% 
  filter(is.na(Seed) == 0) %>% 
  rename(TeamID_A = TeamID, Seed_A = Seed, Rating_A = Rating, win_rate_A = win_rate, gap_avg_A = gap_avg,
         win_rate_3w_A = win_rate_3w, gap_avg_3w_A = gap_avg_3w)

features_B <- features %>% 
  select(Season, TeamID, Seed, Rating, win_rate, gap_avg, win_rate_3w, gap_avg_3w) %>% 
  filter(is.na(Seed) == 0) %>% 
  rename(TeamID_B = TeamID, Seed_B = Seed, Rating_B = Rating, win_rate_B = win_rate, gap_avg_B = gap_avg,
         win_rate_3w_B = win_rate_3w, gap_avg_3w_B = gap_avg_3w)

result_merged <- bind_rows(tmp, tmp2) %>% 
  filter(Season >= 2010) %>% 
  left_join(features_A, by = c("Season", "TeamID_A")) %>% 
  left_join(features_B, by = c("Season", "TeamID_B")) %>% 
  mutate(
    diff_seed = Seed_A - Seed_B,
    diff_rating = Rating_A - Rating_B,
    diff_win_rate = win_rate_A - win_rate_B,
    diff_gap_avg = gap_avg_A - gap_avg_B,
    diff_score = Score_A-Score_B,
    diff_win_rate_3w = win_rate_3w_A - win_rate_3w_B,
    diff_gap_avg_3w = gap_avg_3w_A - gap_avg_3w_B,
    target = ifelse(Score_A-Score_B>0, 1, 0)
  )

result_merged %>% write_csv("train_womens.csv")

# Save a dataset that contains the women's 
# 2024 data to use for comparing model accuracy last 
# year.
womens_teams_2024 <- features_A |> filter(Season == 2024) |> 
  rename_with(~ gsub("_A$", "", .), ends_with("_A"))
womens_team_matchups_2024 <- expand_grid(TeamID_A = womens_teams_2024$TeamID,
                                         TeamID_B = womens_teams_2024$TeamID) |>
  filter(TeamID_A != TeamID_B) |> 
  left_join(womens_teams_2024, by = c("TeamID_A" = "TeamID")) |>
  select(-Season) |> 
  left_join(womens_teams_2024, by = c("TeamID_B"= "TeamID"), suffix = c("_A", "_B")) |>
  mutate(diff_seed = Seed_A - Seed_B,
         diff_rating = Rating_A - Rating_B,
         diff_win_rate = win_rate_A - win_rate_B,
         diff_gap_avg = gap_avg_A - gap_avg_B,
         diff_win_rate_3w = win_rate_3w_A - win_rate_3w_B,
         diff_gap_avg_3w = gap_avg_3w_A - gap_avg_3w_B,
         WLoc = case_when(Seed_A == 1 & Seed_B %in% c("8", "9", "16") ~ "H",
                          Seed_B == 1 & Seed_A %in% c("8", "9", "16") ~ "A", 
                          T ~ "N")) |> 
  left_join(read_csv(here::here("Data/2024_tourney_seeds.csv")), by = c("TeamID_A" = "TeamID")) |>
  select(-Tournament) |> 
  left_join(read_csv(here::here("Data/2024_tourney_seeds.csv")), by = c("TeamID_B" = "TeamID"), suffix = c("_A", "_B")) |> 
  filter(!is.na(Seed_A_A), !is.na(Seed_B_B))  

write_rds(womens_team_matchups_2024, "Data/team_matchups_2024_w.rds") 
