# Jonathan Lieb
# February 20, 2025

# This script makes the evan miya data that can be attached to the 
# regular data. 

# Load packages
library(tidyverse)

# Load old data to check compatibility
mens_modeling <- read_rds("Data/model_data.rds") |> 
  bind_rows()

glimpse(mens_modeling)

mens_names <- read_csv("Data/MTeams.csv")
glimpse(mens_names)

season = "2022-23"

seasons <- c("2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18",
             "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24")
evan_miya_full <- vector("list", length(seasons))
for (i in seq_along(seasons)) {
  # Read in data
  season <- seasons[i]
  evan_miya <- read_rds(paste0("Data/evan_miya_", season, ".rds"))
  
  
  if (season %in% c("2023-24")){
    colnames(evan_miya) <- c("relative_rank", "team", "o_rate", "d_rate", "relative_rating",
                             "opp_adjust", "pace_adj", "off_rank", "def_rank", "true_tempo",
                             "tempo_rank", "home_rank", "kills_per_game", "kills_allowed_per_game",
                             "total_kills", "total_kills_allowed", "d1_wins", "d1_losses",
                             "season")
  }
  
  # Clean data
  evan_miya_filtered <- evan_miya |> 
    select(relative_rank, team, o_rate, d_rate, relative_rating, opp_adjust, pace_adj,
           true_tempo, season) |> 
    mutate(season = as.numeric(paste0("20", str_extract(season, "(?<=-)[0-9]{2}"))),
      across(c(o_rate, d_rate, relative_rating, relative_rank, true_tempo),
                 ~ as.numeric(.x)),
           opp_adjust = factor(opp_adjust, levels = c(-0.05, .03)),
           pace_adj = factor(pace_adj, levels = c(-0.05, .03)))

  print(count(evan_miya_filtered))
  # Add season
  evan_miya_full[[i]] <- evan_miya_filtered
}

evan_miya_full <- bind_rows(evan_miya_full)
glimpse(evan_miya_full)


#Change the names above to match the names below do 
# each of the 86 teams and in alphabetical order
evan_miya_full_names <- evan_miya_full |> 
  mutate(
    team = str_replace(team, "State", "St"),
    team = str_replace(team, "Saint", "St"),
    team = str_replace(team, "Western", "W"),
    team = case_when(
    team == "Abilene Christian" ~ "Abilene Chr",
    team == "Albany" ~ "Albany",
    team == "American" ~ "American Univ",
    team == "Arkansas-Little Rock" ~ "Ark Little Rock",
    team == "Arkansas-Pine Bluff" ~ "Ark Pine Bluff",
    team == "Boston University" ~ "Boston Univ",
    team == "Cal St Bakersfield" ~ "CS Bakersfield",
    team == "Cal St Fullerton" ~ "CS Fullerton",
    team == "Cal St Northridge" ~ "CS Northridge",
    team == "California Baptist" ~ "Cal Baptist",
    team == "Central Arkansas" ~ "Cent Arkansas",
    team == "Central Connecticut" ~ "Central Conn",
    team == "Central Michigan" ~ "C Michigan",
    team == "Charleston Southern" ~ "Charleston So",
    team == "Coastal Carolina" ~ "Coastal Car",
    team == "College of Charleston" ~ "Col Charleston",
    team == "East Tennessee St" ~ "ETSU",
    team == "Eastern Illinois" ~ "E Illinois",
    team == "Eastern Kentucky" ~ "E Kentucky",
    team == "Eastern Michigan" ~ "E Michigan",
    team == "Eastern Washington" ~ "E Washington",
    team == "Fairleigh Dickinson" ~ "F Dickinson",
    team == "Florida Atlantic" ~ "FL Atlantic",
    team == "Florida Gulf Coast" ~ "FL Gulf Coast",
    team == "Florida International" ~ "Florida Intl",
    team == "Fort Wayne" ~ "PFW",
    team == "Gardner-Webb" ~ "Gardner Webb",
    team == "George Washington" ~ "G Washington",
    team == "Georgia Southern" ~ "Ga Southern",
    team == "Green Bay" ~ "WI Green Bay",
    team == "Houston Christian" ~ "Houston Chr",
    team == "Illinois-Chicago" ~ "IL Chicago",
    team == "IU Indy" ~ "IUPUI",
    team == "Kennesaw St" ~ "Kennesaw",
    team == "Kent St" ~ "Kent",
    team == "Long Island" ~ "LIU Brooklyn",
    team == "Louisiana-Lafayette" ~ "Louisiana",
    team == "Louisiana-Monroe" ~ "ULM",
    team == "Loyola Chicago" ~ "Loyola-Chicago",
    team == "Loyola Maryland" ~ "Loyola MD",
    team == "Loyola Marymount" ~ "Loy Marymount",
    team == "Maryland-Eastern Shore" ~ "MD E Shore",
    team == "Miami (Fla.)" ~ "Miami FL",
    team == "Miami (Ohio)" ~ "Miami OH",
    team == "Middle Tennessee" ~ "MTSU",
    team == "Milwaukee" ~ "WI Milwaukee",
    team == "Mississippi Valley St" ~ "MS Valley St",
    team == "Missouri-Kansas City" ~ "Missouri KC",
    team == "Monmouth" ~ "Monmouth NJ",
    team == "Mount St. Mary's" ~ "Mt St Mary's",
    team == "NC St" ~ "NC State",
    team == "North Carolina A&T" ~ "NC A&T",
    team == "North Carolina Central" ~ "NC Central",
    team == "North Dakota St" ~ "N Dakota St",
    team == "Northern Colorado" ~ "N Colorado",
    team == "Northern Illinois" ~ "N Illinois",
    team == "Northern Kentucky" ~ "N Kentucky",
    team == "Northwestern St" ~ "Northwestern LA",
    team == "Ole Miss" ~ "Mississippi",
    team == "Omaha" ~ "NE Omaha",
    team == "Queens" ~ "Queens NC",
    team == "Sacramento St" ~ "CS Sacramento",
    team == "SIU Edwardsville" ~ "SIUE",
    team == "South Carolina St" ~ "S Carolina St",
    team == "South Carolina Upstate" ~ "SC Upstate",
    team == "South Dakota St" ~ "S Dakota St",
    team == "Southeast Missouri St" ~ "SE Missouri St",
    team == "Southeastern Louisiana" ~ "SE Louisiana",
    team == "Southern" ~ "Southern Univ",
    team == "Southern Illinois" ~ "S Illinois",
    team == "Southern Mississippi" ~ "Southern Miss",
    team == "St Francis (NY)" ~ "St Francis NY",
    team == "St Francis (PA)" ~ "St Francis PA",
    team == "St Joseph's" ~ "St Joseph's PA",
    team == "St Mary's" ~ "St Mary's CA",
    team == "St Thomas (Minn.)" ~ "St Thomas MN",
    team == "Stephen F. Austin" ~ "SF Austin",
    team == "Tennessee-Martin" ~ "TN Martin",
    team == "Texas-Rio Grande Valley" ~ "UTRGV",
    team == "Texas A&M-Commerce" ~ "TX A&M Commerce",
    team == "Texas A&M-Corpus Christi" ~ "TAM C. Christi",
    team == "Texas Southern" ~ "TX Southern",
    team == "The Citadel" ~ "Citadel",
    team == "UMass Lowell" ~ "MA Lowell",
    team == "UTSA" ~ "UT San Antonio",
    team == "W Kentucky" ~ "WKU",
    TRUE ~ team
  )) |> 
  left_join(mens_names, by = c("team" = "TeamName")) |> 
  select(-c(FirstD1Season, LastD1Season)) |> 
  drop_na()

glimpse(evan_miya_full_names)
# count nas in each column
sapply(evan_miya_full_names, function(x) sum(is.na(x)))

tourney_teams <- mens_names$TeamName

names_no_match <- evan_miya_full_names |> 
  filter(is.na(TeamID)) |> 
  pull(team) |> 
  unique() |> 
  sort()


tourney_missing <- mens_names |> 
  anti_join(evan_miya_full_names, by = c("TeamName" = "team"))

names_no_match
tourney_missing$TeamName |> sort()

write_rds(evan_miya_full_names, "Data/evan_miya_full.rds")
       