#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
#####################################################
results <- fetch_results_afltables(1897:2012)
restructure_afl_data <- function(afl_data) {
  afl_home <- data.frame(
    Date = afl_data$Date,
    Season = afl_data$Season,
    Team = afl_data$Home.Team,
    Opponent = afl_data$Away.Team,
    Result = ifelse(afl_data$Home.Points > afl_data$Away.Points, "W", "L"),
    Points_For = afl_data$Home.Points,
    Points_Against = afl_data$Away.Points,
    Spread = afl_data$Home.Points - afl_data$Away.Points,
    Played = TRUE,
    Home = TRUE,
    Game_ID = afl_data$Game,
    ELO = NA,
    Opp_ELO = NA
  )
  afl_away <- data.frame(
    Date = afl_data$Date,
    Season = afl_data$Season,
    Team = afl_data$Away.Team,
    Opponent = afl_data$Home.Team,
    Result = ifelse(afl_data$Away.Points > afl_data$Home.Points, "W", "L"),
    Points_For = afl_data$Away.Points,
    Points_Against = afl_data$Home.Points,
    Spread = afl_data$Away.Points - afl_data$Home.Points,
    Played = TRUE,
    Home = FALSE,
    Game_ID = afl_data$Game,
    ELO = NA,
    Opp_ELO = NA
  )
  return(bind_rows(afl_home, afl_away))
}
traditional_results <- restructure_afl_data(results)
colnames(traditional_results)

traditional_results <- traditional_results %>% mutate(
  ELO = 0,
  Opp_ELO = 0,
  Result = ifelse(Result == "W", 1, Result),
  Result = ifelse(Result == "L", 0, Result),
  Result = ifelse(Result == "T", 0.5, Result),
  Result = as.numeric(Result)
)

teams_elo <- unique(c(traditional_results$Team))

teams_elo <- data.frame(
  Team = teams_elo,
  ELO = 1500,
  stringsAsFactors = FALSE
)

teams_elo <- teams_elo[order(teams_elo$Team), ]

#####################################################
# The creation of ELO
library(elo)

for(i in 1:nrow(traditional_results)){
  if(i %% 2 != 0){ 
    # i = 1
    print(i)
    
    Team_A <- traditional_results$Team[i]
    Team_B <- traditional_results$Team[i+1]
    
    Result_A <- traditional_results$Result[i]
    Result_B <- traditional_results$Result[i+1]
    
    ELO_A <- as.numeric(teams_elo[teams_elo$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(teams_elo[teams_elo$Team == Team_B, "ELO"])
    
    traditional_results$ELO[i] <- ELO_A
    traditional_results$Opp_ELO[i] <- ELO_B
    
    traditional_results$ELO[i+1] <- ELO_B
    traditional_results$Opp_ELO[i+1] <- ELO_A
    
    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)
    
    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)
    
    Elo_Updated_A <- ELO_A + 20 * (Result_A - E_A) # Our K value here is 20, the K value controls how much ELO moves after each game, higher more dramatically and lower less. 
    Elo_Updated_B <- ELO_B + 20 * (Result_B - E_B)
    
    teams_elo[teams_elo$Team == Team_A, "ELO"] <- Elo_Updated_A
    teams_elo[teams_elo$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}

traditional_results <- traditional_results %>%
  mutate(Elo_Difference = ELO - Opp_ELO)

traditional_results <- traditional_results %>%
  mutate(Result_Binary = Result)  
#####################################################
seasons <- 2013:2025
afl_player_stats_list <- list()
for (season in seasons) {
  results <- fetch_player_stats(season = season, source = "AFL", comp = "AFLM")
  afl_player_stats_list[[as.character(season)]] <- results
}
all_cols <- unique(unlist(lapply(afl_player_stats_list, names)))
afl_player_stats_list <- lapply(afl_player_stats_list, function(df) {
  missing_cols <- setdiff(all_cols, names(df))
  df[missing_cols] <- NA
  return(df[all_cols])
})

afl_player_stats <- do.call(rbind, afl_player_stats_list)
afl_player_stats <- afl_player_stats %>% 
  mutate(
    date = as.Date(utcStartTime),
    season =year(date)
  )
colnames(afl_player_stats)

team_game_stats <- afl_player_stats %>%
  mutate(
    round = round.roundNumber,
    team = team.name,
    home_team = home.team.name,
    opponent = ifelse(team.name == home.team.name, away.team.name, home.team.name),
    game_id = paste0(season, "_", round, "_", pmin(home.team.name, away.team.name), "_vs_", pmax(home.team.name, away.team.name))  # unique game identifier
  ) %>%
  group_by(season, round, date, venue = venue.name, game_id, team, home_team, opponent) %>%
  summarise(
    goals = sum(goals, na.rm = TRUE),
    behinds = sum(behinds, na.rm = TRUE),
    total_score = goals * 6 + behinds,
    
    kicks = sum(kicks, na.rm = TRUE),
    handballs = sum(handballs, na.rm = TRUE),
    kick_to_handball_ratio = sum(kicks / handballs, na.rm = TRUE),
    disposals = sum(disposals, na.rm = TRUE),
    marks = sum(marks, na.rm = TRUE),
    tackles = sum(tackles, na.rm = TRUE),
    contested_possessions = sum(contestedPossessions, na.rm = TRUE),
    uncontested_possessions = sum(uncontestedPossessions, na.rm = TRUE),
    contested_ratio = sum(contested_possessions/uncontested_possessions, na.rm = TRUE),
    inside50s = sum(inside50s, na.rm = TRUE),
    rebound50s = sum(rebound50s, na.rm = TRUE),
    clearances = sum(clearances.totalClearances, na.rm = TRUE),
    metres_gained = sum(metresGained, na.rm = TRUE),
    score_involvements = sum(scoreInvolvements, na.rm = TRUE),
    turnovers = sum(turnovers, na.rm = TRUE),
    intercepts = sum(intercepts, na.rm = TRUE),
    tackles_inside50 = sum(tacklesInside50, na.rm = TRUE),
    pressure_acts = sum(extendedStats.pressureActs, na.rm = TRUE),
    ground_ball_gets = sum(extendedStats.groundBallGets, na.rm = TRUE),
    contested_marks = sum(contestedMarks, na.rm = TRUE),
    one_percenters = sum(onePercenters, na.rm = TRUE),
    disposal_efficiency = mean(disposalEfficiency, na.rm = TRUE),
    clangers = sum(clangers, na.rm = TRUE),
    .groups = "drop"
  )

team_game_results <- team_game_stats %>%
  left_join(
    team_game_stats %>%
      select(game_id, opponent_score = total_score, opponent_team = team),
    by = c("game_id" = "game_id", "opponent" = "opponent_team")
  ) %>%
  mutate(
    margin = total_score - opponent_score,
    result = case_when(
      margin > 0 ~ "Win",
      margin < 0 ~ "Loss",
      TRUE ~ "Draw"
    )
  )

team_game_results <- team_game_results %>%
  mutate(
    result = case_when(
      result == "Win" ~ 1,
      result == "Loss" ~ 0,
      result == "Draw" ~ 0.5,
      TRUE ~ NA_real_
    )
  )

#####################################################
team_name_map <- c(
  "Fitzroy" = NA,  # No longer active
  "Collingwood" = "Collingwood",
  "Geelong" = "Geelong Cats",
  "Sydney" = "Sydney Swans",
  "Essendon" = "Essendon",
  "St Kilda" = "St Kilda",
  "Melbourne" = "Melbourne",
  "Carlton" = "Carlton",
  "Richmond" = "Richmond",
  "University" = NA,  # No longer active
  "Hawthorn" = "Hawthorn",
  "North Melbourne" = "North Melbourne",
  "Footscray" = "Western Bulldogs",  # Rebranded
  "West Coast" = "West Coast Eagles",
  "Brisbane Lions" = "Brisbane Lions",
  "Adelaide" = "Adelaide Crows",
  "Fremantle" = "Fremantle",
  "Port Adelaide" = "Port Adelaide",
  "Gold Coast" = "Gold Coast SUNS",
  "GWS" = "GWS GIANTS"
)
traditional_results <- traditional_results %>%
  mutate(
    team_mapped = team_name_map[Team],
    opponent_mapped = team_name_map[Opponent]
  )
final_elo_2012 <- traditional_results %>%
  filter(Season == 2012) %>%
  select(team_mapped, ELO) %>%
  group_by(team_mapped) %>%
  summarise(ELO_2012 = last(ELO), .groups = "drop") %>%
  rename(team = team_mapped)

team_game_results <- team_game_results %>%
  left_join(final_elo_2012, by = "team")

elo_table <- final_elo_2012 %>%
  filter(!is.na(team)) %>%
  mutate(current_elo = ELO_2012) %>%
  select(team, current_elo)

team_game_results <- team_game_results %>%
  arrange(date)

team_game_results$elo_before <- NA_real_
team_game_results$elo_after <- NA_real_
team_game_results$opponent_elo_before <- NA_real_
team_game_results$opponent_elo_after <- NA_real_

K <- 20

for (i in 1:nrow(team_game_results)) {
  
  team <- team_game_results$team[i]
  opp <- team_game_results$opponent[i]
  
  if (team %in% elo_table$team & opp %in% elo_table$team) {
    
    elo_team <- elo_table$current_elo[elo_table$team == team]
    elo_opp  <- elo_table$current_elo[elo_table$team == opp]
    
    R_team <- 10^(elo_team / 400)
    R_opp  <- 10^(elo_opp / 400)
    
    E_team <- R_team / (R_team + R_opp)
    E_opp  <- R_opp / (R_team + R_opp)
    
    result_team <- team_game_results$result[i]
    result_opp <- 1 - result_team  # Opponent's result is complementary
    
    new_elo_team <- elo_team + K * (result_team - E_team)
    new_elo_opp  <- elo_opp  + K * (result_opp  - E_opp)
    
    team_game_results$elo_before[i] <- elo_team
    team_game_results$elo_after[i]  <- new_elo_team
    team_game_results$opponent_elo_before[i] <- elo_opp
    team_game_results$opponent_elo_after[i]  <- new_elo_opp
    
    elo_table$current_elo[elo_table$team == team] <- new_elo_team
    elo_table$current_elo[elo_table$team == opp]  <- new_elo_opp
  }
}

final_results <- team_game_results %>%
  mutate(Elo_Difference = elo_before - opponent_elo_before)

final_results <- final_results %>%
  mutate(Home = ifelse(team == home_team, 1, 0))

final_results <- final_results %>%
  filter(result %in% c(0, 1)) # Drop draws


#####################################################
# Add features here
#####################################################
# Add team averages which teams like Richmond, Brisbane 3 peat, Hawks 3 peat dominated in
team_stats_cols <- c(  "tackles", 
                       "pressure_acts", 
                       "tackles_inside50", 
                       "ground_ball_gets", 
                       "contested_possessions", 
                       "inside50s",
                       "intercepts",
                       "contested_ratio",
                       "kick_to_handball_ratio")
final_results <- final_results %>%
  arrange(team, date) %>%
  group_by(team) %>%
  mutate(across(
    all_of(team_stats_cols),
    ~ coalesce(lag(cummean(.)), 0),
    .names = "avg_{.col}"
  )) %>%
  ungroup()

opponent_avgs <- final_results %>%
  select(date, team, starts_with("avg_")) %>%
  rename_with(~ paste0("opp_", .), starts_with("avg_")) %>%
  rename(opponent = team)

final_results <- final_results %>%
  left_join(opponent_avgs, by = c("date", "opponent"))

stat_diff_cols <- c("tackles", "pressure_acts", "tackles_inside50", "ground_ball_gets", 
                    "contested_possessions", "inside50s", "intercepts", 
                    "contested_ratio", "kick_to_handball_ratio")

for (stat in stat_diff_cols) {
  final_results[[paste0("diff_", stat)]] <- final_results[[paste0("avg_", stat)]] - final_results[[paste0("opp_avg_", stat)]]
}

#####################################################
# Add team form


#####################################################

# Run Logistic Regression Model 

win_prob_glm_1 <- glm(
  result ~ Elo_Difference + Home +
    diff_tackles +
    diff_pressure_acts +
    diff_tackles_inside50 +
    diff_ground_ball_gets +
    diff_contested_possessions +
    diff_inside50s +
    diff_intercepts +
    diff_contested_ratio +
    diff_kick_to_handball_ratio,
  data = final_results,
  family = "binomial"
)

summary(win_prob_glm_1)

## Test Model

final_results$Win_Prob_Pred <- predict(win_prob_glm_1, type = "response")

# Accuracy of the model
final_results <- final_results %>%
  mutate(GLM_Forecast = ifelse(Win_Prob_Pred > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == result, 1, 0))

glm_accuracy <- mean(final_results$GLM_Correct, na.rm = TRUE)
glm_accuracy * 100

## Filter for 2025

results_2025 <- final_results %>% filter(season == 2025)

# Accuracy of the model
results_2025 <- results_2025 %>%
  mutate(GLM_Forecast = ifelse(Win_Prob_Pred > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == result, 1, 0))

glm_accuracy_2025 <- mean(results_2025$GLM_Correct, na.rm = TRUE)
glm_accuracy_2025 * 100

# Ladder creation
# ACTUAL ladder
ladder_actual <- results_2025 %>%
  group_by(team) %>%
  summarise(
    Games = n(),
    Wins_Actual = sum(result == 1),
    Losses_Actual = sum(result == 0),
    Draws_Actual = sum(result == 0.5),
    Points_Actual = Wins_Actual * 4 + Draws_Actual * 2,
  )

# PREDICTED ladder
ladder_predicted <- results_2025 %>%
  group_by(team) %>%
  summarise(
    Wins_Pred = sum(GLM_Forecast == 1),
    Losses_Pred = sum(GLM_Forecast == 0),
    Draws_Pred = sum(Win_Prob_Pred > 0.45 & Win_Prob_Pred < 0.55),  # optional
    Points_Pred = Wins_Pred * 4 + Draws_Pred * 2,
  )

# Merge both ladders
ladder_comparison <- ladder_actual %>%
  left_join(ladder_predicted, by = "team") %>%
  arrange(desc(Points_Actual))

ladder_comparison <- ladder_comparison %>%
  mutate(Point_Diff = Points_Pred - Points_Actual,
         Rank_Actual = rank(-Points_Actual),
         Rank_Pred = rank(-Points_Pred),
         Rank_Diff = abs(Rank_Pred - Rank_Actual))

mean(ladder_comparison$Rank_Diff)
ladder_comparison












