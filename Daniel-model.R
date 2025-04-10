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

final_results <- final_results %>%
  arrange(team, date) %>%
  group_by(team) %>%
  mutate(
    form_last_3 = coalesce(lag(slide_dbl(result, ~mean(.x, na.rm = TRUE), .before = 2, .complete = TRUE)), 0),
    form_last_5 = coalesce(lag(slide_dbl(result, ~mean(.x, na.rm = TRUE), .before = 4, .complete = TRUE)), 0)
  ) %>%
  ungroup()
#####################################################
# Add rolling ladder

final_results <- final_results %>%
  arrange(season, round, date) %>%
  group_by(season, team) %>%
  mutate(
    cumulative_wins = lag(cumsum(result), default = 0),
    cumulative_points = lag(cumsum(result * 4), default = 0)  # 4 points per win
  ) %>%
  ungroup()

final_results <- final_results %>%
  group_by(season, round) %>%
  mutate(
    ladder_position = ifelse(round <= 1, 0, rank(-cumulative_points, ties.method = "min"))
  ) %>%
  ungroup()

opponent_ladder <- final_results %>%
  select(season, round, team, opponent_ladder_position = ladder_position)

final_results <- final_results %>%
  left_join(opponent_ladder, by = c("season", "round", "opponent" = "team"))

final_results <- final_results %>% mutate(
  ladder_diff = ladder_position - opponent_ladder_position
)
#####################################################
# Adding weather
seasons <- 2013:2025
afl_results_stats_list <- list()
for (season in seasons) {
  results <- fetch_results(season = season, source = "AFL", comp = "AFLM")
  afl_results_stats_list[[as.character(season)]] <- results
}
all_cols <- unique(unlist(lapply(afl_results_stats_list, names)))
afl_results_stats_list <- lapply(afl_results_stats_list, function(df) {
  missing_cols <- setdiff(all_cols, names(df))
  df[missing_cols] <- NA
  return(df[all_cols])
})

afl_results_stats <- do.call(rbind, afl_results_stats_list)
afl_results_stats <- afl_results_stats %>% 
  mutate(
    date = as.Date(match.date),
    season =year(date)
  )
colnames(afl_results_stats)

weather_info <- afl_results_stats %>%
  mutate(
    date = as.Date(match.date),
    season = year(date),
    round = round.roundNumber,
    home_team = match.homeTeam.name,
    away_team = match.awayTeam.name,
    game_id = paste0(season, "_", round, "_", pmin(home_team, away_team), "_vs_", pmax(home_team, away_team))
  ) %>%
  select(game_id, weather_type = weather.weatherType)

final_results <- final_results %>%
  left_join(weather_info, by = "game_id")

final_results <- final_results %>%
  mutate(
    is_bad_weather = case_when(
      grepl("Rain", weather_type, ignore.case = TRUE) ~ 1,
      grepl("Wind", weather_type, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    )
  )
#####################################################
# Adding Venue instead of home

home_grounds <- list(
  "Adelaide Crows" = c("Adelaide Oval"),
  "Port Adelaide" = c("Adelaide Oval"),
  "Brisbane Lions" = c("Gabba"),
  "Carlton" = c("MCG", "Marvel Stadium"),
  "Collingwood" = c("MCG"),
  "Essendon" = c("MCG", "Marvel Stadium"),
  "Fremantle" = c("Optus Stadium"),
  "Geelong Cats" = c("GMHBA Stadium"),
  "Gold Coast SUNS" = c("People First Stadium"),
  "GWS GIANTS" = c("ENGIE Stadium", "Manuka Oval"),
  "Hawthorn" = c("MCG", "University of Tasmania Stadium", "Marvel Stadium"),
  "Melbourne" = c("MCG"),
  "North Melbourne" = c("Marvel Stadium", "Blundstone Arena"),
  "Richmond" = c("MCG"),
  "St Kilda" = c("Marvel Stadium"),
  "Sydney Swans" = c("SCG"),
  "West Coast Eagles" = c("Optus Stadium"),
  "Western Bulldogs" = c("Marvel Stadium")
)
final_results <- final_results %>%
  rowwise() %>%
  mutate(
    home_advantage_score = case_when(
      venue %in% home_grounds[[team]] ~ 1,                    # True home
      venue %in% home_grounds[[opponent]] ~ -1,               # True away
      TRUE ~ 0                                                # Neutral/unknown
    )
  ) %>%
  ungroup()
#####################################################
# Adding rest days in between matches and short turn around
final_results <- final_results %>%
  arrange(team, date) %>%
  group_by(team) %>%
  mutate(
    rest_days = as.numeric(date - lag(date)),
    rest_days = ifelse(is.na(rest_days), 7, rest_days)  # Assume 7 days for first match
  ) %>%
  ungroup()

final_results <- final_results %>%
  mutate(
    short_turnaround = ifelse(rest_days < 6, 1, 0)
  )

opponent_rest <- final_results %>%
  select(date, team, rest_days) %>%
  rename(opponent = team, opponent_rest_days = rest_days)

final_results <- final_results %>%
  left_join(opponent_rest, by = c("date", "opponent")) %>%
  mutate(rest_diff = rest_days - opponent_rest_days)
#####################################################
# Adding is_final

final_results <- final_results %>%
  mutate(
    is_final = ifelse(round > 24, 1, 0)
  )
#####################################################

# Run Logistic Regression Model 

win_prob_glm_1 <- glm(
  result ~ Elo_Difference + home_advantage_score +
    diff_tackles +
    diff_pressure_acts +
    diff_tackles_inside50 +
    diff_ground_ball_gets +
    diff_contested_possessions +
    diff_inside50s +
    diff_intercepts +
    diff_contested_ratio +
    diff_kick_to_handball_ratio +
    form_last_5 +
    ladder_diff +
    is_bad_weather +
    rest_days +
    short_turnaround +
    is_final,
  data = final_results,
  family = "binomial"
)

summary(win_prob_glm_1)
#####################################################
# Selecting only significant variables
win_prob_glm_1 <- step(win_prob_glm_1, direction = "both", trace = FALSE)
summary(win_prob_glm_1)
#####################################################
# Test Model
final_results$Win_Prob_Pred <- predict(win_prob_glm_1, type = "response")

# Accuracy of the model
final_results <- final_results %>%
  mutate(GLM_Forecast = ifelse(Win_Prob_Pred > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == result, 1, 0))

glm_accuracy <- mean(final_results$GLM_Correct, na.rm = TRUE)
glm_accuracy * 100
#####################################################
# Calculating area under the curve
library(pROC)
roc_full <- roc(final_results$result, final_results$Win_Prob_Pred)
auc(roc_full)  # Area Under Curve
#####################################################
# Finding the best threshold

thresholds <- seq(0.3, 0.7, by = 0.01)
acc_by_thresh <- sapply(thresholds, function(t) {
  forecast <- ifelse(final_results$Win_Prob_Pred > t, 1, 0)
  mean(forecast == final_results$result, na.rm = TRUE)
})

best_thresh <- thresholds[which.max(acc_by_thresh)]
best_thresh
max(acc_by_thresh) * 100

#####################################################
# Ladder creation for GLM model
# Filter for 2025
results_2025 <- final_results %>% filter(season == 2025)

# Accuracy of the model
results_2025 <- results_2025 %>%
  mutate(GLM_Forecast = ifelse(Win_Prob_Pred > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == result, 1, 0))

glm_accuracy_2025 <- mean(results_2025$GLM_Correct, na.rm = TRUE)
glm_accuracy_2025 * 100
#####################################################
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
    Points_Pred = Wins_Pred * 4,
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
#####################################################
mean(ladder_comparison$Rank_Diff)
ladder_comparison
#####################################################
# Adding some indicators for margin

final_results <- final_results %>%
  arrange(team, date) %>%
  group_by(team) %>%
  mutate(
    avg_totalpoints = lag(cummean(total_score)),
    avg_margin = lag(cummean(margin))
  ) %>%
  ungroup()

opponent_pts_avgs <- final_results %>%
  select(date, team, avg_totalpoints, avg_margin) %>%
  rename(opponent = team) %>%
  rename_with(~ paste0("opp_", .), starts_with("avg_"))

final_results <- final_results %>%
  left_join(opponent_pts_avgs, by = c("date", "opponent")) %>%
  mutate(
    diff_avg_totalpoints = avg_totalpoints - opp_avg_totalpoints,
    diff_avg_margin = avg_margin - opp_avg_margin
  )
#####################################################
# Linear Regression - calculates margin

margin_model <- lm(
  margin ~ Elo_Difference + home_advantage_score +
    diff_tackles + diff_pressure_acts + diff_tackles_inside50 +
    diff_ground_ball_gets + diff_contested_possessions +
    diff_inside50s + diff_intercepts + diff_contested_ratio +
    diff_kick_to_handball_ratio + form_last_5 +
    ladder_diff + is_bad_weather + rest_days +
    short_turnaround + is_final + diff_avg_totalpoints + diff_avg_margin,
  data = final_results
)

summary(margin_model)
#####################################################
# Testing model

results_2025 <- final_results %>%
  filter(season == 2025) %>%
  mutate(
  Margin_Pred = predict(margin_model, newdata = .)
  )

results_2025 <- results_2025 %>%
  mutate(Margin_Pred_Result = ifelse(Margin_Pred > 0, 1, 0))

results_2025 <- results_2025 %>%
  mutate(Margin_Pred_Correct = ifelse(Margin_Pred_Result == result, 1, 0))

margin_model_accuracy <- mean(results_2025$Margin_Pred_Correct, na.rm = TRUE) * 100
margin_model_accuracy
mae_margin_2025 <- mean(abs(results_2025$margin - results_2025$Margin_Pred), na.rm = TRUE)
mae_margin_2025

set.seed(123)
results_2025 <- results_2025 %>%
  mutate(
    Simulated_Margin = rnorm(n(), mean = Margin_Pred, sd = sd(margin_model$residuals, na.rm = TRUE)),
    Simulated_Result = ifelse(Simulated_Margin > 0, 1, 0)
  )
results_2025 <- results_2025 %>% select(date, team, opponent, Simulated_Margin, Simulated_Result)
#####################################################
# Ladder creation for margin model
ladder_actual <- final_results %>%
  filter(season == 2025) %>%
  group_by(team) %>%
  summarise(
    Games = n(),
    Actual_Wins = sum(result == 1),
    Actual_Losses = sum(result == 0),
    Actual_Draws = sum(result == 0.5),
    Actual_Points = Actual_Wins * 4 + Actual_Draws * 2
  ) %>%
  arrange(desc(Actual_Points)) %>%
  mutate(Actual_Rank = rank(-Actual_Points, ties.method = "min"))


ladder_simulated <- results_2025 %>%
  group_by(team) %>%
  summarise(
    Games = n(),
    Sim_Wins = sum(Simulated_Result == 1),
    Sim_Losses = sum(Simulated_Result == 0),
    Sim_Points = Sim_Wins * 4 
  ) %>%
  arrange(desc(Sim_Points)) %>%
  mutate(Sim_Rank = rank(-Sim_Points, ties.method = "min"))

ladder_comparison <- ladder_simulated %>%
  left_join(ladder_actual, by = "team") %>%
  mutate(
    Point_Diff = Sim_Points - Actual_Points,
    Rank_Diff = abs(Sim_Rank - Actual_Rank)
  ) %>%
  arrange(Actual_Rank)

mean(ladder_comparison$Rank_Diff)
#####################################################
fixture_2025 <- fetch_fixture(season = 2025, source = "AFL", comp = "AFLM")

process_fixture <- function(fixture) {
  fixture_processed <- fixture %>%
    mutate(
      date = as.Date(utcStartTime),
      season = year(date),
      round = round.roundNumber,
      home_team = home.team.name,
      away_team = away.team.name,
      venue = venue.name,
      game_id = paste0(season, "_", round, "_", pmin(home_team, away_team), "_vs_", pmax(home_team, away_team))
    ) %>%
    select(date, season, round, venue, game_id, home_team, away_team)
  
  # Create home team entries
  home_entries <- fixture_processed %>%
    mutate(
      team = home_team,
      opponent = away_team,
      Home = 1
    )
  
  # Create away team entries
  away_entries <- fixture_processed %>%
    mutate(
      team = away_team,
      opponent = home_team,
      Home = 0
    )
  
  # Combine both entries
  bind_rows(home_entries, away_entries) %>%
    arrange(date, game_id)
}

# Process the 2025 fixture
fixture_processed <- process_fixture(fixture_2025)
#####################################################
elo_table_current <- final_results %>%
  filter(season == 2024) %>%
  group_by(team) %>%
  filter(round == max(round)) %>%
  summarise(current_elo = last(elo_after), .groups = "drop")
#####################################################
# Add team statistics from the end of 2024
team_stats_end_2024 <- final_results %>%
  filter(season == 2024) %>%
  group_by(team) %>%
  filter(round == max(round)) %>%
  select(team, all_of(paste0("avg_", team_stats_cols)), form_last_3, form_last_5, 
         avg_totalpoints, avg_margin) %>%
  distinct()
#####################################################
# Add last ladder position from 2024
ladder_2024 <- final_results %>%
  filter(season == 2024) %>%
  group_by(team) %>%
  filter(round == 24) %>%
  select(team, ladder_position) %>%
  distinct()
#####################################################
stat_diff_cols <- c(
  "avg_tackles", "avg_pressure_acts", "avg_tackles_inside50",
  "avg_ground_ball_gets", "avg_contested_possessions", "avg_inside50s",
  "avg_intercepts", "avg_contested_ratio", "avg_kick_to_handball_ratio"
)

fixture_with_features <- fixture_processed %>%
  # Add ELO ratings
  left_join(elo_table_current, by = "team") %>%
  left_join(elo_table_current %>% rename(opponent = team, opponent_elo = current_elo), 
            by = "opponent") %>%
  mutate(Elo_Difference = current_elo - opponent_elo) %>%
  # Add team stats
  left_join(team_stats_end_2024, by = "team") %>%
  left_join(team_stats_end_2024 %>% 
              rename_with(~ paste0("opp_", .), -team) %>%
              rename(opponent = team), 
            by = "opponent") %>%
  # Calculate stat differences
  mutate(
    across(
      all_of(stat_diff_cols),
      ~ .x - get(paste0("opp_", cur_column())),
      .names = "diff_{.col}"
    )
  ) %>% 
rowwise() %>%
  mutate(
    home_advantage_score = case_when(
      venue %in% home_grounds[[team]] ~ 1,               # True home
      venue %in% home_grounds[[opponent]] ~ -1,          # True away
      TRUE ~ 0                                           # Neutral
    )
  ) %>%
  ungroup() %>%
  # Add ladder difference
  left_join(ladder_2024, by = "team") %>%
  left_join(ladder_2024 %>% rename(opponent = team, opponent_ladder_position = ladder_position), 
            by = "opponent") %>%
  mutate(ladder_diff = ladder_position - opponent_ladder_position) %>%
  # Add other features
  mutate(
    is_final = ifelse(as.numeric(round) > 24, 1, 0),
    is_bad_weather = 0,  # Can't predict weather, assume neutral
    short_turnaround = 0 # Starting assumption
  )
fixture_with_features <- fixture_with_features %>%
  rename_with(~ gsub("^diff_avg_", "diff_", .), starts_with("diff_avg_"))
#####################################################
# Make predictions using the GLM model
win_prob_glm_1 <- glm(
  result ~ Elo_Difference + home_advantage_score +
    ladder_diff,
  data = final_results,
  family = "binomial"
)

summary(win_prob_glm_1)
fixture_with_features$Win_Prob_Pred <- predict(win_prob_glm_1, 
                                               newdata = fixture_with_features, 
                                               type = "response")
#####################################################
# Make predictions using the margin model
margin_model <- lm(
  result ~ Elo_Difference + home_advantage_score +
    diff_tackles +
    ladder_diff,   
    data = final_results
)
summary(margin_model)
fixture_with_features$Margin_Pred <- predict(margin_model, 
                                             newdata = fixture_with_features)
#####################################################
set.seed(2025)

fixture_with_features <- fixture_with_features %>%
  mutate(
    Simulated_Result = rbinom(n(), size = 1, prob = Win_Prob_Pred),  # 1 = team wins, 0 = opponent wins
    Sim_Winner = ifelse(Simulated_Result == 1, team, opponent),
    Sim_Loser = ifelse(Simulated_Result == 0, team, opponent)
  ) %>%
  group_by(game_id) %>%
  slice(1) %>%
  ungroup()

fixture_long <- fixture_with_features %>%
  filter(round < 25) %>%  # regular season only
  select(game_id, Sim_Winner, Sim_Loser) %>%
  pivot_longer(cols = c(Sim_Winner, Sim_Loser), names_to = "result_type", values_to = "team") %>%
  mutate(
    Sim_Win = ifelse(result_type == "Sim_Winner", 1, 0)
  )
#####################################################
ladder_prediction <- fixture_long %>%
  group_by(team) %>%
  summarise(
    Games = n(),
    Sim_Wins = sum(Sim_Win),
    Sim_Losses = Games - Sim_Wins,
    Sim_Points = Sim_Wins * 4,
  ) %>%
  arrange(desc(Sim_Points)) %>%
  mutate(Position = row_number())
#####################################################
# Simulate finals
simulate_finals <- function(ladder) {
  # Get top 8 teams
  finals_teams <- ladder_prediction %>%
    filter(Position <= 8) %>%
    select(team, Position)
  
  # Qualifying Finals: 1v4, 2v3
  qf1 <- data.frame(
    team = finals_teams$team[finals_teams$Position == 1],
    opponent = finals_teams$team[finals_teams$Position == 4],
    round = "QF1",
    home_advantage = 1
  )
  
  qf2 <- data.frame(
    team = finals_teams$team[finals_teams$Position == 2],
    opponent = finals_teams$team[finals_teams$Position == 3],
    round = "QF2",
    home_advantage = 1
  )
  
  # Elimination Finals: 5v8, 6v7
  ef1 <- data.frame(
    team = finals_teams$team[finals_teams$Position == 5],
    opponent = finals_teams$team[finals_teams$Position == 8],
    round = "EF1",
    home_advantage = 1
  )
  
  ef2 <- data.frame(
    team = finals_teams$team[finals_teams$Position == 6],
    opponent = finals_teams$team[finals_teams$Position == 7],
    round = "EF2",
    home_advantage = 1
  )
  
  # Create all first-round finals matchups
  finals_round1 <- bind_rows(qf1, qf2, ef1, ef2)
  
  # Predict results using our models
  finals_round1 <- finals_round1 %>%
    left_join(elo_table_current, by = "team") %>%
    left_join(elo_table_current %>% rename(opponent = team, opponent_elo = current_elo), 
              by = "opponent") %>%
    mutate(
      Elo_Difference = current_elo - opponent_elo,
      home_advantage_score = home_advantage,
      # Add other required predictors with reasonable values
      diff_contested_possessions = 0,
      form_last_5 = 0.6,  # Assume teams in finals have good form
      ladder_diff = 0,    # Will be calculated
      is_bad_weather = 0,
      rest_days = 7,
      is_final = 1
    )
  
  # Add ladder positions
  finals_round1 <- finals_round1 %>%
    left_join(finals_teams %>% select(team, Position), by = "team") %>%
    left_join(finals_teams %>% select(team, Position) %>% rename(opponent = team, opponent_Position = Position), 
              by = "opponent") %>%
    mutate(ladder_diff = Position - opponent_Position)
  
  # Make predictions
  finals_round1$Win_Prob = predict(win_prob_glm_1, newdata = finals_round1, type = "response")
  
  # Simulate results
  set.seed(2025)
  finals_round1 <- finals_round1 %>%
    mutate(
      win = ifelse(runif(n()) < Win_Prob, 1, 0),
      winner = ifelse(win == 1, team, opponent),
      loser = ifelse(win == 1, opponent, team)
    )
#####################################################
  # Second round of finals
  sf1 <- data.frame(
    team = finals_round1$loser[finals_round1$round == "QF1"],
    opponent = finals_round1$winner[finals_round1$round == "EF1"],
    round = "SF1",
    home_advantage = 1
  )
  
  sf2 <- data.frame(
    team = finals_round1$loser[finals_round1$round == "QF2"],
    opponent = finals_round1$winner[finals_round1$round == "EF2"],
    round = "SF2",
    home_advantage = 1
  )
  
  finals_round2 <- bind_rows(sf1, sf2)
  
  # Predict second round results
  finals_round2 <- finals_round2 %>%
    left_join(elo_table_current, by = "team") %>%
    left_join(elo_table_current %>% rename(opponent = team, opponent_elo = current_elo), 
              by = "opponent") %>%
    mutate(
      Elo_Difference = current_elo - opponent_elo,
      home_advantage_score = home_advantage,
      diff_contested_possessions = 0,
      form_last_5 = 0.6,
      ladder_diff = 0,
      is_bad_weather = 0,
      rest_days = 7,
      is_final = 1
    )
  
  finals_round2$Win_Prob = predict(win_prob_glm_1, newdata = finals_round2, type = "response")
  
  # Simulate results
  set.seed(2026)
  finals_round2 <- finals_round2 %>%
    mutate(
      win = ifelse(runif(n()) < Win_Prob, 1, 0),
      winner = ifelse(win == 1, team, opponent),
      loser = ifelse(win == 1, opponent, team)
    )
#####################################################
  # Preliminary Finals
  pf1 <- data.frame(
    team = finals_round1$winner[finals_round1$round == "QF1"],
    opponent = finals_round2$winner[finals_round2$round == "SF1"],
    round = "PF1",
    home_advantage = 1
  )
  
  pf2 <- data.frame(
    team = finals_round1$winner[finals_round1$round == "QF2"],
    opponent = finals_round2$winner[finals_round2$round == "SF2"],
    round = "PF2",
    home_advantage = 1
  )
  
  finals_round3 <- bind_rows(pf1, pf2)
  
  # Predict third round results
  finals_round3 <- finals_round3 %>%
    left_join(elo_table_current, by = "team") %>%
    left_join(elo_table_current %>% rename(opponent = team, opponent_elo = current_elo), 
              by = "opponent") %>%
    mutate(
      Elo_Difference = current_elo - opponent_elo,
      home_advantage_score = home_advantage,
      diff_contested_possessions = 0,
      form_last_5 = 0.7,
      ladder_diff = 0,
      is_bad_weather = 0,
      rest_days = 7,
      is_final = 1
    )
  
  finals_round3$Win_Prob = predict(win_prob_glm_1, newdata = finals_round3, type = "response")
  
  # Simulate results
  set.seed(2027)
  finals_round3 <- finals_round3 %>%
    mutate(
      win = ifelse(runif(n()) < Win_Prob, 1, 0),
      winner = ifelse(win == 1, team, opponent),
      loser = ifelse(win == 1, opponent, team)
    )
#####################################################
  # Grand Final
  gf <- data.frame(
    team = finals_round3$winner[finals_round3$round == "PF1"],
    opponent = finals_round3$winner[finals_round3$round == "PF2"],
    round = "GF",
    home_advantage = 0  # Neutral venue
  )
  
  # Predict Grand Final result
  gf <- gf %>%
    left_join(elo_table_current, by = "team") %>%
    left_join(elo_table_current %>% rename(opponent = team, opponent_elo = current_elo), 
              by = "opponent") %>%
    mutate(
      Elo_Difference = current_elo - opponent_elo,
      home_advantage_score = home_advantage,
      diff_contested_possessions = 0,
      form_last_5 = 0.8,
      ladder_diff = 0,
      is_bad_weather = 0,
      rest_days = 14,  # Two weeks break
      is_final = 1
    )
  
  gf$Win_Prob = predict(win_prob_glm_1, newdata = gf, type = "response")
  
  # Simulate Grand Final result
  set.seed(2028)
  gf <- gf %>%
    mutate(
      win = ifelse(runif(n()) < Win_Prob, 1, 0),
      winner = ifelse(win == 1, team, opponent),
      loser = ifelse(win == 1, opponent, team)
    )
#####################################################
  # Compile all finals results
  all_finals <- bind_rows(
    finals_round1 %>% mutate(stage = "Week 1"),
    finals_round2 %>% mutate(stage = "Week 2"),
    finals_round3 %>% mutate(stage = "Week 3"),
    gf %>% mutate(stage = "Grand Final")
  )
  
  # Return finals results and premier
  list(
    all_finals = all_finals,
    premier = gf$winner
  )
}
#####################################################
# Simulate the finals
finals_results <- simulate_finals(ladder_prediction)
#####################################################
# Function to simulate a single season
simulate_single_season <- function(fixture_data, elo_table, win_model, seed_value = NULL) {
  if (!is.null(seed_value)) {
    set.seed(seed_value)
  }
  
  # Create a copy of the fixture to work with
  fixture_sim <- fixture_data %>%
    mutate(
      Simulated_Result = rbinom(n(), size = 1, prob = Win_Prob_Pred),  # 1 = team wins, 0 = opponent wins
      Sim_Winner = ifelse(Simulated_Result == 1, team, opponent),
      Sim_Loser = ifelse(Simulated_Result == 0, team, opponent)
    ) %>%
    group_by(game_id) %>%
    slice(1) %>%
    ungroup()
  
  # Create ladder from simulated results
  fixture_long <- fixture_sim %>%
    filter(round < 25) %>%  # regular season only
    select(game_id, Sim_Winner, Sim_Loser) %>%
    pivot_longer(cols = c(Sim_Winner, Sim_Loser), names_to = "result_type", values_to = "team") %>%
    mutate(Sim_Win = ifelse(result_type == "Sim_Winner", 1, 0))
  
  ladder_sim <- fixture_long %>%
    group_by(team) %>%
    summarise(
      Games = n(),
      Sim_Wins = sum(Sim_Win),
      Sim_Losses = Games - Sim_Wins,
      Sim_Points = Sim_Wins * 4,
    ) %>%
    arrange(desc(Sim_Points)) %>%
    mutate(Position = row_number())
  
  # Simulate finals
  finals_result <- simulate_finals(ladder_sim, elo_table, win_model)
  
  # Return results
  return(list(
    ladder = ladder_sim,
    premier = finals_result$premier,
    grand_finalist = finals_result$grand_finalist,
    finals_teams = ladder_sim %>% filter(Position <= 8) %>% pull(team)
  ))
}
#####################################################
# Modified finals simulation function to work with our multi-simulation approach
simulate_finals <- function(ladder, elo_table, win_model) {
  # Get top 8 teams
  finals_teams <- ladder %>%
    filter(Position <= 8) %>%
    select(team, Position)
  
  # Qualifying Finals: 1v4, 2v3
  qf1 <- data.frame(
    team = finals_teams$team[finals_teams$Position == 1],
    opponent = finals_teams$team[finals_teams$Position == 4],
    round = "QF1",
    home_advantage = 1
  )
  
  qf2 <- data.frame(
    team = finals_teams$team[finals_teams$Position == 2],
    opponent = finals_teams$team[finals_teams$Position == 3],
    round = "QF2",
    home_advantage = 1
  )
  
  # Elimination Finals: 5v8, 6v7
  ef1 <- data.frame(
    team = finals_teams$team[finals_teams$Position == 5],
    opponent = finals_teams$team[finals_teams$Position == 8],
    round = "EF1",
    home_advantage = 1
  )
  
  ef2 <- data.frame(
    team = finals_teams$team[finals_teams$Position == 6],
    opponent = finals_teams$team[finals_teams$Position == 7],
    round = "EF2",
    home_advantage = 1
  )
  
  # Create all first-round finals matchups
  finals_round1 <- bind_rows(qf1, qf2, ef1, ef2)
  
  # Predict results using our models
  finals_round1 <- finals_round1 %>%
    left_join(elo_table %>% select(team, current_elo), by = "team") %>%
    left_join(elo_table %>% select(team, current_elo) %>% 
                rename(opponent = team, opponent_elo = current_elo),
              by = "opponent") %>%
    mutate(
      Elo_Difference = current_elo - opponent_elo,
      home_advantage_score = home_advantage,
      ladder_diff = 0  # Will be calculated
    )
  
  # Add ladder positions
  finals_round1 <- finals_round1 %>%
    left_join(finals_teams %>% select(team, Position), by = "team") %>%
    left_join(finals_teams %>% select(team, Position) %>% 
                rename(opponent = team, opponent_Position = Position),
              by = "opponent") %>%
    mutate(ladder_diff = Position - opponent_Position)
  
  # Make predictions
  finals_round1$Win_Prob = predict(win_model, newdata = finals_round1, type = "response")
  
  # Simulate results
  finals_round1 <- finals_round1 %>%
    mutate(
      win = ifelse(runif(n()) < Win_Prob, 1, 0),
      winner = ifelse(win == 1, team, opponent),
      loser = ifelse(win == 1, opponent, team)
    )
  
  # Second round of finals
  sf1 <- data.frame(
    team = finals_round1$loser[finals_round1$round == "QF1"],
    opponent = finals_round1$winner[finals_round1$round == "EF1"],
    round = "SF1",
    home_advantage = 1
  )
  
  sf2 <- data.frame(
    team = finals_round1$loser[finals_round1$round == "QF2"],
    opponent = finals_round1$winner[finals_round1$round == "EF2"],
    round = "SF2",
    home_advantage = 1
  )
  
  finals_round2 <- bind_rows(sf1, sf2)
  
  # Predict second round results
  finals_round2 <- finals_round2 %>%
    left_join(elo_table %>% select(team, current_elo), by = "team") %>%
    left_join(elo_table %>% select(team, current_elo) %>% 
                rename(opponent = team, opponent_elo = current_elo),
              by = "opponent") %>%
    mutate(
      Elo_Difference = current_elo - opponent_elo,
      home_advantage_score = home_advantage,
      ladder_diff = 0
    )
  
  finals_round2$Win_Prob = predict(win_model, newdata = finals_round2, type = "response")
  
  # Simulate results
  finals_round2 <- finals_round2 %>%
    mutate(
      win = ifelse(runif(n()) < Win_Prob, 1, 0),
      winner = ifelse(win == 1, team, opponent),
      loser = ifelse(win == 1, opponent, team)
    )
  
  # Preliminary Finals
  pf1 <- data.frame(
    team = finals_round1$winner[finals_round1$round == "QF1"],
    opponent = finals_round2$winner[finals_round2$round == "SF1"],
    round = "PF1",
    home_advantage = 1
  )
  
  pf2 <- data.frame(
    team = finals_round1$winner[finals_round1$round == "QF2"],
    opponent = finals_round2$winner[finals_round2$round == "SF2"],
    round = "PF2",
    home_advantage = 1
  )
  
  finals_round3 <- bind_rows(pf1, pf2)
  
  # Predict third round results
  finals_round3 <- finals_round3 %>%
    left_join(elo_table %>% select(team, current_elo), by = "team") %>%
    left_join(elo_table %>% select(team, current_elo) %>% 
                rename(opponent = team, opponent_elo = current_elo),
              by = "opponent") %>%
    mutate(
      Elo_Difference = current_elo - opponent_elo,
      home_advantage_score = home_advantage,
      ladder_diff = 0
    )
  
  finals_round3$Win_Prob = predict(win_model, newdata = finals_round3, type = "response")
  
  # Simulate results
  finals_round3 <- finals_round3 %>%
    mutate(
      win = ifelse(runif(n()) < Win_Prob, 1, 0),
      winner = ifelse(win == 1, team, opponent),
      loser = ifelse(win == 1, opponent, team)
    )
  
  # Grand Final
  gf <- data.frame(
    team = finals_round3$winner[finals_round3$round == "PF1"],
    opponent = finals_round3$winner[finals_round3$round == "PF2"],
    round = "GF",
    home_advantage = 0  # Neutral venue
  )
  
  # Predict Grand Final result
  gf <- gf %>%
    left_join(elo_table %>% select(team, current_elo), by = "team") %>%
    left_join(elo_table %>% select(team, current_elo) %>% 
                rename(opponent = team, opponent_elo = current_elo),
              by = "opponent") %>%
    mutate(
      Elo_Difference = current_elo - opponent_elo,
      home_advantage_score = home_advantage,
      ladder_diff = 0
    )
  
  gf$Win_Prob = predict(win_model, newdata = gf, type = "response")
  
  # Simulate Grand Final result
  gf <- gf %>%
    mutate(
      win = ifelse(runif(n()) < Win_Prob, 1, 0),
      winner = ifelse(win == 1, team, opponent),
      loser = ifelse(win == 1, opponent, team)
    )
  
  # Return premier and grand finalist
  return(list(
    premier = gf$winner,
    grand_finalist = c(gf$winner, gf$loser)
  ))
}
#####################################################
# Run 10,000 simulations
run_multiple_simulations <- function(n_sims = 10000) {
  all_teams <- unique(fixture_with_features$team)
  ladder_positions <- data.frame(team = all_teams)
  finals_appearances <- data.frame(team = all_teams, appearances = 0)
  grand_final_appearances <- data.frame(team = all_teams, appearances = 0)
  premierships <- data.frame(team = all_teams, wins = 0)
  
  progress_interval <- n_sims / 10
  cat("Running", n_sims, "simulations...\n")
  
  for (i in 1:n_sims) {
    if (i %% progress_interval == 0) {
      cat(i / n_sims * 100, "% complete\n")
    }
    
    sim_result <- simulate_single_season(
      fixture_with_features, 
      elo_table_current, 
      win_prob_glm_1, 
      seed_value = i
    )
    
    sim_ladder <- sim_result$ladder
    if (i == 1) {
      ladder_positions <- sim_ladder %>% select(team, Position)
    } else {
      ladder_positions <- ladder_positions %>%
        left_join(sim_ladder %>% select(team, Position), by = "team") %>%
        mutate(Position = Position.x + Position.y) %>%
        select(team, Position)
    }
    
    finals_appearances$appearances <- finals_appearances$appearances + 
      as.numeric(finals_appearances$team %in% sim_result$finals_teams)

        grand_final_appearances$appearances <- grand_final_appearances$appearances + 
      as.numeric(grand_final_appearances$team %in% sim_result$grand_finalist)
    
    premierships$wins <- premierships$wins + 
      as.numeric(premierships$team == sim_result$premier)
  }
  
  ladder_positions$avg_position <- ladder_positions$Position / n_sims
  ladder_positions <- ladder_positions %>% 
    arrange(avg_position) %>%
    select(team, avg_position)

  finals_appearances$percentage <- finals_appearances$appearances / n_sims * 100
  grand_final_appearances$percentage <- grand_final_appearances$appearances / n_sims * 100
  premierships$percentage <- premierships$wins / n_sims * 100
  
  return(list(
    ladder = ladder_positions,
    finals = finals_appearances %>% arrange(desc(percentage)),
    grand_final = grand_final_appearances %>% arrange(desc(percentage)),
    premier = premierships %>% arrange(desc(percentage))
  ))
}
#####################################################
# Run the simulations (adjust number if needed)
sim_results <- run_multiple_simulations(10000)

# Print summary results
cat("\n=== AFL 2025 Season Prediction (10,000 Simulations) ===\n\n")

cat("Predicted Average Ladder Positions:\n")
print(sim_results$ladder)

cat("\nPercentage of Simulations Making Finals:\n")
print(sim_results$finals %>% select(team, percentage))

cat("\nPercentage of Simulations Making Grand Final:\n")
print(sim_results$grand_final %>% select(team, percentage))

cat("\nPremiership Probabilities:\n")
print(sim_results$premier %>% select(team, percentage))
#####################################################
# Plot premiership probabilities
ggplot(sim_results$premier, aes(x = reorder(team, percentage), y = percentage)) +
  geom_col(fill = "darkblue") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "2025 AFL Premiership Probabilities (10,000 Simulations)",
       x = "Team",
       y = "Probability (%)") +
  theme(panel.grid.minor = element_blank())

# Plot finals probabilities
ggplot(sim_results$finals, aes(x = reorder(team, percentage), y = percentage)) +
  geom_col(aes(fill = percentage > 50)) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "2025 AFL Finals Probabilities (10,000 Simulations)",
       x = "Team",
       y = "Probability (%)") +
  theme(panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray70"), guide = "none")

# Visualize average ladder positions
ggplot(sim_results$ladder, aes(x = reorder(team, -avg_position), y = avg_position)) +
  geom_col(aes(fill = avg_position <= 8)) +
  geom_text(aes(label = sprintf("%.1f", avg_position)), vjust = -0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Ladder Positions (10,000 Simulations)",
       x = "Team",
       y = "Average Position") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray70"), name = "Finals") +
  scale_y_reverse(breaks = 1:18)
#####################################################
calculate_odds <- function(probability_df) {
  odds_df <- probability_df %>%
    mutate(
      implied_prob = ifelse(percentage <= 0, 0.1, percentage),
      decimal_odds = round(100 / implied_prob, 2)
    )
  
  return(odds_df)
}

premiership_odds <- calculate_odds(sim_results$premier)

premiership_odds_table <- premiership_odds %>%
  select(team, implied_prob, decimal_odds) %>%
  arrange(desc(implied_prob)) %>%
  rename(
    "Team" = team,
    "Win Probability (%)" = implied_prob,
    "Decimal Odds" = decimal_odds
  )

cat("\n=== 2025 AFL Premiership Betting Odds (10,000 Simulations) ===\n\n")
print(premiership_odds_table, row.names = FALSE)
#####################################################
# Fit Poisson model using total_score as the response
poisson_model <- glm(
  total_score ~ Elo_Difference + home_advantage_score + 
    form_last_5 + ladder_diff + 
    diff_tackles + diff_inside50s + diff_contested_possessions + 
    diff_intercepts + diff_contested_ratio + diff_kick_to_handball_ratio,
  data = final_results,
  family = poisson(link = "log")
)

summary(poisson_model)
#####################################################

fixture_with_features$Expected_Points <- predict(poisson_model, 
                                                 newdata = fixture_with_features, 
                                                 type = "response")
#####################################################
set.seed(2025)
fixture_with_features <- fixture_with_features %>%
  mutate(
    Sim_Score = rpois(n(), lambda = Expected_Points)
  )
#####################################################
poisson_sim <- fixture_with_features %>%
  dplyr::select(game_id, date, season, round, team, opponent, Sim_Score) %>%
  rename(Team_Score = Sim_Score) %>% filter(season == 2025)

opponent_scores <- poisson_sim %>%
  dplyr::select(game_id, opponent = team, team = opponent, Opponent_Score = Team_Score)

poisson_results <- poisson_sim %>%
  left_join(opponent_scores, by = c("game_id", "team", "opponent")) %>%
  mutate(
    margin = Team_Score - Opponent_Score,
    result = case_when(
      Team_Score > Opponent_Score ~ 1,
      Team_Score < Opponent_Score ~ 0,
      TRUE ~ 0.5
    )
  )
#####################################################
ladder_actual <- final_results %>%
  filter(season == 2025) %>%
  group_by(team) %>%
  summarise(
    Games = n(),
    Actual_Wins = sum(result == 1),
    Actual_Losses = sum(result == 0),
    Actual_Draws = sum(result == 0.5),
    Actual_Points = Actual_Wins * 4 + Actual_Draws * 2
  ) %>%
  arrange(desc(Actual_Points)) %>%
  mutate(Actual_Rank = row_number())

poisson_ladder <- poisson_results %>%
  filter(round < 25) %>%  # Only home & away season
  group_by(team) %>%
  summarise(
    Games = n(),
    Wins = sum(result == 1),
    Draws = sum(result == 0.5),
    Losses = sum(result == 0),
    Points = Wins * 4 + Draws * 2
  ) %>%
  arrange(desc(Points)) %>%
  mutate(Rank = row_number())

poisson_ladder_comparison <- poisson_ladder %>%
  left_join(ladder_actual, by = "team") %>%
  mutate(
    Point_Diff = Points - Actual_Points,
    Rank_Diff = abs(Rank - Actual_Rank)
  ) %>%
  arrange(Actual_Rank)

