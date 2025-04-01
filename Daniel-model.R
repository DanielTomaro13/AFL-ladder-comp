
# Load libaries
library(dplyr) # For data manipulation
library(fitzRoy) # For AFL data
library(tidyr)

# Fetch data using FitzRoy package functions
# See FitzRoy documentation 

standard_results <- fetch_results_afltables(1897:2025)
colnames(standard_results)

player_results <- fetch_player_stats_afltables(1990:2025)
colSums(is.na(player_results))
colnames(player_results)
player_results <- player_results %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# Data Manipulation

player_results_clean <- player_results %>%
  mutate(FullName = paste(First.name, Surname)) %>%
  select(
    Season, Round, Date, Venue,
    FullName, PlayerID = ID,
    TeamPlayedFor = Playing.for,
    
    Kicks, Marks, Handballs, Disposals, Goals, Behinds,
    HitOuts = Hit.Outs, Tackles, Rebound50s = Rebounds,
    Inside50s = Inside.50s, Clearances, Clangers,
    FreesFor = Frees.For, FreesAgainst = Frees.Against,
    ContestedPossessions = Contested.Possessions,
    UncontestedPossessions = Uncontested.Possessions,
    ContestedMarks = Contested.Marks,
    MarksInside50 = Marks.Inside.50, OnePercenters = One.Percenters,
    GoalAssists = Goal.Assists,

    HomeTeam = Home.team,
    HomeQ1Goals = HQ1G, HomeQ1Behinds = HQ1B,
    HomeQ2Goals = HQ2G, HomeQ2Behinds = HQ2B,
    HomeQ3Goals = HQ3G, HomeQ3Behinds = HQ3B,
    HomeQ4Goals = HQ4G, HomeQ4Behinds = HQ4B,
    HomeETGoals = HQETG, HomeETBehinds = HQETB,
    HomeScore = Home.score,
    
    AwayTeam = Away.team,
    AwayQ1Goals = AQ1G, AwayQ1Behinds = AQ1B,
    AwayQ2Goals = AQ2G, AwayQ2Behinds = AQ2B,
    AwayQ3Goals = AQ3G, AwayQ3Behinds = AQ3B,
    AwayQ4Goals = AQ4G, AwayQ4Behinds = AQ4B,
    AwayETGoals = AQETG, AwayETBehinds = AQETB,
    AwayScore = Away.score,
    
    HomeQ1Points = HQ1P, HomeQ2Points = HQ2P,
    HomeQ3Points = HQ3P, HomeQ4Points = HQ4P,
    HomeExtraTimePoints = HQETP,
    
    AwayQ1Points = AQ1P, AwayQ2Points = AQ2P,
    AwayQ3Points = AQ3P, AwayQ4Points = AQ4P,
    AwayExtraTimePoints = AQETP,
    
    Team, Age, CareerGames = Career.Games,
    HomeOrAway = Home.Away
  )

team_game_stats <- player_results_clean %>%
  group_by(Season, Round, Date, Venue,
           TeamPlayedFor, HomeOrAway,
           HomeTeam, AwayTeam,
           HomeScore, AwayScore,
           HQ1Goals = HomeQ1Goals, HQ1Behinds = HomeQ1Behinds,
           HQ2Goals = HomeQ2Goals, HQ2Behinds = HomeQ2Behinds,
           HQ3Goals = HomeQ3Goals, HQ3Behinds = HomeQ3Behinds,
           HQ4Goals = HomeQ4Goals, HQ4Behinds = HomeQ4Behinds,
           HQETGoals = HomeETGoals, HQETBehinds = HomeETBehinds,
           AQ1Goals = AwayQ1Goals, AQ1Behinds = AwayQ1Behinds,
           AQ2Goals = AwayQ2Goals, AQ2Behinds = AwayQ2Behinds,
           AQ3Goals = AwayQ3Goals, AQ3Behinds = AwayQ3Behinds,
           AQ4Goals = AwayQ4Goals, AQ4Behinds = AwayQ4Behinds,
           AQETGoals = AwayETGoals, AQETBehinds = AwayETBehinds) %>%
  summarise(
    TotalPlayers = n(),
    Disposals = sum(Disposals, na.rm = TRUE),
    Kicks = sum(Kicks, na.rm = TRUE),
    Handballs = sum(Handballs, na.rm = TRUE),
    Marks = sum(Marks, na.rm = TRUE),
    Goals = sum(Goals, na.rm = TRUE),
    Behinds = sum(Behinds, na.rm = TRUE),
    HitOuts = sum(HitOuts, na.rm = TRUE),
    Tackles = sum(Tackles, na.rm = TRUE),
    Inside50s = sum(Inside50s, na.rm = TRUE),
    Clearances = sum(Clearances, na.rm = TRUE),
    Clangers = sum(Clangers, na.rm = TRUE),
    FreesFor = sum(FreesFor, na.rm = TRUE),
    FreesAgainst = sum(FreesAgainst, na.rm = TRUE),
    ContestedPossessions = sum(ContestedPossessions, na.rm = TRUE),
    UncontestedPossessions = sum(UncontestedPossessions, na.rm = TRUE),
    ContestedMarks = sum(ContestedMarks, na.rm = TRUE),
    MarksInside50 = sum(MarksInside50, na.rm = TRUE),
    OnePercenters = sum(OnePercenters, na.rm = TRUE),
    GoalAssists = sum(GoalAssists, na.rm = TRUE),
    .groups = "drop"
  )

team_game_stats <- team_game_stats %>%
  mutate(
    TeamScore = if_else(TeamPlayedFor == HomeTeam, HomeScore, AwayScore),
    Opponent = if_else(TeamPlayedFor == HomeTeam, AwayTeam, HomeTeam),
    OpponentScore = if_else(TeamPlayedFor == HomeTeam, AwayScore, HomeScore),
    
    Result = case_when(
      TeamScore > OpponentScore ~ "Win",
      TeamScore < OpponentScore ~ "Loss",
      TRUE ~ "Draw"
    ),
    
    MatchType = if_else(grepl("EF|QF|SF|PF|GF", Round), "Finals", "Regular Season"),
    
    Q1Goals = if_else(TeamPlayedFor == HomeTeam, HQ1Goals, AQ1Goals),
    Q1Behinds = if_else(TeamPlayedFor == HomeTeam, HQ1Behinds, AQ1Behinds),
    Q2Goals = if_else(TeamPlayedFor == HomeTeam, HQ2Goals, AQ2Goals),
    Q2Behinds = if_else(TeamPlayedFor == HomeTeam, HQ2Behinds, AQ2Behinds),
    Q3Goals = if_else(TeamPlayedFor == HomeTeam, HQ3Goals, AQ3Goals),
    Q3Behinds = if_else(TeamPlayedFor == HomeTeam, HQ3Behinds, AQ3Behinds),
    Q4Goals = if_else(TeamPlayedFor == HomeTeam, HQ4Goals, AQ4Goals),
    Q4Behinds = if_else(TeamPlayedFor == HomeTeam, HQ4Behinds, AQ4Behinds),
    ETGoals = if_else(TeamPlayedFor == HomeTeam, HQETGoals, AQETGoals),
    ETBehinds = if_else(TeamPlayedFor == HomeTeam, HQETBehinds, AQETBehinds),
    
    ELO = NA_real_,
    OpponentELO = NA_real_
  ) %>%
  select(
    Season, Round, Date, MatchType, Venue, 
    TeamPlayedFor, Opponent, HomeOrAway, Result,
    TeamScore, OpponentScore,
    Q1Goals, Q1Behinds, Q2Goals, Q2Behinds, Q3Goals, Q3Behinds,
    Q4Goals, Q4Behinds, ETGoals, ETBehinds,
    Disposals, Kicks, Handballs, Marks, Goals, Behinds,
    HitOuts, Tackles, Inside50s, Clearances, Clangers,
    FreesFor, FreesAgainst, ContestedPossessions, UncontestedPossessions,
    ContestedMarks, MarksInside50, OnePercenters, GoalAssists,
    TotalPlayers, ELO, OpponentELO
  )


# Making columns numeric
str(team_game_stats)

# Create numeric columns for ELO modeling
team_game_stats_numeric <- team_game_stats %>%
  mutate(
    Result = case_when(
      Result == "Win" ~ 1,
      Result == "Loss" ~ 0,
      Result == "Draw" ~ 0.5,
      TRUE ~ NA_real_
    ),
    HomeOrAway = case_when(
      HomeOrAway == "Home" ~ 1,
      HomeOrAway == "Away" ~ 0,
      TRUE ~ NA_real_
    ),
    MatchType = case_when(
      MatchType == "Finals" ~ 1,
      MatchType == "Regular Season" ~ 0,
      TRUE ~ NA_real_
    ),
    SignificantVenue = case_when(
      TeamPlayedFor %in% c("Adelaide", "Port Adelaide") & Venue == "Adelaide Oval" ~ 1,
      TeamPlayedFor == "Geelong" & Venue %in% c("GMHBA Stadium", "Kardinia Park") ~ 1,
      TeamPlayedFor %in% c("West Coast", "Fremantle") & Venue %in% c("Optus Stadium", "Subiaco Oval") ~ 1,
      TRUE ~ 0
    ),
    IsInterstateTeam = if_else(TeamPlayedFor %in% c(
      "West Coast", "Fremantle",      # WA
      "Adelaide", "Port Adelaide",    # SA
      "Brisbane", "Gold Coast",       # QLD
      "Sydney", "GWS"                 # NSW
    ), 1, 0),
    IsFirst5Rounds = if_else(Round <= 5, 1, 0),
    IsLast5Rounds = if_else(Round >= 19 & MatchType == 0, 1, 0),
    IsBigGameDay = if_else(weekdays(as.Date(Date)) %in% c("Thursday", "Friday", "Monday"), 1, 0)
  )

# Creating a teams data frame 
results <- team_game_stats_numeric
teams_elo <- unique(c(results$TeamPlayedFor))

teams_elo <- data.frame(
  Team = unique(results$TeamPlayedFor),
  ELO = 1500,
  stringsAsFactors = FALSE
)
teams_elo <- teams_elo[order(teams_elo$Team), ]

# The creation of ELO
library(elo)

K <- 20

for (i in seq(1, nrow(results), by = 2)) {
  print(i)
  
  Team_A <- results$TeamPlayedFor[i]
  Team_B <- results$TeamPlayedFor[i + 1]
  
  Result_A <- results$Result[i]
  Result_B <- results$Result[i + 1]
  
  ELO_A <- teams_elo$ELO[teams_elo$Team == Team_A]
  ELO_B <- teams_elo$ELO[teams_elo$Team == Team_B]
  
  results$ELO[i] <- ELO_A
  results$OpponentELO[i] <- ELO_B
  results$ELO[i + 1] <- ELO_B
  results$OpponentELO[i + 1] <- ELO_A
  
  R_A <- 10^(ELO_A / 400)
  R_B <- 10^(ELO_B / 400)
  E_A <- R_A / (R_A + R_B)
  E_B <- R_B / (R_A + R_B)
  
  New_ELO_A <- ELO_A + K * (Result_A - E_A)
  New_ELO_B <- ELO_B + K * (Result_B - E_B)
  
  teams_elo$ELO[teams_elo$Team == Team_A] <- New_ELO_A
  teams_elo$ELO[teams_elo$Team == Team_B] <- New_ELO_B
}

# Add ELO Difference to the data set
results <- results %>%
  mutate(Elo_Difference = ELO - OpponentELO)

# Run Logistic Regression Model 
# We use logistic regression as it is a classification model, there are two outcomes, win or lose therefore it will output a probability between 0-1. 

# Team averages add

results <- results%>%
  mutate(Result_Binary = Result) 
results <- results %>% filter(Result_Binary != 0.5)


colSums(is.na(results))
results <- na.omit(results)

elo_model <- glm(
  Result_Binary ~ 
    Elo_Difference + HomeOrAway + MatchType +
    SignificantVenue + IsFirst5Rounds + IsLast5Rounds +
    IsInterstateTeam + IsBigGameDay,   
  family = binomial,
  data = results
)

summary(elo_model)

# Add form eventually and also team averages, consecutive wins and losses, travel distance, primetime or not game,
# round number, season averages, player strength through brownlow votes or games etc, ELO changes over last 5 games,
# weather wetweather 1 or 0, interaction terms 

## Test Model

results$Predicted_Prob <- predict(elo_model, type = "response")

results <- results %>%
  mutate(GLM_Forecast = ifelse(Predicted_Prob > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == Result_Binary, 1, 0))

mean(results$GLM_Correct, na.rm = TRUE) * 100
## Predict and filter for 2025

results_2025 <- results %>% filter(Season == 2025)

# Accuracy of the model
results_2025 <- results_2025 %>%
  mutate(GLM_Forecast = ifelse(Predicted_Prob > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == Result_Binary, 1, 0))

glm_accuracy_2025 <- mean(results_2025$GLM_Correct, na.rm = TRUE)
glm_accuracy_2025 * 100

# Ladder creation - my dataset is called results_2025 
# ACTUAL ladder
ladder_actual <- results_2025 %>%
  group_by(TeamPlayedFor) %>%
  summarise(
    Games = n(),
    Wins_Actual = sum(Result == 1),
    Losses_Actual = sum(Result == 0),
    Draws_Actual = sum(Result == 0.5),
    WinPct_Actual = round(mean(Result) * 100, 1),
    Points_Actual = Wins_Actual * 4 + Draws_Actual * 2,
    Points_For = sum(TeamScore),
    Points_Against = sum(OpponentScore),
    Percentage_Actual = round((Points_For / Points_Against) * 100, 1)
  )

# PREDICTED ladder
ladder_predicted <- results_2025 %>%
  group_by(TeamPlayedFor) %>%
  summarise(
    Wins_Pred = sum(GLM_Forecast == 1),
    Losses_Pred = sum(GLM_Forecast == 0),
    WinPct_Pred = round(mean(GLM_Forecast) * 100, 1),
    Points_Pred = Wins_Pred * 4,
    Percentage_Pred = round((sum(TeamScore) / sum(OpponentScore)) * 100, 1)
  )

# Merge both ladders
ladder_comparison <- ladder_actual %>%
  left_join(ladder_predicted, by = "TeamPlayedFor") %>%
  arrange(desc(Points_Actual))

ladder_comparison <- ladder_comparison %>%
  mutate(Point_Diff = Points_Pred - Points_Actual,
         Rank_Actual = rank(-Points_Actual),
         Rank_Pred = rank(-Points_Pred),
         Rank_Diff = abs(Rank_Pred - Rank_Actual))

mean(ladder_comparison$Rank_Diff)
ladder_comparison












