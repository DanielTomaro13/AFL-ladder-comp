
# Load libaries
library(dplyr) # For data manipulation
library(fitzRoy) # For AFL data
library(tidyr)
#####################################################
# Fetch data using FitzRoy package functions
footywire <- fetch_player_stats_footywire(2009:2025)
footywire <- footywire %>% select(
  Date, Season, Round, Venue, Player, Team, Opposition, Status, Match_id,
  GA, CP, UP, ED, DE, CM, MI5, One.Percenters, BO, TOG, K, HB, D, M, G, B, T,
  HO, I50, CL, CG, R50, FF, FA, AF, SC
)
colnames(footywire)
colSums(is.na(footywire))
player_results <- na.omit(footywire)
#####################################################
# Data Manipulation
team_scores <- player_results %>%
  group_by(Match_id, Team, Status) %>%
  summarise(
    TeamGoals = sum(G, na.rm = TRUE),
    TeamBehinds = sum(B, na.rm = TRUE),
    TeamScore = TeamGoals * 6 + TeamBehinds,
    .groups = "drop"
  )
player_results_clean <- player_results %>%
  left_join(team_scores, by = c("Match_id", "Team", "Status")) %>%
  mutate(
    HomeTeam = ifelse(Status == "Home", Team, Opposition),
    AwayTeam = ifelse(Status == "Away", Team, Opposition),
    HomeScore = ifelse(Status == "Home", TeamScore, NA),
    AwayScore = ifelse(Status == "Away", TeamScore, NA)
  ) %>%
  group_by(Match_id) %>%
  fill(HomeScore, AwayScore, .direction = "downup") %>%
  ungroup() %>%
  select(
    Season, Round, Date, Venue,
    Player, Team, Opposition, Status,
    GA, CP, UP, ED, DE, CM, MI5, One.Percenters, BO,
    TOG, K, HB, D, M, G, B, T, HO, I50, CL, CG, R50,
    FF, FA, AF, SC,
    HomeTeam, AwayTeam, HomeScore, AwayScore,
    HomeOrAway = Status
  )

team_game_stats <- player_results_clean %>%
  group_by(
    Season, Round, Date, Venue,
    TeamPlayedFor = Team, 
    HomeOrAway,
    HomeTeam, AwayTeam,
    HomeScore, AwayScore
  ) %>%
  summarise(
    TotalPlayers = n(),
    Disposals = sum(D, na.rm = TRUE),
    Kicks = sum(K, na.rm = TRUE),
    Handballs = sum(HB, na.rm = TRUE),
    Marks = sum(M, na.rm = TRUE),
    Goals = sum(G, na.rm = TRUE),
    Behinds = sum(B, na.rm = TRUE),
    HitOuts = sum(HO, na.rm = TRUE),
    Tackles = sum(T, na.rm = TRUE),
    Inside50s = sum(I50, na.rm = TRUE),
    Clearances = sum(CL, na.rm = TRUE),
    Clangers = sum(CG, na.rm = TRUE),
    FreesFor = sum(FF, na.rm = TRUE),
    FreesAgainst = sum(FA, na.rm = TRUE),
    ContestedPossessions = sum(CP, na.rm = TRUE),
    UncontestedPossessions = sum(UP, na.rm = TRUE),
    ContestedMarks = sum(CM, na.rm = TRUE),
    MarksInside50 = sum(MI5, na.rm = TRUE),
    OnePercenters = sum(One.Percenters, na.rm = TRUE),
    GoalAssists = sum(GA, na.rm = TRUE),
    SuperCoach = sum(SC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TeamScore = if_else(TeamPlayedFor == HomeTeam, HomeScore, AwayScore),
    Opponent = if_else(TeamPlayedFor == HomeTeam, AwayTeam, HomeTeam),
    OpponentScore = if_else(TeamPlayedFor == HomeTeam, AwayScore, HomeScore),
    
    Result = case_when(
      TeamScore > OpponentScore ~ "Win",
      TeamScore < OpponentScore ~ "Loss",
      TRUE ~ "Draw"
    ),
    
    MatchType = if_else(grepl("EF|QF|SF|PF|GF", Round), "Finals", "Regular Season")
  )

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
    MatchType = if_else(MatchType == "Finals", 1, 0),
    SignificantVenue = case_when(
      TeamPlayedFor %in% c("Adelaide", "Port Adelaide") & Venue == "Adelaide Oval" ~ 1,
      TeamPlayedFor == "Geelong" & Venue %in% c("GMHBA Stadium", "Kardinia Park") ~ 1,
      TeamPlayedFor %in% c("West Coast", "Fremantle") & Venue %in% c("Optus Stadium", "Subiaco Oval") ~ 1,
      TRUE ~ 0
    ),
    IsInterstateTeam = if_else(TeamPlayedFor %in% c(
      "West Coast", "Fremantle", "Adelaide", "Port Adelaide",
      "Brisbane", "Gold Coast", "Sydney", "GWS"
    ), 1, 0),
    IsBigGameDay = if_else(weekdays(as.Date(Date)) %in% c("Thursday", "Friday", "Monday"), 1, 0),
    ELO = NA_real_,
    OpponentELO = NA_real_
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
#####################################################
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
#####################################################
# Run Logistic Regression Model 
# We use logistic regression as it is a classification model, there are two outcomes, win or lose therefore it will output a probability between 0-1. 

results <- results%>%
  mutate(Result_Binary = Result) 
results <- results %>% filter(Result_Binary != 0.5)


colSums(is.na(results))
results <- na.omit(results)

elo_model <- glm(
  Result_Binary ~ 
    Elo_Difference + HomeOrAway + MatchType +
    SignificantVenue +
    IsInterstateTeam + IsBigGameDay,   
  family = binomial,
  data = results
)

summary(elo_model)
#####################################################
# Add form eventually and also team averages, consecutive wins and losses, travel distance, primetime or not game,
# round number, season averages, player strength through brownlow votes or games etc, ELO changes over last 5 games,
# weather wetweather 1 or 0, interaction terms 
#####################################################
## Test Model

results$Predicted_Prob <- predict(elo_model, type = "response")

results <- results %>%
  mutate(GLM_Forecast = ifelse(Predicted_Prob > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == Result_Binary, 1, 0))

mean(results$GLM_Correct, na.rm = TRUE) * 100
#####################################################
## Predict and filter for 2025

results_2025 <- results %>% filter(Season == 2025)

# Accuracy of the model
results_2025 <- results_2025 %>%
  mutate(GLM_Forecast = ifelse(Predicted_Prob > 0.5, 1, 0),
         GLM_Correct = ifelse(GLM_Forecast == Result_Binary, 1, 0))

glm_accuracy_2025 <- mean(results_2025$GLM_Correct, na.rm = TRUE)
glm_accuracy_2025 * 100
#####################################################

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
#####################################################












