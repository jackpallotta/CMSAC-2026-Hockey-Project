rm(list=ls())
library(tidyverse)
library(nhlscraper)
library(splines)
library(pROC)
pbp_cleaned <- readRDS("pbp_cleaned.rds")
schedule <- readRDS("schedule.rds")

# Shot events that receive expected goal (xG) values
shot_events <- c("goal", "shot-on-goal", "missed-shot")

# Given the game state at the time of a faceoff, what is the
# probability that this team wins the game?

# faceoff-winning team's perspective
faceoffs <- pbp_cleaned |>
  filter(eventTypeDescKey == "faceoff") |>
  left_join(schedule |> select(gameId, winningTeamId, awayTeamId, homeTeamId), by = "gameId") |>
  mutate(faceoff_eventId = eventId, 
         faceoffWon = 1L,
         wonGame = as.integer(eventOwnerTeamId == winningTeamId))

# mirror every faceoff from the losing team's perspective
mirrored_faceoffs <- faceoffs |>
  mutate(eventOwnerTeamId = if_else(
      eventOwnerTeamId == homeTeamId,
      awayTeamId,
      homeTeamId),
    faceoffWon = 0L,
    wonGame = as.integer(eventOwnerTeamId == winningTeamId),
    goalDifferential = -goalDifferential,
    zoneCode = case_when(
      zoneCode == "O" ~ "D",
      zoneCode == "D" ~ "O",
      TRUE ~ zoneCode),
    manDifferential = -manDifferential,
    temp = isEmptyNetFor,
    isEmptyNetFor = isEmptyNetAgainst,
    isEmptyNetAgainst = temp) |>
  select(-temp)

# build the modeling dataset
game_state_data <- bind_rows(faceoffs, mirrored_faceoffs) |>
  filter(periodNumber <= 3) |>
  arrange(gameId, periodNumber, secondsElapsedInGame, faceoff_eventId) |>
  mutate(
    
    # goal differential at +/-3 goals
    goalDifferential = case_when(
      goalDifferential <= -3 ~ -3L,
      goalDifferential >= 3 ~ 3L,
      TRUE ~ as.integer(goalDifferential)),
    
    # time remaining in regulation
    secondsRemaining = 3600 - secondsElapsedInGame,
    minutesRemaining = secondsRemaining / 60,
    
    # convert predictors to modeling variables
    goalDifferential = factor(goalDifferential),
    zoneCode = factor(zoneCode),
    manDifferential = as.integer(manDifferential),
    isEmptyNetFor = as.integer(isEmptyNetFor),
    isEmptyNetAgainst = as.integer(isEmptyNetAgainst)
  )

# fit the game state model
# estimate each team's probability of winning the game using information at moment of faceoff
# measures win probability from current game state, not faceoff value
game_state_model <- glm(
  wonGame ~
    factor(goalDifferential) *
    ns(minutesRemaining, df = 4) +
    zoneCode +
    manDifferential +
    isEmptyNetFor +
    isEmptyNetAgainst,
  family = binomial(),
  data = game_state_data)

summary(game_state_model)

# predict win probability for every observation
game_state_results <- game_state_data |>
  mutate(win_prob = predict(game_state_model, type = "response"),
    pred_decile = ntile(win_prob, 10))

# evaluate model performance
# compare predicted win probability to observed win percentage
calibration_check <- game_state_results |>
  group_by(pred_decile) |>
  summarize(
    predicted = mean(win_prob),
    actual = mean(wonGame),
    n = n(),
    .groups = "drop")

calibration_check

# area under ROC Curve (AUC)
roc_obj <- roc(
  response = game_state_results$wonGame,
  predictor = game_state_results$win_prob,
  quiet = TRUE)

auc(roc_obj)

# brier score
mean((game_state_results$wonGame - game_state_results$win_prob)^2)

# prepare for next stage: measuring what happens after a faceoff
# wp_before = probability of winning right before the faceoff
game_state_faceoffs <- game_state_results |>
  filter(faceoffWon == 1) |>
  select(gameId, periodNumber, faceoff_eventId, eventOwnerTeamId, secondsElapsedInGame,
    secondsRemaining, minutesRemaining, goalDifferential, zoneCode, manDifferential,
    isEmptyNetFor, isEmptyNetAgainst, wonGame, win_prob) |>
  rename(wp_before = win_prob)