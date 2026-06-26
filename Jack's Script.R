rm(list=ls())
library(tidyverse)
library(nhlscraper)
pbp_cleaned <- readRDS("pbp_cleaned.rds")
  
# outline the three types of shot events that produce xG values
shot_events <- c("goal", "shot-on-goal", "missed-shot")

# filter all faceoff events, creates notes for faceoffs with an unblocked shot attempt within 5 seconds
all_faceoffs <- pbp_cleaned |>
  arrange(gameId, periodNumber, secondsElapsedInGame, eventId) |>
  group_by(gameId, periodNumber) |>
  mutate(faceoff_number = cumsum(eventTypeDescKey == "faceoff"),
         faceoff_eventId = if_else(eventTypeDescKey == "faceoff", eventId, NA),
         faceoff_time = if_else(eventTypeDescKey == "faceoff", secondsElapsedInGame, NA_real_)) |>
  fill(faceoff_eventId, faceoff_time, .direction = "down") |>
  filter(eventTypeDescKey == "faceoff" | (eventTypeDescKey %in% shot_events & 
                                            secondsElapsedInGame - faceoff_time <= 5)) |>
  mutate(event_note = case_when(
      eventTypeDescKey == "faceoff" ~ "faceoff",
      eventTypeDescKey %in% shot_events ~ "shot within 5 seconds of faceoff"),
      seconds_after_faceoff = secondsElapsedInGame - faceoff_time) |>
  ungroup()

# filter faceoff events where the any team recorded an unblocked shot attempt within 5 seconds
faceoffs_with_shots <- all_faceoffs |>
  group_by(gameId, periodNumber, faceoff_eventId) |>
  filter(any(eventTypeDescKey %in% shot_events)) |>
  ungroup()

# filter faceoff events where the winning team recorded an unblocked shot attempt within 5 seconds
faceoffs_with_shots_2 <- faceoffs_with_shots |>
  group_by(faceoff_eventId) |>
  filter(n_distinct(eventOwnerTeamId) == 1) |>
  ungroup()

# count number of faceoffs where the winning team recorded an unblocked shot attempt within 5 seconds
faceoffs_with_shots_2 |>
  filter(eventTypeDescKey == "faceoff") |>
  nrow()

# count of total faceoffs in the 5 season sample
pbp_cleaned |>
  filter(eventTypeDescKey == "faceoff") |>
  nrow()

# for future reference - variables to define leverage
leverage <- pbp_cleaned |>
  select(gameId, eventId, seasonId, periodNumber, periodType, secondsElapsedInPeriod, secondsElapsedInGame,
         eventOwnerTeamId, eventTeamVenue, eventTypeDescKey, situationCode, homeIsEmptyNet, awayIsEmptyNet,
         isEmptyNetFor, isEmptyNetAgainst, homeSkaterCount, awaySkaterCount, skaterCountFor, skaterCountAgainst,
         manDifferential, strengthState, goalDifferential, homeGoals, awayGoals, goalsFor, goalsAgainst,
         xCoord, yCoord, xCoordNorm, yCoordNorm, zoneCode, winningPlayerId, losingPlayerId) |>
  filter(eventTypeDescKey == "faceoff")


