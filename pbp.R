rm(list=ls())
library(tidyverse)
library(nhlscraper)

# set the specified seasons
seasons <- c(20212022, 20222023, 20232024, 20242025, 20252026)

# generate raw play-by-play data
pbp <- seasons |>
  map_dfr(\(s) {
    message("Downloading ", s)
    
    gc_play_by_plays(season = s) |>
      mutate(
        season = s,
        highlightClip = as.character(highlightClip)
      )
  })

# add expected goals
pbp <- calculate_expected_goals(play_by_play = pbp)

# filter for regular season games
pbp <- pbp |>
  filter(gameTypeId == 2)

# create a unique eventId variable
pbp <- pbp |>
  group_by(gameId) |>
  mutate(eventId = paste0(gameId, "_", str_pad(row_number(), 4, pad = "0"))) |>
  ungroup()

# create event team venue column
pbp <- pbp |> 
  mutate(eventTeamVenue = case_when(
    isHome == TRUE ~ "home", 
    TRUE ~ "away"))

# create last event variable
pbp <- pbp |>
  arrange(gameId, eventId) |>
  group_by(gameId) |>
  mutate(lastEvent = lag(eventTypeDescKey, n = 1, default = "")) |>
  ungroup()

# create time since last event variable
pbp <- pbp |>
  arrange(gameId, eventId) |>
  group_by(gameId) |>
  mutate(TSLE = secondsElapsedInGame - (lag(secondsElapsedInGame, n = 1, default = NA))) |>
  ungroup()

# create goal, sog, USAT, SAT variables
pbp <- pbp |>
  mutate(goal = case_when(
    eventTypeDescKey == "goal" ~ 1,
    TRUE ~ 0)) |>
  mutate(sog = case_when(
    eventTypeDescKey %in% c("goal", "shot-on-goal") ~ 1,
    TRUE ~ 0)) |>
  mutate(USAT = case_when(
    eventTypeDescKey %in% c("goal", "shot-on-goal", "missed-shot") ~ 1,
    TRUE ~ 0)) |>
  mutate(SAT = case_when(
    eventTypeDescKey %in% c("goal", "shot-on-goal", "missed-shot", "blocked-shot") ~ 1,
    TRUE ~ 0))

# select variables of interest
pbp <- pbp |>
  select(gameId, eventId, seasonId, periodNumber, periodType, secondsElapsedInPeriod, secondsElapsedInGame,
         eventOwnerTeamId, eventTeamVenue, eventTypeDescKey, situationCode, isHome, homeIsEmptyNet, awayIsEmptyNet,
         isEmptyNetFor, isEmptyNetAgainst, homeSkaterCount, awaySkaterCount, skaterCountFor, skaterCountAgainst,
         manDifferential, strengthState, goalDifferential, homeGoals, awayGoals, goalsFor, goalsAgainst,
         lastEvent, TSLE, goaliePlayerIdFor, goaliePlayerIdAgainst, 
         skater1PlayerIdFor, skater1PlayerIdAgainst, skater2PlayerIdFor, skater2PlayerIdAgainst, skater3PlayerIdFor, 
         skater3PlayerIdAgainst, skater4PlayerIdFor, skater4PlayerIdAgainst, skater5PlayerIdFor, 
         skater5PlayerIdAgainst, skater6PlayerIdFor, skater6PlayerIdAgainst, homeTeamDefendingSide,
         zoneCode, xCoord, yCoord, xCoordNorm, yCoordNorm, distance, angle, shotType, goal, sog, USAT, SAT, xG, playerId,
         winningPlayerId, losingPlayerId, hittingPlayerId, hitteePlayerId, committedByPlayerId,
         drawnByPlayerId, servedByPlayerId, blockingPlayerId, goalieInNetId, shootingPlayerId, 
         scoringPlayerId, assist1PlayerId, assist2PlayerId, penaltyTypeCode, penaltyTypeDescKey,
         penaltyDuration, reason, secondaryReason)

write_rds(pbp, "pbp_cleaned.rds")

