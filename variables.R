rm(list=ls())
library(tidyverse)
library(nhlscraper)
pbp_cleaned <- readRDS("pbp_cleaned.rds")
schedule <- readRDS("schedule.rds")
rosters <- readRDS("rosters.rds")
players <- readRDS("players.rds")

pbp_cleaned <- pbp_cleaned |>
  mutate(coordinates = paste(xCoord, yCoord, sep = ", ")) |>
  select(gameId, eventId, seasonId, periodNumber, periodType, secondsElapsedInGame, eventOwnerTeamId,
         eventTeamVenue, eventTypeDescKey, situationCode, isHome, homeIsEmptyNet, awayIsEmptyNet, 
         isEmptyNetFor, isEmptyNetAgainst, homeSkaterCount, awaySkaterCount, skaterCountFor,
         skaterCountAgainst, manDifferential, goalDifferential, strengthState, homeTeamDefendingSide,
         zoneCode, coordinates, winningPlayerId, losingPlayerId)

# faceoff-winning team's perspective
faceoffs <- pbp_cleaned |>
  filter(eventTypeDescKey == "faceoff") |>
  left_join(schedule |> select(gameId, awayTeamId, homeTeamId, winningTeamId), by = "gameId") |>
  mutate(faceoffWon = 1L,
         wonGame = as.integer(eventOwnerTeamId == winningTeamId))

# mirror every faceoff from the losing team's perspective
mirrored_faceoffs <- faceoffs |>
  mutate(eventOwnerTeamId = if_else(
    eventOwnerTeamId == homeTeamId,
    awayTeamId,
    homeTeamId),
    eventTeamVenue = if_else(
      eventTeamVenue == "home",
      "away",
      "home"),
    faceoffWon = 0L,
    wonGame = as.integer(eventOwnerTeamId == winningTeamId),
    goalDifferential = -goalDifferential,
    strengthState = case_when(
      strengthState == "power-play"   ~ "penalty-kill",
      strengthState == "penalty-kill" ~ "power-play",
      TRUE ~ strengthState),
    zoneCode = case_when(
      zoneCode == "O" ~ "D",
      zoneCode == "D" ~ "O",
      TRUE ~ zoneCode),
    manDifferential = -manDifferential,
    temp = isEmptyNetFor,
    isEmptyNetFor = isEmptyNetAgainst,
    isEmptyNetAgainst = temp) |>
  select(-temp)

# combine the data
combined_faceoffs <- bind_rows(faceoffs, mirrored_faceoffs)

# cap the goal differential at +/- 3
combined_faceoffs <- combined_faceoffs |>
  arrange(gameId, eventId, periodNumber, secondsElapsedInGame) |>
  mutate(goalDifferential = case_when(
      goalDifferential <= -3 ~ -3L,
      goalDifferential >= 3 ~ 3L,
      TRUE ~ as.integer(goalDifferential)))

# create a variable for the faceoff player, each unique faceoff has two observations
combined_faceoffs <- combined_faceoffs |>
  mutate(faceoffPlayerId = case_when(
    faceoffWon == 1 ~ winningPlayerId,
    faceoffWon == 0 ~ losingPlayerId))

# join the players df to get the handedness of the two faceoff players
combined_faceoffs <- combined_faceoffs |>
  left_join(players |> select(season, playerId, player, shoots),
            by = c("faceoffPlayerId" = "playerId", "seasonId" = "season"))

# create a variable for whether a player takes the faceoff on his strong side
combined_faceoffs <- combined_faceoffs |>
  mutate(strongSide = case_when(
    eventTeamVenue == "home" & homeTeamDefendingSide == "left" & shoots == "L" &
      coordinates %in% c("-69, 22", "-20, 22", "20, 22", "69, 22") ~ 1,
    eventTeamVenue == "home" & homeTeamDefendingSide == "left" & shoots == "R" &
      coordinates %in% c("-69, -22", "-20, -22", "20, -22", "69, -22") ~ 1,
    
    eventTeamVenue == "away" & homeTeamDefendingSide == "left" & shoots == "L" &
      coordinates %in% c("-69, -22", "-20, -22", "20, -22", "69, -22") ~ 1,
    eventTeamVenue == "away" & homeTeamDefendingSide == "left" & shoots == "R" &
      coordinates %in% c("-69, 22", "-20, 22", "20, 22", "69, 22") ~ 1,
    
    eventTeamVenue == "home" & homeTeamDefendingSide == "right" & shoots == "L" &
      coordinates %in% c("-69, -22", "-20, -22", "20, -22", "69, -22") ~ 1,
    eventTeamVenue == "home" & homeTeamDefendingSide == "right" & shoots == "R" &
      coordinates %in% c("-69, 22", "-20, 22", "20, 22", "69, 22") ~ 1,
    
    eventTeamVenue == "away" & homeTeamDefendingSide == "right" & shoots == "L" &
      coordinates %in% c("-69, 22", "-20, 22", "20, 22", "69, 22") ~ 1,
    eventTeamVenue == "away" & homeTeamDefendingSide == "right" & shoots == "R" &
      coordinates %in% c("-69, -22", "-20, -22", "20, -22", "69, -22") ~ 1,
   
    coordinates %in% c("0, 0") ~ 0,
    TRUE ~ 0
  ))

# create a variable for which player places his stick down first
combined_faceoffs <- combined_faceoffs |>
  mutate(stickDownFirst = case_when(
    eventTeamVenue == "home" & homeTeamDefendingSide == "left" &
        coordinates %in% c("-69, 22", "-20, 22", "-69, -22", "-20, -22") ~ 1,
    eventTeamVenue == "home" & homeTeamDefendingSide == "right" &
        coordinates %in% c("20, 22", "69, 22", "20, -22", "69, -22") ~ 1,
    eventTeamVenue == "away" & homeTeamDefendingSide == "left" &
      coordinates %in% c("20, 22", "69, 22", "20, -22", "69, -22") ~ 1,
    eventTeamVenue == "away" & homeTeamDefendingSide == "right" &
      coordinates %in% c("-69, 22", "-20, 22", "-69, -22", "-20, -22") ~ 1,
    coordinates %in% c("0, 0") & eventTeamVenue == "away" ~ 1,
      TRUE ~ 0))
