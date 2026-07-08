rm(list=ls())
library(tidyverse)
library(nhlscraper)
pbp_cleaned <- readRDS("pbp_cleaned.rds")
schedule <- readRDS("schedule.rds")
rosters <- readRDS("rosters.rds")
players <- readRDS("players.rds")
careerGameNumbersCleaned <- readRDS("careerGameNumbersCleaned.rds")

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
  left_join(schedule |> select(gameId, gameDate, awayTeamId, homeTeamId, winningTeamId), by = "gameId") |>
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
  left_join(players |> select(season, playerId, player, shoots, birthDate),
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

# create a variable for the player age
combined_faceoffs <- combined_faceoffs |>
  mutate(age = interval(birthDate, gameDate) %/% years(1)) |>
  select(-birthDate)

# add career game experience
combined_faceoffs <- combined_faceoffs |>
  left_join(careerGameNumbersCleaned |> select(playerId, gameId, seasonGameNumber, careerGameNumber),
            by = c("faceoffPlayerId" = "playerId", "gameId" = "gameId"))

# add faceoff number taken in game
combined_faceoffs <- combined_faceoffs |>
  group_by(gameId, faceoffPlayerId) |>
  arrange(secondsElapsedInGame, .by_group = TRUE) |>
  mutate(faceoffGameCount = row_number()) |>
  ungroup()

# add faceoff number taken in season
combined_faceoffs <- combined_faceoffs |>
  group_by(seasonId, faceoffPlayerId) |>
  arrange(gameDate, gameId, secondsElapsedInGame, .by_group = TRUE) |>
  mutate(seasonFaceoffCount = row_number()) |>
  ungroup()

# calculate time remaining in mins and secs for reg & OT
combined_faceoffs <- combined_faceoffs |>
  mutate(secondsRemainingReg = pmax(3600 - secondsElapsedInGame, 0),
    minutesRemainingReg = secondsRemainingReg / 60,
    secondsRemainingOT = if_else(periodNumber == 4,
      pmax(3900 - secondsElapsedInGame, 0), NA_real_),
    minutesRemainingOT = secondsRemainingOT / 60)

combined_faceoffs <- combined_faceoffs |>
  arrange(seasonId, gameDate, gameId, eventId)

faceoffsCleaned <- combined_faceoffs |>
  select(seasonId, gameId, gameDate, eventId, periodNumber, periodType,
         minutesRemainingReg, secondsRemainingReg, minutesRemainingOT,
         secondsRemainingOT, eventTypeDescKey, eventTeamVenue, homeTeamDefendingSide,
         strengthState, isEmptyNetFor, isEmptyNetAgainst, skaterCountFor, skaterCountAgainst,
         manDifferential, goalDifferential, zoneCode, coordinates, faceoffPlayerId,
         player, shoots, strongSide, stickDownFirst, age, faceoffGameCount,
         seasonFaceoffCount, seasonGameNumber, careerGameNumber, faceoffWon, wonGame)

test_game <- combined_faceoffs |>
  filter(gameId == 2025021312) |>
  arrange(eventId)

#Play by play Official----
#pbp pulled from Jack
#pulls the first five seconds after a face-off
pbp_faceoffs = pbp_cleaned |>
  mutate(row_id = row_number()) |>
  filter(eventTypeDescKey == "faceoff") |>
  select(faceoff_row = row_id,
         gameId,                 
         periodNumber,                 
         faceoff_time = secondsElapsedInGame) |>
  mutate(faceoff_end = faceoff_time + 5)

events_after_faceoff2 = pbp_faceoffs |>
  inner_join(
    pbp,
    by = join_by(
      gameId == gameId,
      periodNumber == periodNumber,
      faceoff_time < secondsElapsedInGame,   
      faceoff_end >= secondsElapsedInGame)) |>
  mutate(fo_success = as.factor(ifelse(eventTypeDescKey == "shot-on-goal" | eventTypeDescKey =="goal", 1, 0))) |>
  mutate(is_shot_atmpt = as.numeric(eventTypeDescKey == "shot-on-goal" | eventTypeDescKey == "missed-shot" | eventTypeDescKey == "goal")) |>
  mutate(faceoffDotCategory = case_when(xCoord == -69 & yCoord == 22 ~ '1',
                                        xCoord == -20 & yCoord == 22 ~ '2',
                                        xCoord == 20 & yCoord == 22 ~ '3',
                                        xCoord == 69 & yCoord == 22 ~ '4',
                                        xCoord == -69 & yCoord == -22 ~ '5',
                                        xCoord == -20 & yCoord == -22 ~ '6',
                                        xCoord == 20 & yCoord == -22 ~ '7',
                                        xCoord == 69 & yCoord == -22 ~ '8',
                                        xCoord == 0 & yCoord == 0 ~ '0')) |>
  mutate(leftRight = as.factor(case_when(homeTeamDefendingSide == 'left' & eventTeamVenue == 'home'~
                                           if_else(faceoffDotCategory %in% c('1','2','3','4'),'L','R'),
                                         homeTeamDefendingSide == 'left' & eventTeamVenue == 'away'~
                                           if_else(faceoffDotCategory %in% c('5','6','7','8'),'L','R'),
                                         homeTeamDefendingSide == 'right' & eventTeamVenue == 'home'~
                                           if_else(faceoffDotCategory %in% c('5','6','7','8'),'L','R'),
                                         homeTeamDefendingSide == 'right' & eventTeamVenue == 'away'~
                                           if_else(faceoffDotCategory %in% c('1','2','3','4'),'L','R')))) |>
  mutate(zoneCode = as.factor(if_else(zoneCode == 'N' & faceoffDotCategory != 'C',
                                      case_when(homeTeamDefendingSide == 'left' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('2','6') ~ 
                                                  'C-NZ',
                                                homeTeamDefendingSide == 'left' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('3','7') ~
                                                  'F-NZ',
                                                homeTeamDefendingSide == 'left' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('2','6') ~
                                                  'F-NZ',
                                                homeTeamDefendingSide == 'left' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('3','7') ~
                                                  'C-NZ',
                                                homeTeamDefendingSide == 'right' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('2','6') ~
                                                  'F-NZ',
                                                homeTeamDefendingSide == 'right' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('3','7') ~
                                                  'C-NZ',
                                                homeTeamDefendingSide == 'right' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('2','6') ~
                                                  'C-NZ',
                                                homeTeamDefendingSide == 'right' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('3','7') ~
                                                  'F-NZ'),zoneCode))) |>
  mutate(zoneCode = if_else(zoneCode == 'N','C', zoneCode))|>
  mutate(situationDescriptor = as.factor(case_when(is.na(zoneCode) | is.na(leftRight) ~ NA_character_,
                                                   TRUE ~ paste(zoneCode, leftRight, sep = ' ')))) |>
  mutate(situationLinkage = as.factor(case_when(situationDescriptor %in% c('D L', 'O R') ~
                                                  'A',
                                                situationDescriptor %in% c('D R', 'O L') ~
                                                  'B',
                                                situationDescriptor %in% c('F-NZ R', 'C-NZ L') ~
                                                  'C',
                                                situationDescriptor %in% c('F-NZ L', 'C-NZ R') ~
                                                  'D',
                                                situationDescriptor == 'C R' ~
                                                  'E'))) |>
  mutate(periodNumber = as.factor(periodNumber))

