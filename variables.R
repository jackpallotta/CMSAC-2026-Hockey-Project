rm(list=ls())
library(tidyverse)
library(nhlscraper)
pbp_cleaned <- readRDS("pbp_cleaned.rds")
schedule <- readRDS("schedule.rds")
rosters <- readRDS("rosters.rds")
players <- readRDS("players.rds")
careerGameNumbersCleaned <- readRDS("careerGameNumbersCleaned.rds")

pbp_cleaned_temp <- pbp_cleaned |>
  mutate(coordinates = paste(xCoord, yCoord, sep = ", ")) |>
  select(gameId, eventId, seasonId, periodNumber, periodType, secondsElapsedInGame, eventOwnerTeamId,
         eventTeamVenue, eventTypeDescKey, situationCode, isHome, homeIsEmptyNet, awayIsEmptyNet, 
         isEmptyNetFor, isEmptyNetAgainst, homeSkaterCount, awaySkaterCount, skaterCountFor,
         skaterCountAgainst, manDifferential, goalDifferential, strengthState, homeTeamDefendingSide,
         zoneCode, coordinates, winningPlayerId, losingPlayerId)

# faceoff-winning team's perspective
faceoffs_temp <- pbp_cleaned_temp |>
  filter(eventTypeDescKey == "faceoff") |>
  left_join(schedule |> select(gameId, gameDate, awayTeamId, homeTeamId, winningTeamId), by = "gameId") |>
  mutate(faceoffWon = 1L,
         wonGame = as.integer(eventOwnerTeamId == winningTeamId))

# mirror every faceoff from the losing team's perspective
mirrored_faceoffs <- faceoffs_temp |>
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
faceoffs <- bind_rows(faceoffs_temp, mirrored_faceoffs)

# cap the goal differential at +/- 3
faceoffs <- faceoffs |>
  arrange(gameId, eventId, periodNumber, secondsElapsedInGame) |>
  mutate(goalDifferential = case_when(
      goalDifferential <= -3 ~ -3L,
      goalDifferential >= 3 ~ 3L,
      TRUE ~ as.integer(goalDifferential)))

# create a variable for the faceoff player, each unique faceoff has two observations
faceoffs <- faceoffs |>
  mutate(faceoffPlayerId = case_when(
    faceoffWon == 1 ~ winningPlayerId,
    faceoffWon == 0 ~ losingPlayerId))

# join the players df to get the handedness of the two faceoff players
faceoffs <- faceoffs |>
  left_join(players |> select(season, playerId, player, shoots, birthDate),
            by = c("faceoffPlayerId" = "playerId", "seasonId" = "season"))

# create a variable for whether a player takes the faceoff on his strong side
faceoffs <- faceoffs |>
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
    TRUE ~ 0))

# create a variable for which player places his stick down first
faceoffs <- faceoffs |>
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
faceoffs <- faceoffs |>
  mutate(age = interval(birthDate, gameDate) %/% years(1)) |>
  select(-birthDate)

# add career game experience
faceoffs <- faceoffs |>
  left_join(careerGameNumbersCleaned |> select(playerId, gameId, seasonGameNumber, careerGameNumber),
            by = c("faceoffPlayerId" = "playerId", "gameId" = "gameId"))

# add faceoff number taken in game
faceoffs <- faceoffs |>
  group_by(gameId, faceoffPlayerId) |>
  arrange(secondsElapsedInGame, .by_group = TRUE) |>
  mutate(faceoffGameCount = row_number()) |>
  ungroup()

# add faceoff number taken in season
faceoffs <- faceoffs |>
  group_by(seasonId, faceoffPlayerId) |>
  arrange(gameDate, gameId, secondsElapsedInGame, .by_group = TRUE) |>
  mutate(seasonFaceoffCount = row_number()) |>
  ungroup()

# calculate time remaining in mins and secs for reg & OT
faceoffs <- faceoffs |>
  mutate(secondsRemainingReg = pmax(3600 - secondsElapsedInGame, 0),
    minutesRemainingReg = secondsRemainingReg / 60,
    secondsRemainingOT = if_else(periodNumber == 4,
      pmax(3900 - secondsElapsedInGame, 0), NA_real_),
    minutesRemainingOT = secondsRemainingOT / 60)

# coalesce reg and OT
faceoffs <- faceoffs |>
  mutate(secondsRemaining = coalesce(na_if(secondsRemainingReg, 0), secondsRemainingOT),
    minutesRemaining = coalesce(na_if(minutesRemainingReg, 0), minutesRemainingOT))

# create overtime binary variable
faceoffs <- faceoffs |>
  mutate(isOT = case_when(
    periodType == "REG" ~ 0,
    periodType == "OT" ~ 1))

# change empty net variables to binary
faceoffs <- faceoffs |>
  mutate(isEmptyNetFor = as.integer(isEmptyNetFor),
         isEmptyNetAgainst = as.integer(isEmptyNetAgainst))

# update man differential variables to include extra attackers for empty nets
faceoffs <- faceoffs |>
  mutate(manDifferential = skaterCountFor - skaterCountAgainst)

faceoffs <- faceoffs |>
  arrange(seasonId, gameDate, gameId, eventId)

# establish unblocked shot attempt events
unblocked_events <- c("goal", "shot-on-goal", "missed-shot")

# create faceoff windows for within 5 seconds
faceoff_windows <- faceoffs |>
  mutate(faceoffId = row_number()) |>
  select(faceoffId, gameId, periodNumber, faceoffTeamId = eventOwnerTeamId,
         faceoffTime = secondsElapsedInGame) |>
  mutate(faceoffEnd = faceoffTime + 5)

# inner join pbp events
events_after_faceoff <- faceoff_windows |>
  inner_join(pbp_cleaned |> filter(eventTypeDescKey %in% unblocked_events) |>
      select(gameId, periodNumber, secondsElapsedInGame, eventOwnerTeamId, eventTypeDescKey, xG),
    by = join_by(gameId, periodNumber, 
                 faceoffTime < secondsElapsedInGame, 
                 faceoffEnd >= secondsElapsedInGame)) |>
  mutate(isFor = eventOwnerTeamId == faceoffTeamId)

# summarise the USAT for/against within 5 seconds of each faceoff event
faceoff_summary <- events_after_faceoff |>
  group_by(faceoffId) |>
  summarise(USATFor5 = as.integer(any(isFor)),
    USATAgainst5 = as.integer(any(!isFor)),
    USATCountFor5 = sum(isFor),
    USATCountAgainst5 = sum(!isFor),
    xGFor5 = sum(if_else(isFor, xG, 0), na.rm = TRUE),
    xGAgainst5 = sum(if_else(!isFor, xG, 0), na.rm = TRUE),
    .groups = "drop")

# join the faceoff summary with other faceoff variables
faceoffs <- faceoffs |>
  mutate(faceoffId = row_number()) |>
  left_join(faceoff_summary, by = "faceoffId") |>
  mutate(across(c(USATFor5,
                  USATAgainst5,
                  USATCountFor5,
                  USATCountAgainst5,
                  xGFor5,
                  xGAgainst5), ~ replace_na(.x, 0))) |>
  select(-faceoffId)

# create faceoff dot category variables
faceoffs <- faceoffs |>
  mutate(faceoffDotCategory = recode(
    coordinates, "-69, 22" = "1",
    "-20, 22" = "2",
    "20, 22" = "3",
    "69, 22" = "4",
    "-69, -22" = "5",
    "-20, -22" = "6",
    "20, -22" = "7",
    "69, -22" = "8",
    "0, 0" = "9",
    .default = NA_character_))

# create faceoff situation from faceoff player's perspective
faceoffs <- faceoffs |>
  mutate(teamDefendingSide = case_when(
      eventTeamVenue == "home" ~ homeTeamDefendingSide,
      eventTeamVenue == "away" & homeTeamDefendingSide == "left"  ~ "right",
      eventTeamVenue == "away" & homeTeamDefendingSide == "right" ~ "left",
      TRUE ~ NA_character_),
    
    # determine whether dot is on the left or right
    faceoffSide = case_when(
      coordinates == "0, 0" ~ "C",
      
      teamDefendingSide == "right" &
        coordinates %in% c("-69, 22", "-20, 22", "20, 22", "69, 22") ~ "R",
      
      teamDefendingSide == "right" &
        coordinates %in% c("-69, -22", "-20, -22", "20, -22", "69, -22") ~ "L",
      
      teamDefendingSide == "left" &
        coordinates %in% c("-69, 22", "-20, 22", "20, 22", "69, 22") ~ "L",
      
      teamDefendingSide == "left" &
        coordinates %in% c("-69, -22", "-20, -22", "20, -22", "69, -22") ~ "R",
      
      TRUE ~ NA_character_
    ),
    
    # classify faceoff situation
    faceoffSituation = case_when(
      coordinates == "0, 0" ~ "N-C",
      
      # OZ faceoffs
      zoneCode == "O" & faceoffSide == "R" ~ "O-R",
      zoneCode == "O" & faceoffSide == "L" ~ "O-L",
      
      # DZ faceoffs
      zoneCode == "D" & faceoffSide == "R" ~ "D-R",
      zoneCode == "D" & faceoffSide == "L" ~ "D-L",
      
      # NZ defensive side
      zoneCode == "N" & teamDefendingSide == "right" &
        coordinates %in% c("20, 22", "20, -22") ~ paste0("N-D-", faceoffSide),
      
      zoneCode == "N" & teamDefendingSide == "left" &
        coordinates %in% c("-20, 22", "-20, -22") ~ paste0("N-D-", faceoffSide),
      
      # NZ offensive side
      zoneCode == "N" & teamDefendingSide == "right" &
        coordinates %in% c("-20, 22", "-20, -22") ~ paste0("N-O-", faceoffSide),
      
      zoneCode == "N" & teamDefendingSide == "left" &
        coordinates %in% c("20, 22", "20, -22") ~ paste0("N-O-", faceoffSide),
      
      TRUE ~ NA_character_))

# clean the dataset and select desired varaibles for modeling
faceoffsCleaned <- faceoffs |>
  select(seasonId, gameId, gameDate, eventId, periodNumber,
         secondsRemaining, minutesRemaining, isOT, eventTypeDescKey, eventTeamVenue, teamDefendingSide,
         strengthState, isEmptyNetFor, isEmptyNetAgainst, skaterCountFor, skaterCountAgainst,
         manDifferential, goalDifferential, coordinates, faceoffDotCategory, 
         faceoffSituation, faceoffPlayerId, player, shoots, strongSide, stickDownFirst, age, 
         faceoffGameCount, seasonFaceoffCount, seasonGameNumber, careerGameNumber, faceoffWon,
         USATFor5, USATAgainst5, USATCountFor5, USATCountAgainst5, xGFor5, xGAgainst5,
         wonGame)

test_game <- faceoffsCleaned |>
  filter(gameId == 2025021312) |>
  arrange(eventId)

saveRDS(faceoffsCleaned, "faceoffsCleaned.rds")
