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
  select(gameId, eventId, seasonId, periodNumber, periodType, secondsElapsedInGame, secondsElapsedInPeriod,
         eventOwnerTeamId, eventTeamVenue, eventTypeDescKey, situationCode, isHome, homeIsEmptyNet, awayIsEmptyNet, 
         isEmptyNetFor, isEmptyNetAgainst, homeSkaterCount, awaySkaterCount, skaterCountFor,
         skaterCountAgainst, manDifferential, goalDifferential, strengthState, homeTeamDefendingSide,
         zoneCode, coordinates, winningPlayerId, losingPlayerId, skater1PlayerIdFor, skater1PlayerIdAgainst,
         skater2PlayerIdFor, skater2PlayerIdAgainst, skater3PlayerIdFor, skater3PlayerIdAgainst,
         skater4PlayerIdFor, skater4PlayerIdAgainst, skater5PlayerIdFor, skater5PlayerIdAgainst,
         skater6PlayerIdFor, skater6PlayerIdAgainst)

# faceoff-winning team's perspective
faceoffs_temp <- pbp_cleaned_temp |>
  filter(eventTypeDescKey == "faceoff") |>
  left_join(schedule |> select(gameId, gameDate, awayTeamId, homeTeamId, winningTeamId), by = "gameId") |>
  mutate(faceoffWon = 1L,
         wonGame = as.integer(eventOwnerTeamId == winningTeamId))

# create the faceoff teammate variables
faceoffs_temp <- faceoffs_temp |>
  mutate(playerOnIce1 = case_when(faceoffWon == 1 ~ skater1PlayerIdFor, TRUE ~ skater1PlayerIdAgainst),
         playerOnIce2 = case_when(faceoffWon == 1 ~ skater2PlayerIdFor, TRUE ~ skater2PlayerIdAgainst),
         playerOnIce3 = case_when(faceoffWon == 1 ~ skater3PlayerIdFor, TRUE ~ skater3PlayerIdAgainst),
         playerOnIce4 = case_when(faceoffWon == 1 ~ skater4PlayerIdFor, TRUE ~ skater4PlayerIdAgainst),
         playerOnIce5 = case_when(faceoffWon == 1 ~ skater5PlayerIdFor, TRUE ~ skater5PlayerIdAgainst),
         playerOnIce6 = case_when(faceoffWon == 1 ~ skater6PlayerIdFor, TRUE ~ skater6PlayerIdAgainst))

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
      temp2 = skaterCountFor,
      skaterCountFor = skaterCountAgainst,
      skaterCountAgainst = temp2,
    zoneCode = case_when(
      zoneCode == "O" ~ "D",
      zoneCode == "D" ~ "O",
      TRUE ~ zoneCode),
    manDifferential = -manDifferential,
    temp = isEmptyNetFor,
    isEmptyNetFor = isEmptyNetAgainst,
    isEmptyNetAgainst = temp) |>
  select(-temp, -temp2)

# get the players on ice for the faceoff losing team
mirrored_faceoffs <- mirrored_faceoffs |>
  mutate(playerOnIce1 = case_when(faceoffWon == 0 ~ skater1PlayerIdAgainst, TRUE ~ NA_integer_),
         playerOnIce2 = case_when(faceoffWon == 0 ~ skater2PlayerIdAgainst, TRUE ~ NA_integer_),
         playerOnIce3 = case_when(faceoffWon == 0 ~ skater3PlayerIdAgainst, TRUE ~ NA_integer_),
         playerOnIce4 = case_when(faceoffWon == 0 ~ skater4PlayerIdAgainst, TRUE ~ NA_integer_),
         playerOnIce5 = case_when(faceoffWon == 0 ~ skater5PlayerIdAgainst, TRUE ~ NA_integer_),
         playerOnIce6 = case_when(faceoffWon == 0 ~ skater6PlayerIdAgainst, TRUE ~ NA_integer_))

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
  left_join(players |> select(season, playerId, player, shoots, height, weight, birthDate),
            by = c("faceoffPlayerId" = "playerId", "seasonId" = "season"))

# create the strengthState variable
faceoffs <- faceoffs |>
  mutate(situationCode = paste0(skaterCountFor, "v", skaterCountAgainst))

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

# calculate time remaining for reg and OT
faceoffs <- faceoffs |>
  mutate(secondsRemainingReg = if_else(
    periodNumber %in% 1:3,
    pmax(3600 - secondsElapsedInGame, 0),
    NA_real_),
    minutesRemainingReg = secondsRemainingReg / 60,
    secondsRemainingOT = if_else(
      periodNumber == 4,
      pmax(3900 - secondsElapsedInGame, 0),
      NA_real_
    ), minutesRemainingOT = secondsRemainingOT / 60)

# calculate time remaining in current period and combine reg & OT clock
faceoffs <- faceoffs |>
  mutate(periodLengthSeconds = case_when(
      periodNumber %in% 1:3 ~ 1200,
      periodNumber == 4 ~ 300,
      TRUE ~ NA_real_),
    secondsRemainingPeriod = pmax(periodLengthSeconds - secondsElapsedInPeriod, 0),
    minutesRemainingPeriod = secondsRemainingPeriod / 60,
    secondsRemaining = coalesce(secondsRemainingReg, secondsRemainingOT),
    minutesRemaining = coalesce(minutesRemainingReg, minutesRemainingOT))

# define shot-event groups
USAT_events <- c("goal", "shot-on-goal", "missed-shot")
SAT_events <- c(USAT_events, "blocked-shot")

# define stoppages that truncate SAT opportunity window
censoring_reasons <- c("icing", "offside", "puck-frozen", "puck-in-netting",
  "puck-in-crowd", "puck-in-benches", "hand-pass", "high-stick",
  "player-injury", "official-injury", "net-dislodged-offensive-skater",
  "net-dislodged-defensive-skater", "net-dislodged-by-goaltender")

# create one row per faceoff for first stoppage
initial_faceoff_windows <- faceoffs |>
  distinct(gameId, eventId, .keep_all = TRUE) |>
  transmute(gameId, faceoffEventId = eventId, periodNumber, faceoffTime = secondsElapsedInGame,
    preliminaryFaceoffEnd = faceoffTime + pmin(5, secondsRemainingPeriod))

# find first SAT after each faceoff
first_SAT_after_faceoff <- initial_faceoff_windows |>
  inner_join(pbp_cleaned |> filter(eventTypeDescKey %in% SAT_events) |>
      transmute(gameId, periodNumber, satEventId = eventId, satTime = secondsElapsedInGame,
                satEventType = eventTypeDescKey),
      by = join_by(gameId, periodNumber, faceoffTime < satTime, preliminaryFaceoffEnd >= satTime),
      relationship = "many-to-many") |>
  arrange(gameId, faceoffEventId, satTime, satEventId) |>
  group_by(gameId, faceoffEventId) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(secondsUntilFirstSAT = satTime - faceoffTime) |>
  select(gameId, faceoffEventId, firstSATTime = satTime, secondsUntilFirstSAT,
         firstSATEventType = satEventType)

# find first qualifying stoppage
first_stoppage_after_faceoff <- initial_faceoff_windows |>
  inner_join(pbp_cleaned |> filter(eventTypeDescKey == "stoppage", reason %in% censoring_reasons) |>
               transmute(gameId, periodNumber, stoppageEventId = eventId, stoppageTime = secondsElapsedInGame,
                         stoppageReason = reason),
             by = join_by(gameId, periodNumber, faceoffTime < stoppageTime, preliminaryFaceoffEnd >= stoppageTime),
             relationship = "many-to-many") |>
  arrange(gameId, faceoffEventId, stoppageTime, stoppageEventId) |>
  group_by(gameId, faceoffEventId) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(secondsUntilStoppage = stoppageTime - faceoffTime) |>
  select(gameId, faceoffEventId, firstStoppageTime = stoppageTime, secondsUntilStoppage, stoppageReason)
                
# join stoppage info back
faceoffs <- faceoffs |>
  left_join(first_SAT_after_faceoff, by = c("gameId", "eventId" = "faceoffEventId")) |>
  left_join(first_stoppage_after_faceoff, by = c("gameId", "eventId" = "faceoffEventId")) |>
  mutate(stoppageWindowLimit = coalesce(secondsUntilStoppage, 5),
         availableSATWindow = pmin(5, secondsRemainingPeriod, stoppageWindowLimit),
         fullFiveSecondWindow = availableSATWindow >= 5,
         SATWindowOutcome = case_when(
           !is.na(secondsUntilFirstSAT) & (is.na(secondsUntilStoppage) | 
                                             secondsUntilFirstSAT <= secondsUntilStoppage) ~ "SAT before stoppage",
           !is.na(secondsUntilStoppage) & (is.na(secondsUntilFirstSAT) | 
                                             secondsUntilStoppage < secondsUntilFirstSAT) ~ "Stoppage before SAT",
           secondsRemainingPeriod < 5 & is.na(secondsUntilFirstSAT) ~ "Period ended before SAT",
           TRUE ~ "Full window without SAT"),
         SATWindowCensored = SATWindowOutcome %in% c("Stoppage before SAT", "Period ended before SAT"))

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

# create faceoff windows for within 5 seconds
# create a stable ID for each faceoff
faceoffs <- faceoffs |>
  mutate(faceoffId = row_number())

# create faceoff windows for within 5 seconds
faceoff_windows <- faceoffs |>
  transmute(faceoffId, gameId, eventId, periodNumber, faceoffTeamId = eventOwnerTeamId,
    faceoffTime = secondsElapsedInGame, faceoffEnd = faceoffTime + availableSATWindow,
    homeTeamId, awayTeamId)

# inner join pbp events
events_after_faceoff <- faceoff_windows |>
  inner_join(pbp_cleaned |> filter(eventTypeDescKey %in% SAT_events) |>
      select(gameId, periodNumber, secondsElapsedInGame, eventOwnerTeamId, eventTypeDescKey, xG),
    by = join_by(gameId, periodNumber, faceoffTime < secondsElapsedInGame, faceoffEnd >= secondsElapsedInGame),
    relationship = "many-to-many") |>
  mutate(shotAttemptTeamId = eventOwnerTeamId,
    isFor = shotAttemptTeamId == faceoffTeamId,
    isUnblocked = eventTypeDescKey %in% USAT_events,
    isBlocked = eventTypeDescKey == "blocked-shot",
    isGoal = eventTypeDescKey == "goal")

# summarize the SAT for/against within 5 seconds of each faceoff event
faceoff_summary <- events_after_faceoff |>
  group_by(faceoffId) |>
  summarise(
    SATFor5 = as.integer(any(isFor)),
    SATAgainst5 = as.integer(any(!isFor)),
    SATCountFor5 = sum(isFor),
    SATCountAgainst5 = sum(!isFor),
    
    USATFor5 = as.integer(any(isFor & isUnblocked)),
    USATAgainst5 = as.integer(any(!isFor & isUnblocked)),
    USATCountFor5 = sum(isFor & isUnblocked),
    USATCountAgainst5 = sum(!isFor & isUnblocked),
    
    blockedShotFor5 = as.integer(any(isFor & isBlocked)),
    blockedShotAgainst5 = as.integer(any(!isFor & isBlocked)),
    blockedShotCountFor5 = sum(isFor & isBlocked),
    blockedShotCountAgainst5 = sum(!isFor & isBlocked),
    
    goalFor5 = as.integer(any(isFor & isGoal)),
    goalAgainst5 = as.integer(any(!isFor & isGoal)),
    
    xGFor5 = sum(
      if_else(isFor & isUnblocked, xG, 0),
      na.rm = TRUE
    ),
    
    xGAgainst5 = sum(
      if_else(!isFor & isUnblocked, xG, 0),
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# join the faceoff summary with other faceoff variables
faceoffs <- faceoffs |>
  left_join(
    faceoff_summary,
    by = "faceoffId"
  ) |>
  mutate(
    across(
      c(
        SATFor5,
        SATAgainst5,
        SATCountFor5,
        SATCountAgainst5,
        USATFor5,
        USATAgainst5,
        USATCountFor5,
        USATCountAgainst5,
        blockedShotFor5,
        blockedShotAgainst5,
        blockedShotCountFor5,
        blockedShotCountAgainst5,
        xGFor5,
        xGAgainst5,
        goalFor5,
        goalAgainst5
      ),
      ~ replace_na(.x, 0)
    )
  ) |>
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
  select(seasonId, gameId, gameDate, eventId, periodNumber, secondsRemaining, minutesRemaining,
    secondsRemainingPeriod, minutesRemainingPeriod, isOT, eventTypeDescKey, eventTeamVenue, 
    teamDefendingSide, strengthState, situationCode, isEmptyNetFor, isEmptyNetAgainst, skaterCountFor, skaterCountAgainst,
    manDifferential, goalDifferential, coordinates, zoneCode, faceoffDotCategory, faceoffSituation,
    faceoffPlayerId, player, shoots, strongSide, stickDownFirst, age, height, weight, faceoffGameCount,
    seasonFaceoffCount, seasonGameNumber, careerGameNumber, playerOnIce1, playerOnIce2, 
    playerOnIce3, playerOnIce4, playerOnIce5, playerOnIce6, faceoffWon, fullFiveSecondWindow,
    availableSATWindow, secondsUntilStoppage, stoppageReason, SATWindowOutcome, SATWindowCensored,
    SATFor5, SATAgainst5, SATCountFor5, SATCountAgainst5, USATFor5, USATAgainst5, USATCountFor5,
    USATCountAgainst5, blockedShotFor5, blockedShotAgainst5, blockedShotCountFor5, blockedShotCountAgainst5,
    xGFor5, xGAgainst5, goalFor5, goalAgainst5, wonGame)

saveRDS(faceoffsCleaned, "faceoffsCleaned.rds")
