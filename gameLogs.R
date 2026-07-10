rm(list=ls())
library(tidyverse)
library(purrr)
library(nhlscraper)
players <- readRDS("players.rds")

seasons = c(20032004, 20052006, 20062007, 20072008, 20082009, 20092010, 20102011, 
            20112012, 20122013, 20132014, 20142015, 20152016, 20162017, 20172018, 
            20182019, 20192020, 20202021, 20212022, 20222023, 20232024, 20242025, 
            20252026)

playerIDs <- players |>
  filter(position != "G") |>
  distinct(playerId) |>
  pull(playerId)

safePlayerGameLog <- possibly(
  .f = player_game_log,
  otherwise = tibble())

gameLogs <- playerIDs |>
  map_dfr(\(player_id) {
    seasons |>
      map_dfr(\(season) {
        message("Extracting player ", player_id, " season ", season)
        
        safePlayerGameLog(
          player = player_id,
          season = season,
          game_type = 2
        ) |>
          mutate(
            playerId = player_id,
            season = season,
            .before = 1
          )
      })
  })

# store temp tables
gameLogs_2 <- gameLogs
gameLogs_3 <- gameLogs

# game logs pre sample
gameLogsPreSample <- gameLogs_2 |>
  filter(season <= 20202021) |>
  group_by(playerId) |>
  mutate(gp = n()) |>
  distinct(playerId, .keep_all = TRUE) |>
  select(playerId, gp)

players_join <- players |>
  distinct(playerId, player)

gameLogsPreSample <- gameLogsPreSample |>
  left_join(players_join |> select(playerId, player), by = c("playerId"), relationship = "many-to-one")

# game logs post sample
gameLogsPostSample <- gameLogs_3 |>
  filter(season >= 20212022) |>
  select(playerId, gameId, season, gameDate)

players_join <- players |>
  distinct(playerId, player)

gameLogsPostSample <- gameLogsPostSample |>
  left_join(players_join |> select(playerId, player), by = c("playerId"), relationship = "many-to-one")

# get rolling career gp
careerGameNumbers <- gameLogsPostSample |>
  left_join(gameLogsPreSample |> select(playerId, preSampleGP = gp), by = "playerId") |>
  mutate(preSampleGP = replace_na(preSampleGP, 0)) |>
  group_by(playerId) |>
  arrange(gameDate, gameId, .by_group = TRUE) |>
  mutate(postSampleGameNumber = row_number(),
         careerGameNumber = preSampleGP + postSampleGameNumber) |>
  ungroup()

# get rolling season gp
careerGameNumbers <- careerGameNumbers |>
  group_by(season, playerId) |>
  arrange(gameDate, gameId, .by_group = TRUE) |>
  mutate(seasonGameNumber = row_number()) |>
  ungroup()

# select desired variables
careerGameNumbersCleaned <- careerGameNumbers |>
  select(playerId, player, gameId, seasonGameNumber, careerGameNumber)

# save RDS file
saveRDS(careerGameNumbersCleaned, "careerGameNumbersCleaned.rds")
