rm(list=ls())
library(tidyverse)
library(rvest)
library(purrr)
library(glue)
library(jsonlite)
library(httr)
library(nhlscraper)
schedule <- readRDS("schedule.rds")

gameIDs <- schedule |>
  pull(gameId)

rosters <- gameIDs |>
  map_dfr(\(id) {
    message("Extracting game ", id, " rosters")
    
    game_rosters(game = id) |>
      mutate(gameId = id, .before = 1)
  })


rosters <- rosters |>
  mutate(player = paste(playerFirstName, playerLastName, sep = " ")) |>
  select(gameId, teamId, playerId, player, sweaterNumber, positionCode)

saveRDS(rosters, "rosters.rds")
