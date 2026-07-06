rm(list=ls())
library(tidyverse)
library(glue)
library(httr)
library(jsonlite)
library(purrr)

seasons <- c(20212022, 20222023, 20232024, 20242025, 20252026)

get_skater_bios_api <- function(season_id) {
  "https://api.nhle.com/stats/rest/en/skater/bios?limit=-1&start=0&cayenneExp=seasonId={season_id}" |> 
    glue() |> 
    GET() |> 
    content(type = "text", encoding = "UTF-8") |> 
    fromJSON(flatten = TRUE) -> response
  
  response$data |>
    as_tibble() |>
    select(playerId = "playerId", player = "skaterFullName", position = "positionCode",
           shoots = "shootsCatches", height = "height", weight = "weight",
           birthDate = "birthDate", country = "nationalityCode", birthCity = "birthCity",
           birthStateProvince = "birthStateProvinceCode", draftYear = "draftYear",
           draftRound = "draftRound", draftOverall = "draftOverall", 
           NHLdebut = "firstSeasonForGameType") |>
    distinct(playerId, .keep_all = TRUE) |>
    mutate(player = case_when(
      player == "Elias Pettersson" & position == "D" ~ "Elias Pettersson (D)",
      player == "Sebastian Aho" & position == "D" ~ "Sebastian Aho (D)",
      TRUE ~ player),
      season = season_id)
}

skaters <- seasons |>
  map_dfr(~ get_skater_bios_api(.x))

get_goalie_bios_api <- function(season_id) {
  "https://api.nhle.com/stats/rest/en/goalie/bios?limit=-1&start=0&cayenneExp=seasonId={season_id}" |>
    glue() |> 
    GET() |>
    content(type = "text", encoding = "UTF-8") |>
    fromJSON(flatten = TRUE) -> response
  
  response$data |>
    as_tibble() |>
    mutate(position = "G") |>
    select(playerId = "playerId", player = "goalieFullName", position,
           shoots = "shootsCatches", height = "height", weight = "weight",
           birthDate = "birthDate", country = "nationalityCode", birthCity = "birthCity",
           birthStateProvince = "birthStateProvinceCode", draftYear = "draftYear",
           draftRound = "draftRound", draftOverall = "draftOverall", 
           NHLdebut = "firstSeasonForGameType") |>
    distinct(playerId, .keep_all = TRUE) |>
    mutate(season = season_id)
}

goalies <- seasons |>
  map_dfr(~ get_goalie_bios_api(.x))

players <- bind_rows(skaters, goalies)

rm(skaters, goalies)

players <- players |>
  mutate(playerId = as.character(playerId))

saveRDS(players, "players.rds")
