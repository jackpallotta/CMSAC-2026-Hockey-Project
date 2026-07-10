rm(list=ls())
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
library(purrr)

seasons <- c(20212022, 20222023, 20232024, 20242025, 20252026)

get_schedule_api <- function(team_id, season_id) {
  "https://api-web.nhle.com/v1/club-schedule-season/{team_id}/{season_id}" |>
    glue() |>
    GET() |>
    content(type = "text", encoding = "UTF-8") |>
    fromJSON(flatten = TRUE) -> response
  
  response$games
  
}

nhl_teams <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", 
               "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NJD", "NSH", "NYI", 
               "NYR", "OTT", "PHI", "PIT", "SEA", "SJS", "STL", "TBL", "TOR", 
               "UTA", "VAN", "VGK", "WSH", "WPG")

schedule <- seasons |>
  map_dfr(\(season) {
    nhl_teams |>
      map_dfr(\(team) {
        get_schedule_api(team, season)
      })
  }) |>
  filter(gameType == 2) |>
  rename(gameId = id,
    awayTeamCode = awayTeam.abbrev, awayTeamId = awayTeam.id, awayScore = awayTeam.score,
    homeTeamCode = homeTeam.abbrev, homeTeamId = homeTeam.id, homeScore = homeTeam.score) |>
  distinct(gameId, .keep_all = TRUE) |>
  arrange(gameId) |>
  mutate(winningTeamId = case_when(
      homeScore > awayScore ~ homeTeamId,
      awayScore > homeScore ~ awayTeamId,
      TRUE ~ NA_integer_)) |>
  select(gameId, season, gameType, gameDate, awayTeamCode, awayTeamId, awayScore,
    homeTeamCode, homeTeamId, homeScore, winningTeamId)

saveRDS(schedule, "schedule.rds")
