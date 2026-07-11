## Shift Chart Info for Full Dataset

library(tidyverse)
library(nhlscraper)


schedule = readRDS("schedule.rds")

gameIDs = schedule |>
  pull(gameId)

shifts = gameIDs |>
  map_dfr(\(id) {
    message("Extracting game ", id, " shifts")
    
    shift_chart(game = id) |>
      mutate(gameId = id, .before = 1)
  })

saveRDS(shifts, "shifts.rds")
shifts = readRDS("shifts.rds")
rm(list=ls())
