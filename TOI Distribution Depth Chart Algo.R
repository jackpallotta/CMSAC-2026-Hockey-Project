## TOI Distribution / Depth Chart Algo
## July 10th, 2026
## By Chloe Guagliano

library(tidyverse)
library(nhlscraper)

## all test case relative for now; let's get to an equivalent point to
## the existing Python code (gameId for complex debugging: 2025020007)

shifts = readRDS("shifts.rds")

ppGoals = pbp |>
  filter(gameId == '2025020007' & eventTypeDescKey == 'goal' & periodNumber %in% c('1','2','3')) |>
  filter(strengthState == 'power-play') |>
  select(gameId, eventId, eventTypeDescKey, secondsElapsedInGame, strengthState, eventOwnerTeamId)

penalties = pbp |>
  filter(gameId == '2025020007' & eventTypeDescKey == 'penalty' & periodNumber %in% c('1','2','3')) |>
  select(gameId, eventId, eventTypeDescKey, secondsElapsedInGame, penaltyDuration, eventOwnerTeamId) |>
  mutate(penaltyDuration = penaltyDuration*60) |>
  mutate(isDoubleMinor = if_else(penaltyDuration == 240,
                                 TRUE,
                                 FALSE)) |>
  uncount(if_else(isDoubleMinor, 2, 1)) |>
  group_by(eventId) |>
  mutate(
    # Second half of a double minor starts 120 seconds later
    secondsElapsedInGame = if_else(
      isDoubleMinor & row_number() == 2,
      secondsElapsedInGame + 120,
      secondsElapsedInGame
    ),
    
    # Both entries become 2-minute minors
    penaltyDuration = if_else(
      isDoubleMinor,
      120,
      penaltyDuration
    ),
    
    # Give the two penalties unique IDs
    eventId = if_else(
      isDoubleMinor,
      paste0(eventId, "_", row_number()),
      as.character(eventId)
    )
  ) |>
  ungroup() |>
  mutate(
    anticipatedPenaltyEnd = secondsElapsedInGame + penaltyDuration
  ) |>
  select(-isDoubleMinor)


## re-adjust so that penalty start and anticipated ends are recorded seperately
adjustedPens = penalties |>
  pivot_longer(
    cols = c(secondsElapsedInGame, anticipatedPenaltyEnd),
    names_to = "eventType",
    values_to = "timestamp"
  ) |>
  mutate(
    eventType = recode(
      eventType,
      secondsElapsedInGame = "penaltyStart",
      anticipatedPenaltyEnd = "anticipatedPenaltyEnd"
    )
  ) |>
  mutate(eventId = as.character(eventId)) |>
  select(eventType, timestamp, eventOwnerTeamId, eventId, penaltyDuration)

adjustedPPGs = ppGoals |>
  rename(eventType = eventTypeDescKey,
         timestamp = secondsElapsedInGame) |>
  mutate(eventId = as.character(eventId)) |>
  select(eventType, timestamp, eventOwnerTeamId, eventId)

adjustedEvents = full_join(adjustedPens, adjustedPPGs,
                           by = c('eventType','timestamp','eventOwnerTeamId','eventId')) |>
  arrange(timestamp)





##
segments <- list()
activePenalties <- list()
lastTime <- 0

for (i in seq_len(nrow(adjustedEvents))) {
  
  controlTeam = 0
  opposingTeam = 0
  
  eventType = adjustedEvents$eventType[i]
  timestamp = adjustedEvents$timestamp[i]
  eventTeam = adjustedEvents$eventOwnerTeamId[i]
  eventId = adjustedEvents$eventId[i]
  
  ####################
  # PENALTY START
  ####################
  
  if (eventType == "penaltyStart") {
    
    teamPenalized <- eventTeam
    
    if (lastTime == 0 || length(activePenalties) == 0) {
      
      activePenalties[[length(activePenalties) + 1]] <-
        list(
          id = eventId,
          team = eventTeam,
          penaltyDuration = adjustedEvents$penaltyDuration[i]
        )
      
      lastTime <- timestamp
      
    } else {
      
      for (j in seq_along(activePenalties)) {
        if (activePenalties[[j]]$team == teamPenalized) {
          controlTeam <- controlTeam + 1
        } else {
          opposingTeam <- opposingTeam + 1
        }
      }
      
      if (controlTeam == opposingTeam) {
        
        activePenalties[[length(activePenalties) + 1]] <-
          list(
            id = eventId,
            team = eventTeam,
            penaltyDuration = adjustedEvents$penaltyDuration[i]
          )
        
        segments[[length(segments) + 1]] <-
          c(lastTime, timestamp, eventTeam)
        
        lastTime <- timestamp
        
      } else if (controlTeam > opposingTeam) {
        
        activePenalties[[length(activePenalties) + 1]] <-
          list(
            id = eventId,
            team = eventTeam,
            penaltyDuration = adjustedEvents$penaltyDuration[i]
          )
        
      } else if (controlTeam < opposingTeam) {
        
        if (opposingTeam - controlTeam == 1) {
          
          activePenalties[[length(activePenalties) + 1]] <-
            list(
              id = eventId,
              team = eventTeam,
              penaltyDuration = adjustedEvents$penaltyDuration[i]
            )
          
          segments[[length(segments) + 1]] <-
            c(lastTime, timestamp, eventTeam)
          
          lastTime <- timestamp
        } else {
          
          activePenalties[[length(activePenalties) + 1]] <-
            list(
              id = eventId,
              team = eventTeam,
              penaltyDuration = adjustedEvents$penaltyDuration[i]
            )
        }
      }
    }
  }
  
  
  ####################
  # PENALTY END
  ####################
  
  if (eventType == "anticipatedPenaltyEnd") {
    
    teamPenalized <- eventTeam
    
    for (j in seq_along(activePenalties)) {
      if (activePenalties[[j]]$team == teamPenalized) {
        controlTeam <- controlTeam + 1
      } else {
        opposingTeam <- opposingTeam + 1
      }
    }
    
    
    if (controlTeam > opposingTeam &&
        controlTeam - opposingTeam == 1) {
      
      segments[[length(segments) + 1]] <-
        c(lastTime, timestamp, eventTeam)
    }
    
    
    lastTime <- timestamp
    
    penalty_id <- eventId
    
    for (j in seq_along(activePenalties)) {
      
      if (activePenalties[[j]]$id == penalty_id) {
        activePenalties[[j]] <- NULL
        break
      }
    }
  }
  
  
  ####################
  # POWER PLAY GOAL
  ####################
  
  if (eventType == "goal") {
    
    scoringTeam <- eventTeam
    
    for (j in seq_along(activePenalties)) {
      
      if (activePenalties[[j]]$team == scoringTeam) {
        controlTeam <- controlTeam + 1
      } else {
        opposingTeam <- opposingTeam + 1
      }
    }
    
    
    # Only one active penalty
    if (length(activePenalties) == 1) {
      
      penalty_id <- activePenalties[[1]]$id
      
      duration <- activePenalties[[1]]$penaltyDuration
      shorthandedTeam <- activePenalties[[1]]$team
      
      if (duration == 120) {
        
        activePenalties[[1]] <- NULL
        
        segments[[length(segments) + 1]] <-
          c(lastTime, timestamp, shorthandedTeam)
        
        lastTime <- timestamp
      }
    }
    
    
    # Multiple penalties, but only one extra penalty against scoring team
    else if (opposingTeam - controlTeam == 1) {
      
      for (j in seq_along(activePenalties)) {
        
        if (activePenalties[[j]]$team != scoringTeam) {
          
          penalty_id <- activePenalties[[j]]$id
          
          duration <- activePenalties[[j]]$penaltyDuration
          shorthandedTeam <- activePenalties[[j]]$team
          
          if (duration == 120) {
            
            activePenalties[[j]] <- NULL
            
            segments[[length(segments) + 1]] <-
              c(lastTime, timestamp, shorthandedTeam)
            
            lastTime <- timestamp
            
            break
          }
        }
      }
    }
    
    
    # Multiple penalties against scoring team
    else if (opposingTeam - controlTeam > 1) {
      
      for (j in seq_along(activePenalties)) {
        
        if (activePenalties[[j]]$team != scoringTeam) {
          
          penalty_id <- activePenalties[[j]]$id
          
          duration <- activePenalties[[j]]$penaltyDuration

          if (duration == 120) {
            
            activePenalties[[j]] <- NULL
            break
            
          }
        }
      }
    }
  }
}


segments_df <- do.call(rbind, segments) |>
  as.data.frame()

names(segments_df) <- c("startTime", "endTime", "shorthandedTeam")

segments_df







