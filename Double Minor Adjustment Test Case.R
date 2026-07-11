## Double Minor Adjustment Test Case
## July 10th 2026
## By Chloe Guagliano

library(nhlscraper)
library(tidyverse)

flier = gc_play_by_play(2025030415)

flierPPGs = flier |>
  filter(eventTypeDescKey == 'goal' & periodNumber %in% c('1','2','3')) |>
  filter(strengthState == 'power-play') |>
  select(gameId, eventId, eventTypeDescKey, secondsElapsedInGame, strengthState, eventOwnerTeamId)


flierPens = flier |>
  filter(eventTypeDescKey == 'penalty' & periodNumber %in% c('1','2','3')) |>
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


flier_adjustedPens = flierPens |>
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

flier_adjustedPPGs = flierPPGs |>
  rename(eventType = eventTypeDescKey,
         timestamp = secondsElapsedInGame) |>
  mutate(eventId = as.character(eventId)) |>
  select(eventType, timestamp, eventOwnerTeamId, eventId)

flier_adjustedEvents = full_join(flier_adjustedPens, flier_adjustedPPGs,
                           by = c('eventType','timestamp','eventOwnerTeamId','eventId')) |>
  arrange(timestamp)




segments <- list()
activePenalties <- list()
lastTime <- 0


for (i in seq_len(nrow(flier_adjustedEvents))) {
  
  controlTeam = 0
  opposingTeam = 0
  
  eventType = flier_adjustedEvents$eventType[i]
  timestamp = flier_adjustedEvents$timestamp[i]
  eventTeam = flier_adjustedEvents$eventOwnerTeamId[i]
  eventId = flier_adjustedEvents$eventId[i]
  
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
          penaltyDuration = flier_adjustedEvents$penaltyDuration[i]
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
            penaltyDuration = flier_adjustedEvents$penaltyDuration[i]
          )
        
        segments[[length(segments) + 1]] <-
          c(lastTime, timestamp, eventTeam)
        
        lastTime <- timestamp
        
      } else if (controlTeam > opposingTeam) {
        
        activePenalties[[length(activePenalties) + 1]] <-
          list(
            id = eventId,
            team = eventTeam,
            penaltyDuration = flier_adjustedEvents$penaltyDuration[i]
          )
        
      } else if (controlTeam < opposingTeam) {
        
        if (opposingTeam - controlTeam == 1) {
          
          activePenalties[[length(activePenalties) + 1]] <-
            list(
              id = eventId,
              team = eventTeam,
              penaltyDuration = flier_adjustedEvents$penaltyDuration[i]
            )
          
          segments[[length(segments) + 1]] <-
            c(lastTime, timestamp, eventTeam)
          
          lastTime <- timestamp
        } else {
          
          activePenalties[[length(activePenalties) + 1]] <-
            list(
              id = eventId,
              team = eventTeam,
              penaltyDuration = flier_adjustedEvents$penaltyDuration[i]
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





  
  
  
  
  