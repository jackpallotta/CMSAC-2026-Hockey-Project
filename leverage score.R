rm(list = ls())
library(tidyverse)
library(splines)
library(mgcv)
library(broom)
library(gratia)

theme_set(theme_light())

faceoffsCleaned <- readRDS("faceoffsCleaned.rds")

goal_diff_levels <- c(-3, -2, -1, 0, 1, 2, 3)

# prepare the data
leverageVariables <- faceoffsCleaned |>
  select(gameId, eventId, faceoffPlayerId, player, secondsRemaining, secondsRemainingPeriod,
         situationCode, goalDifferential, zoneCode, isOT, faceoffSituation, faceoffWon, wonGame,
         fullFiveSecondWindow, availableSATWindow, secondsUntilStoppage, stoppageReason,
         SATWindowOutcome, xGFor5, xGAgainst5, goalFor5, goalAgainst5) |>
  mutate(wonGame = as.integer(wonGame),
         situationCode = case_when(situationCode %in% c("6v3", "6v4") ~ "EN +2",
                                   situationCode %in% c("3v6", "4v6") ~ "EN -2",
                                   TRUE ~ situationCode),
         situationCode = factor(situationCode),
         goalDifferential = factor(goalDifferential, levels = goal_diff_levels),
         zoneCode = factor(zoneCode, levels = c("D", "N", "O")),
         isOT = factor(isOT, levels = c(0, 1), labels = c("Regulation", "Overtime")),
         fullFiveSecondWindow = factor(fullFiveSecondWindow, levels = c(0, 1), labels = c("No", "Yes")),
         logAvailableWindow = log1p(availableSATWindow))

# win probability model
wp_gam <- bam(wonGame ~ goalDifferential + s(secondsRemaining, by = goalDifferential, k = 8) +
                zoneCode + situationCode + isOT,
              data = leverageVariables, family = binomial(), method = "fREML", discrete = TRUE)

# create a copy of every faceoff scenario for a goal scored
goalForData <- leverageVariables |>
  mutate(goalDifferential = as.numeric(as.character(goalDifferential)),
         goalDifferential = pmin(goalDifferential + 1, 3),
         goalDifferential = factor(goalDifferential, levels = goal_diff_levels))

# create a copy of every faceoff scenario for a goal against
goalAgainstData <- leverageVariables |>
  mutate(goalDifferential = as.numeric(as.character(goalDifferential)),
         goalDifferential = pmax(goalDifferential - 1, -3),
         goalDifferential = factor(goalDifferential, levels = goal_diff_levels))

# predict three win probabilities and calculate the impact of a goal
leverageVariables <- leverageVariables |>
  mutate(currentWinProbability = predict(
    wp_gam, newdata = leverageVariables,
    type = "response"),
    
    winProbabilityGoalFor = predict(
      wp_gam, newdata = goalForData,
      type = "response"),
    
    winProbabilityGoalAgainst = predict(
      wp_gam, newdata = goalAgainstData,
      type = "response"),
    
    goalForImpact = winProbabilityGoalFor - currentWinProbability,
    goalAgainstImpact = currentWinProbability - winProbabilityGoalAgainst,
    totalGoalSwing = winProbabilityGoalFor - winProbabilityGoalAgainst)

xGF_bam <- bam(xGFor5 ~ goalDifferential + s(secondsRemaining, by = goalDifferential, k = 8, bs = "cr") +
                 zoneCode + situationCode + isOT + s(logAvailableWindow, k = 5, bs = "cr"),
               family = tw(link = "log"), method = "fREML", discrete = TRUE, data = leverageVariables)

xGA_bam <- bam(xGAgainst5 ~ goalDifferential + s(secondsRemaining, by = goalDifferential, k = 8, bs = "cr") +
                 zoneCode + situationCode + isOT + s(logAvailableWindow, k = 5, bs = "cr"),
               family = tw(link = "log"), method = "fREML", discrete = TRUE, data = leverageVariables)

leverageVariables <- leverageVariables |>
  mutate(predictedxGF_5 = predict(
    xGF_bam, 
    newdata = leverageVariables,
    type = "response"),
    
    predictedxGA_5 = predict(
      xGA_bam,
      newdata = leverageVariables,
      type = "response"),
    
    offensiveLeverage = goalForImpact * predictedxGF_5,
    defensiveLeverage = goalAgainstImpact * predictedxGA_5,
    rawLeverage = offensiveLeverage + defensiveLeverage)

# set the leverage score on a scale of 0 to 1, round 4 decimal places
leverageVariables <- leverageVariables |>
  mutate(leverage = round(1 * (rawLeverage - min(rawLeverage, na.rm = TRUE)) /
                            (max(rawLeverage, na.rm = TRUE) - 
                               min(rawLeverage, na.rm = TRUE)), 4))

# join leverage score into cleaned faceoffs
faceoffData <- faceoffsCleaned |>
  left_join(leverageVariables |> select(eventId, faceoffWon, leverage),
            by = c("eventId", "faceoffWon"), relationship = "many-to-one")

saveRDS(faceoffData, "faceoffData.rds")
saveRDS(leverageVariables, "leverageVariables.rds")
