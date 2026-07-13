rm(list = ls())
library(tidyverse)
library(splines)

theme_set(theme_light())

faceoffsCleaned <- readRDS("faceoffsCleaned.rds")

goal_diff_levels <- c(-3, -2, -1, 0, 1, 2, 3)

# prepare the data
leverageVariables <- faceoffsCleaned |>
  select(gameId, eventId, faceoffPlayerId, player, secondsRemaining, secondsRemainingPeriod, isOT,
         manDifferential, isEmptyNetFor, isEmptyNetAgainst, goalDifferential, zoneCode, faceoffSituation, 
         wonGame, fullFiveSecondWindow, availableSATWindow, secondsUntilStoppage, stoppageReason, 
         SATWindowOutcome, SATFor5, SATAgainst5, USATFor5, USATAgainst5, blockedShotFor5, 
         blockedShotAgainst5, xGFor5, xGAgainst5) |>
  mutate(wonGame = as.integer(wonGame),
         manDifferentialFactor = factor(manDifferential),
         goalDifferential = factor(goalDifferential, levels = goal_diff_levels),
         zoneCode = factor(zoneCode, levels = c("D", "N", "O")),
         isOT = factor(isOT, levels = c(0, 1), labels = c("Regulation", "Overtime")),
         isEmptyNetFor = factor(isEmptyNetFor, levels = c(0, 1), labels = c("No", "Yes")),
         isEmptyNetAgainst = factor(isEmptyNetAgainst, levels = c(0, 1), labels = c("No", "Yes")),
         fullFiveSecondWindow = factor(fullFiveSecondWindow, levels = c(0, 1), labels = c("No", "Yes")),
         logAvailableSATWindow = log(availableSATWindow))

# win probability model
wp_glm <- glm(wonGame ~ goalDifferential * ns(secondsRemaining, df = 7) +
                zoneCode * manDifferential + isOT + isEmptyNetFor + isEmptyNetAgainst,
              data = leverageVariables, family = binomial(link = "logit"))

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
    wp_glm, newdata = leverageVariables,
    type = "response"),
    
    winProbabilityGoalFor = predict(
      wp_glm, newdata = goalForData,
      type = "response"),
    
    winProbabilityGoalAgainst = predict(
      wp_glm, newdata = goalAgainstData,
      type = "response"),
    
    goalForImpact = winProbabilityGoalFor - currentWinProbability,
    goalAgainstImpact = currentWinProbability - winProbabilityGoalAgainst,
    totalGoalSwing = winProbabilityGoalFor - winProbabilityGoalAgainst)

SAT_For_glm <- glm(SATFor5 ~ goalDifferential * ns(secondsRemaining, df = 7) +
                     zoneCode * manDifferentialFactor + isOT +
                     isEmptyNetFor + isEmptyNetAgainst + logAvailableSATWindow,
                   family = binomial(link = "logit"), data = leverageVariables)

SAT_Against_glm <- glm(SATAgainst5 ~ goalDifferential * ns(secondsRemaining, df = 7) +
                         zoneCode * manDifferentialFactor + isOT +
                         isEmptyNetFor + isEmptyNetAgainst + logAvailableSATWindow,
                       family = binomial(link = "logit"), data = leverageVariables)

leverageVariables <- leverageVariables |>
  mutate(probabilitySATFor5 = predict(
    SAT_For_glm, 
    newdata = leverageVariables,
    type = "response"),
    
    probabilitySATAgainst5 = predict(
      SAT_Against_glm,
      newdata = leverageVariables,
      type = "response"),
    
    offensiveOpportunityLeverage = goalForImpact * probabilitySATFor5,
    defensiveOpportunityLeverage = goalAgainstImpact * probabilitySATAgainst5,
    opportunityAdjustedLeverage = offensiveOpportunityLeverage + defensiveOpportunityLeverage)

# set the leverage score on a scale of 0 to 1, round 4 decimal places
leverageVariables <- leverageVariables |>
  mutate(leverage = round(1 * (opportunityAdjustedLeverage -
                                   min(opportunityAdjustedLeverage, na.rm = TRUE)) /
                            (max(opportunityAdjustedLeverage, na.rm = TRUE) -
                               min(opportunityAdjustedLeverage, na.rm = TRUE)), 4))

# AUC and calibration
library(pROC)

pred <- predict(wp_glm, type = "response")
roc(leverageVariables$wonGame, pred)

leverageVariables |>
  mutate(pred = predict(wp_glm, type = "response"),
         decile = ntile(pred, 10)) |>
  group_by(decile) |>
  summarize(predicted = mean(pred),
            observed = mean(wonGame))
