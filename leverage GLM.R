rm(list=ls())
library(tidyverse)
library(splines)
faceoffsCleaned <- readRDS("faceoffsCleaned.rds")

# define goal differential levels
goal_diff_levels <- c(-3, -2, -1, 0, 1, 2, 3)

# select variables and create factors 
leverageVariables <- faceoffsCleaned |>
  select(eventId, faceoffPlayerId, player, secondsRemaining, isOT,
    manDifferential, isEmptyNetFor, isEmptyNetAgainst,
    goalDifferential, zoneCode, faceoffSituation,
    wonGame, USATFor5, USATAgainst5, xGFor5, xGAgainst5) |>
  mutate(goalDifferential = factor(
      goalDifferential, levels = goal_diff_levels),
    zoneCode = factor(zoneCode,
      levels = c("D", "N", "O")))

# fit the win probability model
# time and score interaction
# spline allows for time remaining to have non-linear relationship with win probability
wp_glm <- glm(wonGame ~ goalDifferential * ns(secondsRemaining, df = 5) +
    isOT + manDifferential + isEmptyNetFor + isEmptyNetAgainst +
    zoneCode, family = binomial(), data = leverageVariables)

# create a copy of every faceoff scenario for a goal scored
goalForData <- leverageVariables |>
  mutate(goalDifferential = as.numeric(as.character(goalDifferential)),
    goalDifferential = pmin(goalDifferential + 1, 3),
    goalDifferential = factor(
      goalDifferential,
      levels = goal_diff_levels))

# create a copy of every faceoff scenario for a goal against
goalAgainstData <- leverageVariables |>
  mutate(goalDifferential = as.numeric(as.character(goalDifferential)),
    goalDifferential = pmax(goalDifferential - 1, -3),
    goalDifferential = factor(
      goalDifferential,
      levels = goal_diff_levels))

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

usatFor_glm <- glm(USATFor5 ~ zoneCode + goalDifferential * ns(secondsRemaining, df = 5) +
    isOT + manDifferential + isEmptyNetFor + isEmptyNetAgainst, 
    family = binomial(), data = leverageVariables)

usatAgainst_glm <- glm(USATAgainst5 ~ zoneCode + goalDifferential * ns(secondsRemaining, df = 5) +
    isOT + manDifferential + isEmptyNetFor + isEmptyNetAgainst,
    family = binomial(), data = leverageVariables)

# model probability of an USAT for - allows OZ faceoffs to have higher immediate leverage
# model probability of an USAT against - allows DZ faceoffs to have higher immediate leverage
# create offensive and defensive opportunity leverage
# offensive leverage = how valuable a goal would be, how likely the team is to generate an immediate USAT
# defensive leverage = how damaging a goal against would be, how likely opponent generates immediate USAT
# high leverage = goal for would increase win probability, goal against would reduce win probability, and zone + game state make an immediate USAT likely.
leverageVariables <- leverageVariables |>
  mutate(probabilityUSATFor5 = predict(
      usatFor_glm, 
      newdata = leverageVariables,
      type = "response"),
    
    probabilityUSATAgainst5 = predict(
      usatAgainst_glm,
      newdata = leverageVariables,
      type = "response"),
    
    offensiveOpportunityLeverage = goalForImpact * probabilityUSATFor5,
    defensiveOpportunityLeverage = goalAgainstImpact * probabilityUSATAgainst5,
    opportunityAdjustedLeverage = offensiveOpportunityLeverage + defensiveOpportunityLeverage)

# set the leverage score on a scale of 0 to 100, round 4 decimal places
leverageVariables <- leverageVariables |>
  mutate(leverage = round(100 * (opportunityAdjustedLeverage -
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
