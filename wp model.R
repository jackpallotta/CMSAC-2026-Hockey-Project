rm(list = ls())
library(tidyverse)
library(splines)
library(mgcv)
library(pROC)
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

set.seed(717)

N_FOLDS <- 5

faceoff_folds <- leverageVariables |>
  distinct(gameId) |>
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

leverageVariables <- leverageVariables |>
  left_join(faceoff_folds, by = "gameId", relationship = "many-to-one")

wp_cv <- function(x) {
  wp_train <- leverageVariables |> filter(fold != x)
  wp_test <- leverageVariables |> filter(fold == x)
  
  gam_fit <- bam(wonGame ~ goalDifferential + s(secondsRemaining, by = goalDifferential, k = 8) +
                   zoneCode + situationCode + isOT,
                 data = wp_train, family = binomial(), method = "fREML", discrete = TRUE)
  logit_fit <- glm(wonGame ~ goalDifferential + secondsRemaining +
                     zoneCode + situationCode + isOT,
                   data = wp_train, family = binomial(link = "logit"))
  spline_fit <- glm(wonGame ~ goalDifferential * ns(secondsRemaining, df = 7) +
                      zoneCode + situationCode + isOT,
                    data = wp_train, family = binomial(link = "logit"))
  
  wp_out <- tibble(
    gam_pred = predict(gam_fit, newdata = wp_test, type = "response"),
    logit_pred = predict(logit_fit, newdata = wp_test, type = "response"),
    spline_pred = predict(spline_fit, newdata = wp_test, type = "response"),
    test_actual = wp_test$wonGame,
    test_fold = x
  )
  return(wp_out)  
}

wp_preds <- map(1:N_FOLDS, wp_cv) |> 
  list_rbind()

wp_preds |>
  pivot_longer(cols = ends_with("_pred"), names_to = "model", values_to = "test_pred") |>
  mutate(test_pred_class = as.integer(test_pred >= 0.5)) |>
  group_by(model, test_fold) |>
  summarize(accuracy = mean(test_pred_class == test_actual), .groups = "drop") |>
  group_by(model) |>
  summarize(cv_accuracy = mean(accuracy), se_accuracy = sd(accuracy) / sqrt(n()), .groups = "drop")

wp_auc <- wp_preds |> pivot_longer(cols = ends_with("_pred"), names_to = "model", values_to = "test_pred") |>
  group_by(model, test_fold) |>
  summarize(auc = as.numeric(roc(test_actual, test_pred, quiet = TRUE)$auc), .groups = "drop") |>
  group_by(model) |>
  summarize(cv_auc = mean(auc), se_auc = sd(auc) / sqrt(n()), .groups = "drop")

wp_auc

wp_brier <- wp_preds |> pivot_longer(cols = ends_with("_pred"), names_to = "model", values_to = "test_pred") |>
  group_by(model, test_fold) |>
  summarize(brier = mean((test_pred - test_actual)^2), .groups = "drop") |>
  group_by(model) |>
  summarize(cv_brier = mean(brier), se_brier = sd(brier) / sqrt(n()),.groups = "drop")

wp_brier

wp_calibration <- wp_preds |>
  pivot_longer(cols = ends_with("_pred"), names_to = "model", values_to = "test_pred") |>
  group_by(model) |>
  mutate(prediction_tile = ntile(test_pred, 10)) |>
  group_by(model, prediction_tile) |>
  summarize(predictedWP = mean(test_pred), observedWinPct = mean(test_actual), n = n(), .groups = "drop")

wp_calibration

