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

leverageVariables <- faceoffsCleaned |>
  select(gameId, eventId, faceoffPlayerId, player, secondsRemaining,
    secondsRemainingPeriod, situationCode, goalDifferential, zoneCode,
    isOT, faceoffSituation, faceoffWon, wonGame, fullFiveSecondWindow,
    availableSATWindow, secondsUntilStoppage, stoppageReason, SATWindowOutcome,
    xGFor5, xGAgainst5, goalFor5, goalAgainst5) |>
  mutate(wonGame = as.integer(wonGame),
    situationCode = case_when(situationCode %in% c("6v3", "6v4") ~ "EN +2",
      situationCode %in% c("3v6", "4v6") ~ "EN -2", TRUE ~ situationCode),
    situationCode = factor(situationCode),
    goalDifferential = factor(goalDifferential, levels = goal_diff_levels),
    zoneCode = factor(zoneCode, levels = c("D", "N", "O")),
    isOT = factor(isOT, levels = c(0, 1), labels = c("Regulation", "Overtime")),
    fullFiveSecondWindow = factor(fullFiveSecondWindow, levels = c(0, 1), labels = c("No", "Yes")),
    faceoffWon = as.integer(faceoffWon),
    logAvailableWindow = log1p(availableSATWindow))

set.seed(721)

N_FOLDS <- 5

faceoff_folds <- leverageVariables |>
  distinct(gameId) |>
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

leverageVariables <- leverageVariables |>
  left_join(faceoff_folds,
    by = "gameId",
    relationship = "many-to-one")

tweedie_deviance_score <- function(y, mu, p = 1.5) {

  mu <- pmax(mu, 1e-10)
  
  2 * (y^(2 - p) / ((1 - p) * (2 - p)) - 
         y * mu^(1 - p) / (1 - p) + 
         mu^(2 - p) / (2 - p))
}

xG_cv <- function(x) {
  
  xG_train <- leverageVariables |>
    filter(fold != x)
  
  xG_test <- leverageVariables |>
    filter(fold == x)
  
  xGF_gam_reduced <- bam(xGFor5 ~ faceoffWon + zoneCode + goalDifferential +
      s(secondsRemaining, by = goalDifferential, k = 8, bs = "cr") +
      situationCode + isOT +
      s(logAvailableWindow, k = 5, bs = "cr"),
    data = xG_train, family = Tweedie(p = 1.5, link = "log"),
    method = "fREML", discrete = TRUE, nthreads = 4)
  
  xGF_gam_full <- bam(xGFor5 ~ faceoffWon * zoneCode + goalDifferential +
      s(secondsRemaining, by = goalDifferential, k = 8, bs = "cr") +
      situationCode + isOT + s(logAvailableWindow, k = 5, bs = "cr"),
      data = xG_train, family = Tweedie(p = 1.5, link = "log"),
      method = "fREML", discrete = TRUE, nthreads = 4)
  
  xGF_linear_reduced <- bam(xGFor5 ~ faceoffWon + zoneCode +
      goalDifferential * secondsRemaining + situationCode +
      isOT + logAvailableWindow, data = xG_train,
      family = Tweedie(p = 1.5, link = "log"),
      method = "fREML", discrete = TRUE, nthreads = 4)
  
  xGF_linear_full <- bam(xGFor5 ~ faceoffWon * zoneCode +
      goalDifferential * secondsRemaining + situationCode +
      isOT + logAvailableWindow, data = xG_train,
      family = Tweedie(p = 1.5, link = "log"),
      method = "fREML", discrete = TRUE, nthreads = 4)
  
  tibble(gam_reduced_pred = predict(
      xGF_gam_reduced,
      newdata = xG_test,
      type = "response"),
    
    gam_full_pred = predict(
      xGF_gam_full,
      newdata = xG_test,
      type = "response"),
    
    linear_reduced_pred = predict(
      xGF_linear_reduced,
      newdata = xG_test,
      type = "response"),
    
    linear_full_pred = predict(
      xGF_linear_full,
      newdata = xG_test,
      type = "response"),
    
    test_actual = xG_test$xGFor5,
    test_fold = x
  )
}

xG_preds <- map(1:N_FOLDS, xG_cv) |>
  list_rbind()

xG_fold_metrics <- xG_preds |>
  pivot_longer(cols = ends_with("_pred"), names_to = "model", values_to = "test_pred") |>
  group_by(model, test_fold) |>
  summarize(rmse = sqrt(mean((test_actual - test_pred)^2, na.rm = TRUE)),
            mae = mean(abs(test_actual - test_pred), na.rm = TRUE), 
            mse = mean((test_actual - test_pred)^2, na.rm = TRUE),
            tweedie_deviance = mean(tweedie_deviance_score(y = test_actual, mu = test_pred,
                                                           p = 1.5), na.rm = TRUE),
            .groups = "drop")

xG_fold_metrics

xG_metrics <- xG_fold_metrics |>
  group_by(model) |>
  summarize(cv_rmse = mean(rmse),
    se_rmse = sd(rmse) / sqrt(n()),
    cv_mae = mean(mae),
    se_mae = sd(mae) / sqrt(n()),
    cv_mse = mean(mse),
    se_mse = sd(mse) / sqrt(n()),
    cv_tweedie_deviance = mean(tweedie_deviance),
    se_tweedie_deviance = sd(tweedie_deviance) / sqrt(n()),
    .groups = "drop") |>
  arrange(cv_tweedie_deviance)

xG_metrics

gam_interaction_comparison <- xG_fold_metrics |>
  filter(model %in% c("gam_reduced_pred", "gam_full_pred")) |>
  select(test_fold, model, rmse, mae, tweedie_deviance) |>
  pivot_wider(names_from = model, values_from = c(rmse, mae, tweedie_deviance)) |>
  mutate(rmse_improvement = rmse_gam_reduced_pred - rmse_gam_full_pred,
         mae_improvement = mae_gam_reduced_pred - mae_gam_full_pred,
         deviance_improvement = tweedie_deviance_gam_reduced_pred - tweedie_deviance_gam_full_pred)

gam_interaction_comparison

gam_interaction_summary <- gam_interaction_comparison |>
  summarize(mean_rmse_improvement = mean(rmse_improvement),
            mean_mae_improvement = mean(mae_improvement),
            mean_deviance_improvement = mean(deviance_improvement),
            folds_better_rmse = sum(rmse_improvement > 0),
            folds_better_mae = sum(mae_improvement > 0),
            folds_better_deviance = sum(deviance_improvement > 0),
            folds_total = n())

gam_interaction_summary

linear_interaction_comparison <- xG_fold_metrics |>
  filter(model %in% c("linear_reduced_pred", "linear_full_pred")) |>
  select(test_fold, model, rmse, mae, tweedie_deviance) |>
  pivot_wider(names_from = model, values_from = c(rmse, mae, tweedie_deviance)) |>
  mutate(rmse_improvement = rmse_linear_reduced_pred - rmse_linear_full_pred,
         mae_improvement = mae_linear_reduced_pred - mae_linear_full_pred,
         deviance_improvement = tweedie_deviance_linear_reduced_pred - tweedie_deviance_linear_full_pred)

linear_interaction_comparison

linear_interaction_summary <- linear_interaction_comparison |>
  summarize(mean_rmse_improvement = mean(rmse_improvement),
            mean_mae_improvement = mean(mae_improvement),
            mean_deviance_improvement = mean(deviance_improvement),
            folds_better_rmse = sum(rmse_improvement > 0),
            folds_better_mae = sum(mae_improvement > 0),
            folds_better_deviance = sum(deviance_improvement > 0),
            folds_total = n())

linear_interaction_summary

xG_calibration <- xG_preds |>
  pivot_longer(cols = ends_with("_pred"), names_to = "model", values_to = "predicted_xG") |>
  group_by(model) |>
  mutate(prediction_decile = ntile(predicted_xG, 10)) |>
  group_by(model, prediction_decile) |>
  summarize(mean_predicted_xG = mean(predicted_xG, na.rm = TRUE),
    mean_observed_xG = mean(test_actual, na.rm = TRUE),
    calibration_ratio = mean_observed_xG / mean_predicted_xG,
    n = n(),
    .groups = "drop")

xG_calibration

xG_calibration |>
  filter(model == "gam_full_pred") |>
  ggplot(aes(x = mean_predicted_xG, y = mean_observed_xG)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2", size = 3) +
  coord_equal() +
  labs(title = "Calibration of the Conditional xGF GAM",
    subtitle = "Five-fold cross-validated predictions",
    x = "Mean Predicted xG",
    y = "Mean Observed xG") +
  theme_light(base_size = 14)