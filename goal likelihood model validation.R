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

xG_cv <- function(x) {
  xG_train <- leverageVariables |> filter(fold != x)
  xG_test <- leverageVariables |> filter(fold == x)
  
  xGF_tweedie_gam <- bam(xGFor5 ~ goalDifferential + s(secondsRemaining, by = goalDifferential, k = 8, bs = "cr") +
                   zoneCode + situationCode + isOT + s(logAvailableWindow, k = 5, bs = "cr"),
                 data = xG_train, family = tw(link = "log"), method = "fREML", discrete = TRUE)
  xGF_tweedie_glm <- bam(xGFor5 ~ goalDifferential * secondsRemaining +
                     zoneCode + situationCode + isOT + logAvailableWindow,
                   data = xG_train, family = tw(link = "log"), method = "fREML", discrete = TRUE, nthreads = 4)
  xGF_gaussian_gam <- bam(xGFor5 ~ goalDifferential + s(secondsRemaining, by = goalDifferential, k = 8, bs = "cr") +
                      zoneCode + situationCode + isOT + s(logAvailableWindow, k = 5, bs = "cr"),
                    data = xG_train, family = gaussian(link = "identity"), method = "fREML", discrete = TRUE, nthreads = 4)
  
  xG_out <- tibble(
    xGF_tweedie_gam_pred = predict(xGF_tweedie_gam, newdata = xG_test, type = "response"),
    xGF_tweedie_glm_pred = predict(xGF_tweedie_glm, newdata = xG_test, type = "response"),
    xGF_gaussian_gam_pred = predict(xGF_gaussian_gam, newdata = xG_test, type = "response"),
    test_actual = xG_test$xGFor5,
    test_fold = x
  )
  return(xG_out)  
}

xG_preds <- map(1:N_FOLDS, xG_cv) |> 
  list_rbind()

xG_metrics <- xG_preds |>
  pivot_longer(cols = ends_with("_pred"), names_to = "model", values_to = "test_pred") |>
  group_by(model, test_fold) |>
  summarize(rmse = sqrt(mean((test_actual - test_pred)^2)), mae = mean(abs(test_actual - test_pred)),
    mse = mean((test_actual - test_pred)^2), .groups = "drop") |>
  group_by(model) |>
  summarize(cv_rmse = mean(rmse),
    se_rmse = sd(rmse) / sqrt(n()), cv_mae = mean(mae), se_mae = sd(mae) / sqrt(n()),
    cv_mse = mean(mse), se_mse = sd(mse) / sqrt(n()), .groups = "drop")

xG_metrics

xG_calibration <- xG_preds |>
  pivot_longer(cols = ends_with("_pred"), names_to = "model", values_to = "predicted_xG") |>
  group_by(model) |>
  mutate(prediction_tile = ntile(predicted_xG, 10)) |>
  group_by(model, prediction_tile) |>
  summarize(mean_predicted_xG = mean(predicted_xG), mean_observed_xG = mean(test_actual),
    calibration_ratio = mean_observed_xG / mean_predicted_xG, n = n(), .groups = "drop")

xG_calibration

xG_negative_predictions <- xG_preds |>
  summarize(negative_gaussian_count = sum(xGF_gaussian_gam_pred < 0),
    negative_gaussian_perc = mean(xGF_gaussian_gam_pred < 0),
    gaussian_prediction_min = min(xGF_gaussian_gam_pred))

xG_negative_predictions

library(ggplot2)

xG_calibration |>
  filter(model == "xGF_tweedie_gam_pred") |>
  ggplot(aes(x = mean_predicted_xG, y = mean_observed_xG)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2", size = 3) +
  coord_equal() +
  labs(title = "Calibration of the Tweedie GAM", x = "Mean Predicted xG", y = "Mean Observed xG") +
  theme_light(base_size = 14)
