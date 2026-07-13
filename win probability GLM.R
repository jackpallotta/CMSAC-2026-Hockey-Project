rm(list = ls())
library(tidyverse)
library(broom)
library(gtsummary)
library(regressinator)
library(DHARMa)
library(ggeffects)
library(pROC)

theme_set(theme_light())

faceoffsCleaned <- readRDS("faceoffsCleaned.rds")

goal_diff_levels <- c(-3, -2, -1, 0, 1, 2, 3)

# prepare the data
leverageVariables <- faceoffsCleaned |>
  select(gameId, eventId, faceoffPlayerId, player, secondsRemaining, isOT,
    manDifferential, isEmptyNetFor, isEmptyNetAgainst,
    goalDifferential, zoneCode, faceoffSituation, wonGame,
    USATFor5, USATAgainst5, xGFor5, xGAgainst5) |>
  mutate(wonGame = as.integer(wonGame),
    goalDifferential = factor(goalDifferential, levels = goal_diff_levels),
    zoneCode = factor(zoneCode, levels = c("D", "N", "O")),
    isOT = factor(isOT, levels = c(0, 1), labels = c("Regulation", "Overtime")),
    isEmptyNetFor = factor(isEmptyNetFor, levels = c(0, 1), labels = c("No", "Yes")),
    isEmptyNetAgainst = factor(isEmptyNetAgainst, levels = c(0, 1), labels = c("No", "Yes")))

# check response distribution (mirroed dataset)
table(leverageVariables$wonGame)
prop.table(table(leverageVariables$wonGame))

# fit first logit regression
wp_logit_initial <- glm(wonGame ~ goalDifferential + secondsRemaining + isOT +
                          manDifferential + isEmptyNetFor + isEmptyNetAgainst + zoneCode,
                        data = leverageVariables, family = binomial(link = "logit"))

# inspect coefficients as odds ratios
tidy(wp_logit_initial, exponentiate = TRUE, conf.int = TRUE)
tbl_regression(wp_logit_initial, exponentiate = TRUE)

# evaluate the linearity assumption for time remaining
rate_by_time <- leverageVariables |>
  bin_by_quantile(secondsRemaining, breaks = 20) |>
  group_by(goalDifferential, .bin) |>
  summarize(mean_seconds = mean(secondsRemaining, na.rm = TRUE),
            win_probability = mean(wonGame, na.rm = TRUE),
            log_odds = empirical_link(wonGame, family = binomial(link = "logit")),
            n = n(),
            .groups = "drop")

# create plot
rate_by_time |>
  ggplot(aes(x = mean_seconds, y = log_odds,
             group = goalDifferential,
             color = goalDifferential)) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~ goalDifferential, scales = "free_y") +
  guides(size = "none") +
  labs(x = "Seconds Remaining",
       y = "Empirical Log-Odds of Winning",
       color = "Goal Differential",
       title = "Relationship Between Time Remaining and Win Probability")

# check the numeric manpower assumption
rate_by_manpower <- leverageVariables |>
  group_by(manDifferential) |>
  summarize(win_probability = mean(wonGame, na.rm = TRUE),
            log_odds = empirical_link(wonGame, family = binomial(link = "logit")),
            n = n(),
            .groups = "drop")

# create plot
rate_by_manpower |>
  ggplot(aes(x = manDifferential, y = log_odds)) +
  geom_point(aes(size = n)) +
  geom_line() +
  guides(size = "none") +
  labs(x = "Manpower Differential",
       y = "Empiricial Log-Odds of Winning")

set.seed(711)

dh <- simulateResiduals(fittedModel = wp_logit_initial, n = 250)
plot(dh)

# simulated quantile residuals to an augmented dataset
wp_aug <- augment(wp_logit_initial, type.predict = "response") |>
  mutate(.quantile_resid = residuals(dh))

# create plot
wp_aug |>
  ggplot(aes(x = .fitted, y = .quantile_resid)) +
  geom_point(alpha = 0.05) +
  geom_hline(yintercept = 0, color = "orangered", linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted Win Probability", y = "DHARMa Quantile Residual")

# generate predicted probabilities and classes
win_pred_prob <- predict(wp_logit_initial, type = "response")
win_pred_class <- if_else(win_pred_prob > 0.5, 1L, 0L)
mean(win_pred_class != leverageVariables$wonGame, na.rm = TRUE)
mean(win_pred_class == leverageVariables$wonGame, na.rm = TRUE)

# confusion matrix
table(Predicted = factor(
    win_pred_class,
    levels = c(1, 0),
    labels = c("Win", "Loss")),
  Observed = factor(
    leverageVariables$wonGame,
    levels = c(1, 0),
    labels = c("Win", "Loss")))

# AUC
wp_roc <- roc(response = leverageVariables$wonGame,
  predictor = win_pred_prob, quiet = TRUE)
auc(wp_roc)

# brier score
brier_score <- mean((leverageVariables$wonGame - win_pred_prob)^2, na.rm = TRUE)
brier_score

# calibration table
wp_calibration <- leverageVariables |>
  mutate(predicted_probability = win_pred_prob,
         decile = ntile(predicted_probability, 10)) |>
  group_by(decile) |>
  summarize(predicted = mean(predicted_probability, na.rm = TRUE),
            observed = mean(wonGame, na.rm = TRUE),
            n = n(),
            .groups = "drop")

# create plot
wp_calibration |>
  ggplot(aes(x = predicted, y = observed)) +
  geom_point(aes(size = n)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  guides(size = "none") +
  labs(x = "Mean Predicted Win Probability",
       y = "Observed Win Rate",
       title = "Win-Probability Calibration")

# evaluate the linearity assumption for time remaining by goal differential
rate_by_time_score <- leverageVariables |>
  group_by(goalDifferential) |>
  mutate(time_bin = ntile(secondsRemaining, 15)) |>
  group_by(goalDifferential, time_bin) |>
  summarize(mean_seconds = mean(secondsRemaining, na.rm = TRUE),
    prob = mean(wonGame, na.rm = TRUE),
    log_odds = empirical_link(wonGame, family = binomial(link = "logit")),
    n = n(),
    .groups = "drop")

# create plot
rate_by_time_score |>
  ggplot(aes(mean_seconds, log_odds)) +
  geom_point(aes(size = n)) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~ goalDifferential, scales = "free_y") +
  guides(size = "none") +
  labs(x = "Seconds Remaining",
    y = "Empirical Log-Odds of Winning",
    title = "Empirical Log-Odds by Time Remaining and Goal Differential")
