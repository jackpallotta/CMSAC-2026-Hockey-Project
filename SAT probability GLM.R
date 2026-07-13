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
  select(gameId, eventId, faceoffPlayerId, player, secondsRemaining, secondsRemainingPeriod, isOT,
    manDifferential, isEmptyNetFor, isEmptyNetAgainst, goalDifferential, zoneCode, faceoffSituation, 
    wonGame, fullFiveSecondWindow, availableSATWindow, secondsUntilStoppage, stoppageReason, 
    SATWindowOutcome, SATFor5, SATAgainst5, USATFor5, USATAgainst5, blockedShotFor5, 
    blockedShotAgainst5, xGFor5, xGAgainst5) |>
  mutate(wonGame = as.integer(wonGame),
    goalDifferential = factor(goalDifferential, levels = goal_diff_levels),
    zoneCode = factor(zoneCode, levels = c("D", "N", "O")),
    isOT = factor(isOT, levels = c(0, 1), labels = c("Regulation", "Overtime")),
    isEmptyNetFor = factor(isEmptyNetFor, levels = c(0, 1), labels = c("No", "Yes")),
    isEmptyNetAgainst = factor(isEmptyNetAgainst, levels = c(0, 1), labels = c("No", "Yes")),
    fullFiveSecondWindow = factor(fullFiveSecondWindow, levels = c(0, 1), labels = c("No", "Yes")),
    logAvailableSATWindow = log(availableSATWindow))

# check distribution
leverageVariables |>
  distinct(gameId, eventId, .keep_all = TRUE) |>
  count(availableSATWindow, fullFiveSecondWindow) |>
  mutate(pct = n / sum(n))

leverageVariables |>
  distinct(gameId, eventId, .keep_all = TRUE) |>
  group_by(availableSATWindow) |>
  summarize(n = n(), SAT_rate = mean(SATFor5), .groups = "drop")

# check response distribution (mirroed dataset)
table(leverageVariables$SATFor5)
prop.table(table(leverageVariables$SATFor5))

# fit first logit regression
SAT_logit_initial <- glm(SATFor5 ~ goalDifferential + secondsRemaining + isOT +
                          manDifferential + isEmptyNetFor + isEmptyNetAgainst + zoneCode,
                        data = leverageVariables, family = binomial(link = "logit"))

# inspect coefficients as odds ratios
tidy(SAT_logit_initial, exponentiate = TRUE, conf.int = TRUE)
tbl_regression(SAT_logit_initial, exponentiate = TRUE)

# evaluate the linearity assumption for time remaining
rate_by_time <- leverageVariables |>
  bin_by_quantile(secondsRemaining, breaks = 20) |>
  summarize(mean_seconds = mean(secondsRemaining),
            SAT_probability = mean(SATFor5),
            log_odds = empirical_link(SATFor5, family = binomial(link = "logit")),
            n = n())

# evaluate the linearity assumption for time remaining by goal differential
rate_by_time_grouped <- leverageVariables |>
  bin_by_quantile(secondsRemaining, breaks = 20) |>
  group_by(goalDifferential, .bin) |>
  summarize(mean_seconds = mean(secondsRemaining, na.rm = TRUE),
            SAT_probability = mean(SATFor5, na.rm = TRUE),
            log_odds = empirical_link(SATFor5, family = binomial(link = "logit")),
            n = n(),
            .groups = "drop")

# create plot
rate_by_time_grouped |>
  ggplot(aes(x = mean_seconds, y = log_odds,
             group = goalDifferential,
             color = goalDifferential)) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(~ goalDifferential, scales = "free_y") +
  guides(size = "none") +
  labs(x = "Seconds Remaining",
       y = "Empirical Log-Odds of SAT",
       color = "Goal Differential",
       title = "Relationship Between Time Remaining and SAT Probability")

# check the numeric manpower assumption
rate_by_manpower <- leverageVariables |>
  group_by(manDifferential) |>
  summarize(SAT_probability = mean(SATFor5, na.rm = TRUE),
            log_odds = empirical_link(SATFor5, family = binomial(link = "logit")),
            n = n(),
            .groups = "drop")

# create plot
rate_by_manpower |>
  ggplot(aes(x = manDifferential, y = log_odds)) +
  geom_point(aes(size = n)) +
  geom_line() +
  guides(size = "none") +
  labs(x = "Manpower Differential",
       y = "Empiricial Log-Odds of SAT")

set.seed(711)

dh <- simulateResiduals(fittedModel = SAT_logit_initial, n = 250)
plot(dh)

# simulated quantile residuals to an augmented dataset
wp_aug <- augment(SAT_logit_initial, type.predict = "response") |>
  mutate(.quantile_resid = residuals(dh))

# create plot
wp_aug |>
  ggplot(aes(x = .fitted, y = .quantile_resid)) +
  geom_point(alpha = 0.05) +
  geom_hline(yintercept = 0, color = "orangered", linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted SAT Probability", y = "DHARMa Quantile Residual")

# generate predicted probabilities and classes
SAT_pred_prob <- predict(SAT_logit_initial, type = "response")
SAT_pred_class <- if_else(SAT_pred_prob > 0.5, 1L, 0L)
mean(SAT_pred_class != leverageVariables$SATFor5, na.rm = TRUE)
mean(SAT_pred_class == leverageVariables$SATFor5, na.rm = TRUE)

# confusion matrix
table(Predicted = factor(
  SAT_pred_class,
  levels = c(1, 0),
  labels = c("SAT", "No SAT")),
  Observed = factor(
    leverageVariables$SATFor5,
    levels = c(1, 0),
    labels = c("SAT", "No SAT")))

# AUC
SAT_roc <- roc(response = leverageVariables$SATFor5,
              predictor = SAT_pred_prob, quiet = TRUE)
auc(SAT_roc)

# brier score
brier_score <- mean((leverageVariables$SATFor5 - SAT_pred_prob)^2, na.rm = TRUE)
brier_score

# calibration table
SAT_calibration <- leverageVariables |>
  mutate(predicted_probability = SAT_pred_prob,
         decile = ntile(predicted_probability, 10)) |>
  group_by(decile) |>
  summarize(predicted = mean(predicted_probability, na.rm = TRUE),
            observed = mean(SATFor5, na.rm = TRUE),
            n = n(),
            .groups = "drop")

# create plot
SAT_calibration |>
  ggplot(aes(x = predicted, y = observed)) +
  geom_point(aes(size = n)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  guides(size = "none") +
  labs(x = "Mean Predicted SAT Probability",
       y = "Observed SAT Rate",
       title = "SAT-Probability Calibration")

# evaluate the linearity assumption for time remaining by goal differential
rate_by_time_score <- leverageVariables |>
  group_by(goalDifferential) |>
  mutate(time_bin = ntile(secondsRemaining, 15)) |>
  group_by(goalDifferential, time_bin) |>
  summarize(mean_seconds = mean(secondsRemaining, na.rm = TRUE),
            prob = mean(SATFor5, na.rm = TRUE),
            log_odds = empirical_link(SATFor5, family = binomial(link = "logit")),
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
       y = "Empirical Log-Odds of SAT",
       title = "Empirical Log-Odds by Time Remaining and Goal Differential")
