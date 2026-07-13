rm(list = ls())
library(tidyverse)
library(splines)
library(pROC)

# Choose the spline flexibility to test
spline_df <- 7

# baseline logistic regression
SAT_logit_initial <- glm(SATFor5 ~ goalDifferential + secondsRemaining +
    zoneCode + manDifferential + isOT + isEmptyNetFor + isEmptyNetAgainst,
  family = binomial(link = "logit"), data = leverageVariables)

# nonlinear time, additive zone and numeric manpower effects
SAT_glm_spline_base <- glm(SATFor5 ~ goalDifferential * ns(secondsRemaining, df = spline_df) +
    zoneCode + manDifferential + isOT + isEmptyNetFor + isEmptyNetAgainst,
  family = binomial(link = "logit"), data = leverageVariables)

# nonlinear time plus zone-by-numeric-manpower interaction
SAT_glm_spline <- glm(SATFor5 ~ goalDifferential * ns(secondsRemaining, df = spline_df) +
    zoneCode * manDifferential + isOT + isEmptyNetFor + isEmptyNetAgainst,
  family = binomial(link = "logit"), data = leverageVariables)

# treat manpower differential categorically
SAT_glm_manpower_factor <- glm(SATFor5 ~ goalDifferential * ns(secondsRemaining, df = spline_df) +
    zoneCode + factor(manDifferential) + isOT + isEmptyNetFor + isEmptyNetAgainst, 
    family = binomial(link = "logit"), data = leverageVariables)

# categorical manpower effect allowed to differ by zone
SAT_glm_factor_interaction <- glm(SATFor5 ~ goalDifferential * ns(secondsRemaining, df = spline_df) +
    zoneCode * factor(manDifferential) + isOT + isEmptyNetFor + isEmptyNetAgainst, 
    family = binomial(link = "logit"), data = leverageVariables)

# add the actual amount of available SAT exposure
SAT_logit_window <- glm(SATFor5 ~ goalDifferential * ns(secondsRemaining, df = spline_df) +
    zoneCode * factor(manDifferential) + isOT + isEmptyNetFor + isEmptyNetAgainst +
    logAvailableSATWindow, family = binomial(link = "logit"), data = leverageVariables)

models <- list(linear = SAT_logit_initial,
  spline_base = SAT_glm_spline_base,
  spline_numeric_interaction = SAT_glm_spline,
  spline_factor_manpower = SAT_glm_manpower_factor,
  spline_factor_interaction = SAT_glm_factor_interaction,
  window = SAT_logit_window)

SAT_model_comparison <- tibble(
  model = names(models),
  AUC = map_dbl(models, \(m) {
    pred <- predict(m, type = "response")
    
    as.numeric(
      auc(
        roc(
          response = leverageVariables$SATFor5,
          predictor = pred,
          quiet = TRUE
        )
      )
    )
  }),
  Brier = map_dbl(models, \(m) {
    pred <- predict(m, type = "response")
    
    mean(
      (leverageVariables$SATFor5 - pred)^2,
      na.rm = TRUE
    )
  }),
  AIC = map_dbl(models, AIC))

SAT_model_comparison