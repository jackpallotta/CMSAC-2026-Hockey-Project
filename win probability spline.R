rm(list=ls())
library(splines)
library(pROC)
library(purrr)
library(tibble)

# fit spline regression, tested df = 3, 4, 5, 6, 7
wp_glm_spline_base <- glm(wonGame ~ goalDifferential * ns(secondsRemaining, df = 7) +
                       isOT + manDifferential + isEmptyNetFor + isEmptyNetAgainst + zoneCode,
                     data = leverageVariables, family = binomial())

# interaction between zoneCode and manDifferential
wp_glm_spline <- glm(wonGame ~ goalDifferential * ns(secondsRemaining, df = 7) +
                       zoneCode * manDifferential + isOT + isEmptyNetFor + isEmptyNetAgainst,
                     data = leverageVariables, family = binomial())

models <- list(linear = wp_logit_initial, spline_base = wp_glm_spline_base, spline = wp_glm_spline)

wp_model_comparison <- tibble(model = names(models),
    AUC = map_dbl(models, \(m) {
      pred <- predict(m, type = "response")
      
      as.numeric(auc(roc(leverageVariables$wonGame,
                         pred, quiet = TRUE)))
    }),
    Brier = map_dbl(models, \(m) {
      pred <- predict(m, type = "response")
      
      mean((leverageVariables$wonGame - pred)^2,
      na.rm = TRUE)
    }),
    AIC = map_dbl(models, AIC))

wp_model_comparison

anova(wp_glm_spline_base, wp_glm_spline, test = "Chisq")
