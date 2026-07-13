rm(list=ls())
library(tidyverse)
library(mgcv)
library(broom)
library(gratia)

wp_gam <- gam(wonGame ~ goalDifferential + s(secondsRemaining, by = goalDifferential, k = 10) +
    zoneCode * manDifferential + isOT + isEmptyNetFor + isEmptyNetAgainst,
    family = binomial(link = "logit"), method = "REML", data = leverageVariables)

tidy(wp_gam)
tidy(wp_gam, parametric = TRUE)
draw(wp_gam)

set.seed(0711)

N_FOLDS <- 5

game_folds <- leverageVariables |>
  distinct(gameId) |>
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

leverageVariables <- leverageVariables |>
  left_join(game_folds, by = "gameId")

wp_cv <- function(x) {
  
  message("Running fold ", x, " of ", N_FOLDS)
  
  wp_train <- leverageVariables |>
    filter(fold != x)
  
  wp_test <- leverageVariables |>
    filter(fold == x)
  
  gam_fit <- bam(
    wonGame ~
      goalDifferential +
      s(
        secondsRemaining,
        by = goalDifferential,
        k = 10
      ) +
      zoneCode * manDifferential +
      isOT +
      isEmptyNetFor +
      isEmptyNetAgainst,
    family = binomial(link = "logit"),
    method = "fREML",
    discrete = TRUE,
    data = wp_train
  )
  
  logit_fit <- glm(
    wonGame ~
      goalDifferential * ns(secondsRemaining, df = 7) +
      zoneCode * manDifferential +
      isOT +
      isEmptyNetFor +
      isEmptyNetAgainst,
    family = binomial(link = "logit"),
    data = wp_train
  )
  
  tibble(
    gam_pred = predict(
      gam_fit,
      newdata = wp_test,
      type = "response"
    ),
    logit_pred = predict(
      logit_fit,
      newdata = wp_test,
      type = "response"
    ),
    test_actual = wp_test$wonGame,
    test_fold = x
  )
}

wp_preds <- map(1:N_FOLDS, wp_cv) |> list_rbind()

wp_cv_by_fold <- wp_preds |>
  group_by(test_fold) |>
  summarise(
    gam_auc = as.numeric(
      auc(
        roc(
          test_actual,
          gam_pred,
          quiet = TRUE
        )
      )
    ),
    
    logit_auc = as.numeric(
      auc(
        roc(
          test_actual,
          logit_pred,
          quiet = TRUE
        )
      )
    ),
    
    gam_brier = mean(
      (test_actual - gam_pred)^2,
      na.rm = TRUE
    ),
    
    logit_brier = mean(
      (test_actual - logit_pred)^2,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

wp_cv_by_fold
