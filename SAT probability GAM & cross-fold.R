rm(list = ls())
library(tidyverse)
library(splines)
library(mgcv)
library(pROC)

leverageVariables <- leverageVariables |>
  mutate(
    manDifferential = factor(manDifferential)
  )

set.seed(711)

N_FOLDS <- 5

# assign entire games to folds
game_folds <- leverageVariables |>
  distinct(gameId) |>
  mutate(
    fold = sample(
      rep(1:N_FOLDS, length.out = n())
    )
  )

leverageVariables <- leverageVariables |>
  select(-any_of("fold")) |>
  left_join(game_folds, by = "gameId")

leverageVariables |>
  distinct(gameId, eventId, .keep_all = TRUE) |>
  count(fold, manDifferential)

sat_cv <- function(x) {
  
  message("Running fold ", x, " of ", N_FOLDS)
  
  sat_train <- leverageVariables |>
    filter(fold != x)
  
  sat_test <- leverageVariables |>
    filter(fold == x)
  
  gam_fit <- bam(
    SATFor5 ~
      goalDifferential +
      s(secondsRemaining, by = goalDifferential, k = 10) +
      zoneCode * manDifferential +
      isOT +
      isEmptyNetFor +
      isEmptyNetAgainst +
      logAvailableSATWindow,
    family = binomial(),
    method = "fREML",
    discrete = TRUE,
    data = sat_train
  )
  
  logit_fit <- glm(
    SATFor5 ~
      goalDifferential * ns(secondsRemaining, df = 7) +
      zoneCode * manDifferential +
      isOT +
      isEmptyNetFor +
      isEmptyNetAgainst +
      logAvailableSATWindow,
    family = binomial(),
    data = sat_train
  )
  
  gam_pred <- predict(
    gam_fit,
    newdata = sat_test,
    type = "response"
  )
  
  logit_pred <- predict(
    logit_fit,
    newdata = sat_test,
    type = "response"
  )
  
  stopifnot(
    !anyNA(gam_pred),
    !anyNA(logit_pred)
  )
  
  tibble(
    gam_pred = gam_pred,
    logit_pred = logit_pred,
    test_actual = sat_test$SATFor5,
    test_fold = x
  )
}

sat_preds <- map(
  1:N_FOLDS,
  sat_cv
) |>
  list_rbind()

sat_cv_by_fold <- sat_preds |>
  group_by(test_fold) |>
  summarise(
    gam_auc = as.numeric(
      auc(
        roc(
          response = test_actual,
          predictor = gam_pred,
          quiet = TRUE
        )
      )
    ),
    
    logit_auc = as.numeric(
      auc(
        roc(
          response = test_actual,
          predictor = logit_pred,
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

sat_cv_by_fold

sat_cv_summary <- sat_cv_by_fold |>
  summarise(
    mean_gam_auc = mean(gam_auc),
    mean_logit_auc = mean(logit_auc),
    
    mean_gam_brier = mean(gam_brier),
    mean_logit_brier = mean(logit_brier),
    
    sd_gam_auc = sd(gam_auc),
    sd_logit_auc = sd(logit_auc),
    
    sd_gam_brier = sd(gam_brier),
    sd_logit_brier = sd(logit_brier)
  )

sat_cv_summary

sat_cv_overall <- sat_preds |>
  summarise(
    gam_auc = as.numeric(
      auc(
        roc(
          response = test_actual,
          predictor = gam_pred,
          quiet = TRUE
        )
      )
    ),
    
    logit_auc = as.numeric(
      auc(
        roc(
          response = test_actual,
          predictor = logit_pred,
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
    )
  )

sat_cv_overall

