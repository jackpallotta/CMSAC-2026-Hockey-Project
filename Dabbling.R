## Dabbling with Model Building :)
## July 1st 2026
## By Chloe Guagliano

## (just for fun!)

## new beautiful personal access token:
## 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


library(nhlscraper)
library(tidyverse)
library(dplyr)
library(purrr)
library(gtsummary)
library(broom)

## filtering pbp so there's only goals
goalEvents = pbp |>
  filter(eventTypeDescKey == 'goal') |>
  mutate(secondsRemainingInGame = 3600 - secondsElapsedInGame) |>
  mutate(minutesElapsedInGame = secondsElapsedInGame / 60) |>
  select(gameId, periodType, secondsElapsedInGame, minutesElapsedInGame, secondsRemainingInGame, eventOwnerTeamId, eventTeamVenue, scoreState) |>
  group_by(gameId) |>
  mutate(gameWinner = names(which.max(table(eventTeamVenue)))) |>
  mutate(scoringTeamWin = if_else(gameWinner == eventTeamVenue,
                                  1,
                                  0)) |>
  ungroup()

nonRegulationGoalEvents = goalEvents |>
  filter(periodType %in% c('OT','SO'))

gamesWithOvertime = as.list(unique(nonRegulationGoalEvents$gameId))

goalEvents = goalEvents |>
  filter(! (gameId %in% gamesWithOvertime)) 

goalEvents = goalEvents |>
  mutate(scoreState = case_when(scoreState >= 4 ~ 4,
                                scoreState <= -4 ~ -4,
                                scoreState > -4 & scoreState < 4 ~ scoreState)) |>
  mutate(scoreStateGroup = as.character(scoreState)) |>
  mutate(scoreStateGroup = case_when(scoreStateGroup == '4' ~ 'GE_4',
                   scoreStateGroup == '-4' ~ 'LE_-4',
                   !(scoreStateGroup %in% c('-4','4')) ~ scoreStateGroup)) |>
  mutate(scoreStateGroup = as.factor(scoreStateGroup)) |>
  mutate(scoreStateGroup = relevel(scoreStateGroup, ref = "0"))


######## TESTING OTHER VARIATIONS OF THE BASE MODEL
set.seed(91)
N_FOLDS <- 5 # how do you choose number of folds for cross-validation?
goalEvents = goalEvents |>
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

table(goalEvents$fold)

goalEvents_cv = function(x) {
  
  # get test and training data:
  test_data = goalEvents |> filter(fold == 5)                     
  train_data = goalEvents |> filter(fold != 5)
  
  ## inital basic model (scoreState, secondsElapsed, no interaction)
  basic_logit = glm(scoringTeamWin ~ secondsElapsedInGame + scoreStateGroup, 
                    data = train_data, family = binomial)
  
  ## initial basic model with minutesElapsed instead of seconds
  basic_logitMinutes = glm(scoringTeamWin ~ minutesElapsedInGame + scoreStateGroup,
                           data = train_data, family = binomial)
  
  ## base model B (scoreState, timeRemaining, no interaction)
  base_logit_b = glm(scoringTeamWin ~ secondsRemainingInGame + scoreStateGroup, 
                     data = train_data, family = binomial)
  
  ## base model C (scoreState, secondsElapsed, w/ interaction)
  base_logit_c = glm(scoringTeamWin ~ secondsElapsedInGame * scoreStateGroup, 
                     data = train_data, family = binomial)
  
  ## base model D (scoreState, secondsRemaining w/ interaction)
  base_logit_d = glm(scoringTeamWin ~ secondsRemainingInGame * scoreStateGroup,
                     data = train_data, family = binomial)
  
  # return test results:
  out = list(
    predictions = tibble(
    basicLogit_pred = predict(basic_logit, newdata = test_data, type = 'response'),
    basicLogitMinutes_pred = predict(basic_logitMinutes, newdata = test_data, type = 'response'),
    baseLogitB_pred = predict(base_logit_b, newdata = test_data, type = 'response'),
    baseLogitC_pred = predict(base_logit_c, newdata = test_data, type = 'response'),
    baseLogitD_pred = predict(base_logit_d, newdata = test_data, type = 'response'),
    test_actual = test_data$scoringTeamWin,
    test_fold = 5
  ),
  modelC = base_logit_c)
  return(out)
}


results = goalEvents_cv(goalEvents)
base_logit_c = results$modelC


results$predictions |>
  group_by(test_fold) |>
  summarise(
    basicModel_brier = mean((basicLogit_pred - test_actual)^2),
    basicModelMinutes_brier = mean((basicLogitMinutes_pred - test_actual)^2),
    baseModelB_brier = mean((baseLogitB_pred - test_actual)^2),
    baseModelC_brier = mean((baseLogitC_pred - test_actual)^2),
    baseModelD_brier = mean((baseLogitD_pred - test_actual)^2)
  )


##### DON'T DERIVE ANY ACC RESULTS FROM THIS UNTIL THE LOGIT MODEL IS 
##### TRAINED / TESTED BETTER -> RESULTS AREN'T RELIABLE YET


## prediction grid with feasible possibilities + predicting at each
## then deriving goal value by calculating Win Probability Growth
## (see notebook for full conceptual explanation)
prediction_grid = expand.grid(
  secondsElapsedInGame = seq(0, 3600, by = 60),
  scoreStateGroup = levels(goalEvents$scoreStateGroup)
)

prediction_grid$winProb = predict(
  base_logit_c,
  newdata = prediction_grid,
  type = "response"
)

scoreTransitions = tibble(
  scoreStateGroup = c(
    'LE_-4','-3','-2','-1','0','1','2','3','GE_4'),
  nextScoreStateGroup = c(
    '-3','-2','-1','0','1','2','3','GE_4','GE_4')
)

goalValues = prediction_grid |>
  left_join(scoreTransitions, by = 'scoreStateGroup') |>
  left_join(
    prediction_grid |>
      transmute(
        secondsElapsedInGame,
        scoreStateGroup,
        winProb_afterGoal = winProb
      ),
    by = c(
      'secondsElapsedInGame',
      'nextScoreStateGroup' = 'scoreStateGroup'
    )
  ) |>
  mutate(goalValue = winProb_afterGoal - winProb) |>
  arrange(desc(goalValue))


## line graph: win probability over time by score state
## (see distance between lines at given timeElapsed)
prediction_grid |>
  ggplot(aes(x = secondsElapsedInGame, y = winProb, color = factor(scoreStateGroup),
             group = scoreStateGroup)) + 
  geom_point() + 
  geom_line() +
  labs(x = 'Seconds Elapsed In Game',
       y = 'Win Probability',
       color = 'scoreState',
       title = 'Win Probability Over Time by Score State')

## bar chart comparing average goalValue by score state
goalValues |>
  select(scoreStateGroup, nextScoreStateGroup, goalValue) |>
  group_by(scoreStateGroup) |>
  mutate(averageGoalValue = mean(goalValue)) |>
  distinct(scoreStateGroup, nextScoreStateGroup, averageGoalValue) |>
  arrange(desc(averageGoalValue)) |>
  ggplot(aes(x = fct_reorder(nextScoreStateGroup, desc(averageGoalValue)), y = averageGoalValue)) + 
  geom_col() + 
  labs(x = 'Score State Achieved',
       y = 'Average Goal Value',
       title = 'Average Goal Value by Score State Achieved')




