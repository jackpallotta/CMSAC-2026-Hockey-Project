## Dabbling with Model Building :)
## July 1st 2026
## By Chloe Guagliano

## (just for fun!)

usethis::use_git_config(user.name = "chloeguagliano", 
                        user.email = "chloeguagliano@gmail.com")
usethis::create_github_token()


















## Let's get started!
library(nhlscraper)
library(tidyverse)
library(dplyr)
library(purrr)
library(gtsummary)
library(broom)

## filtering pbp so there's only goals
goalEvents = pbp |>
  filter(eventTypeDescKey == 'goal') |>
  select(gameId, periodType, secondsElapsedInGame, eventOwnerTeamId, eventTeamVenue, scoreState) |>
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
  filter(! (gameId %in% gamesWithOvertime)) |>

## fitting an initial log model (scoreState, secondsElapsedInGame, no interaction)
basic_logit = glm(scoringTeamWin ~ secondsElapsedInGame + scoreState, 
                  data = goalEvents, family = binomial)

tidy(basic_logit, conf.int = TRUE, exponentiate = TRUE) |>
  mutate(p.value = formatC(p.value, format = "f", digits = 3))

tbl_regression(basic_logit, exponentiate = TRUE)


##### DON'T DERIVE ANY ACC RESULTS FROM THIS UNTIL THE LOGIT MODEL IS 
##### TRAINED / TESTED BETTER -> RESULTS AREN'T RELIABLE YET


## prediction grid with feasible possibilities + predicting at each
## then deriving goal value by calculating Win Probability Growth
## (see notebook for full conceptual explanation)
prediction_grid = expand.grid(
  secondsElapsedInGame = seq(0, 3600, by = 60),
  scoreState = -5:5
)

prediction_grid$winProb = predict(
  basic_logit,
  newdata = prediction_grid,
  type = "response"
)

goalValues = prediction_grid |>
  mutate(scoreState = as.numeric(scoreState)) |>
  left_join(
    prediction_grid |>
      transmute(
        secondsElapsedInGame,
        scoreState = scoreState - 1,
        winProb_afterGoal = winProb
      ),
    by = c('secondsElapsedInGame', 'scoreState')
  ) |>
  mutate(goalValue = winProb_afterGoal - winProb)


## good plot to make conceptually; need to adjust base model operating
## behind the scenes here and undergo proper model testing -> hope is this will be 
## a good alternative to showing the table w/ goalVal arranged descending
prediction_grid |>
  ggplot(aes(x = secondsElapsedInGame, y = winProb, color = factor(scoreState),
             group = scoreState)) + 
  geom_point() + 
  geom_line()

######## TESTING OTHER VARIATIONS OF THE BASE MODEL
set.seed(91)
N_FOLDS <- 5 # how do you choose number of folds for cross-validation?
goalEvents = goalEvents |>
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

table(goalEvents$fold)

















######## (Model C and D -> interaction and timeRemaining changes)

## fitting base model C (scoreState, secondsElapsedInGame, no interaction)
base_logit_c = glm(scoringTeamWin ~ secondsElapsedInGame * scoreState, 
                  data = goalEvents, family = binomial)

tidy(base_logit_c, conf.int = TRUE, exponentiate = TRUE)
  # mutate(p.value = formatC(p.value, format = "f", digits = 3))
  


tbl_regression(base_logit_c, exponentiate = TRUE)



