#Capstone project
#Pittsburgh Penguins Hockey Face-off project
#strength state: power plays etc
#score state: tied games or big score difference
#look at OT?

require(nhlscraper)
require(tidyverse)
require(car)
require(mgcv)
require(lme4)

theme_set(theme_bw())

ESPN_games_20242025 = espn_games(season = 20242025)
head(ESPN_games_20242025)

#provides the 2024-2025 pittsburgh roster with face off prct win
Pitt_roster2425 = roster_statistics(
  team = "PIT",
  season = 20242025,
  game_type = 2,
  position = "skaters") |>
  select( playerFirstName, playerLastName, faceoffWinPctg, avgShiftsPerGame, positionCode)

view(Pitt_roster2425)

#provides the pittsburgh schedule for the 2024-2025 season
Pitt_schedule = team_season_schedule(team = "PIT", season = 20242025) |>
  filter(gameTypeId > 1) |>
  select(gameId, seasonId, gameTypeId,  awayTeamScore,homeTeamScore,
         awayTeamTriCode,awayTeamCommonName, homeTeamTriCode, homeTeamCommonName)
view(Pitt_schedule)

#creating table of nhl playoff situation 
#shows series situation and which team wins etc
fran_playoff_sit = franchise_playoff_situational_results()
view(fran_playoff_sit)

#filtering games for pittsburgh for the 20232024 season and 20242025 season
pitt_games = games() |>
  filter(homeTeamId == 5 | visitingTeamId ==5) |>
  filter(seasonId == 20242025 | seasonId == 20232024)

#nhl rink drawing (goof for EDA)
nhl_rink = draw_NHL_rink()

#EDA ideas
#distribution of where and when face-offs are taken during a game

#x-axis period of game
#y-axis quantity of face-off
#potential bar chart with zones


#gives game play by play data, useful for face-off information, 
#time in the period, and zone code


#trial data visualization for EDA
#bar chart of face-offs during the periods with zone codes
#game code is from 2023
game_pbp = gc_play_by_play(game = 2023030417) |>
  dplyr::select(periodNumber, secondsElapsedInGame, secondsElapsedInPeriod, eventOwnerTeamId,
         eventTypeDescKey, zoneCode, xCoordNorm, yCoordNorm, winningPlayerId,
         losingPlayerId)

str(game_pbp)
#barchart of face-offs with zone code and period number
game_pbp |>
  mutate(periodNumber = as.factor(periodNumber)) |>
  mutate(zoneCode = as.factor(zoneCode)) |>
  filter(eventTypeDescKey == "faceoff") |>
  ggplot(aes(x = zoneCode, fill = periodNumber)) +
  geom_bar() + theme_bw()

#pulling all instances of face-offs in the 2024-2025 season
season_pbp = gc_play_by_plays(season = 20242025)|>
  dplyr::select(gameId, gameTypeId, periodNumber, secondsElapsedInGame, secondsElapsedInPeriod, 
                eventOwnerTeamId, eventTypeDescKey, strengthState, zoneCode, xCoordNorm, 
                yCoordNorm, winningPlayerId, losingPlayerId) |>
  filter(eventTypeDescKey == "faceoff")

#bar plot of all face-offs with zone-code and period number in the 20242025 season
#appears that the number of face-offs per zone in each period normalize (which i expected)
#may have to do a franchise by franchise visualization for the season
season_pbp |>
  mutate(periodNumber = as.factor(periodNumber)) |>
  ggplot(aes(x = zoneCode, fill = periodNumber)) +
  geom_bar() + theme_bw()

#look at face-offs and the amount of shots-on-goal right after a face-off
#potential conditional probability
#Binomial Bayes theorem: Success is shot on goal after face-off
#failure: no shot on goal after face-off

game_pbp2024 = gc_play_by_play(game = 2024020025)


#gives play by plays for the five seconds after a face-off if anything occurs for one game. 
#if nothing occurs after the faceoff then the faceoff wont show in the data frame

pbp2024 = gc_play_by_play(game = 2024020025)

pbp2024 = pbp2024 |>
  mutate(row_id = row_number())

faceoffs = pbp2024 |>
  filter(eventTypeDescKey == "faceoff")

events_after_faceoff = faceoffs |>
  select(faceoff_row = row_id,
         faceoff_time = secondsElapsedInGame) |>
  cross_join(pbp2024) |>
  filter(
    secondsElapsedInGame > faceoff_time, #does not show the initial face-off but 
    #can show if a face-off occurs within the five seconds of initial faceoff
    secondsElapsedInGame <= faceoff_time + 5
  ) |>
  mutate(is_shot = as.factor(ifelse(eventTypeDescKey == "shot-on-goal", 1, 0))) |>
  mutate(zoneCode = as.factor(zoneCode),
         strengthState = as.factor(strengthState),
         shotType = as.factor(shotType))

str(events_after_faceoff)

mod1 = glm(is_shot ~ zoneCode+strengthState, data = events_after_faceoff, family = binomial)
summary(mod1)

#full season play by play with five seconds after a face-off
season_pbp2024 = gc_play_by_plays(season = 20242025) |>
  mutate(row_id = row_number())

season_faceoffs = season_pbp2024 |>
  filter(eventTypeDescKey == "faceoff") |>
  select(faceoff_row = row_id,
         gameId,                 
         periodNumber,                 
         faceoff_time = secondsElapsedInGame) |>
  mutate(faceoff_end = faceoff_time + 5)

events_after_faceoff = season_faceoffs |>
  inner_join(
    season_pbp2024,
    by = join_by(
      gameId == gameId,
      periodNumber == periodNumber,
      faceoff_time < secondsElapsedInGame,   
      faceoff_end >= secondsElapsedInGame    
    )
  ) |>
  mutate(is_shot = as.factor(ifelse(eventTypeDescKey == "shot-on-goal", 1, 0))) |>
  mutate(zoneCode = as.factor(zoneCode),
         strengthState = as.factor(strengthState),
         shotType = as.factor(shotType))

events_after_faceoff |>
  ggplot(aes(x = zoneCode, fill = is_shot)) +
  geom_bar()

#Play by play Official----
#pbp pulled from Jack
#pulls the first five seconds after a face-off
pbp_faceoffs = pbp |>
  mutate(row_id = row_number()) |>
  filter(eventTypeDescKey == "faceoff") |>
  select(faceoff_row = row_id,
         gameId,                 
         periodNumber,                 
         faceoff_time = secondsElapsedInGame) |>
  mutate(faceoff_end = faceoff_time + 5)

events_after_faceoff2 = pbp_faceoffs |>
  inner_join(
    pbp,
    by = join_by(
      gameId == gameId,
      periodNumber == periodNumber,
      faceoff_time < secondsElapsedInGame,   
      faceoff_end >= secondsElapsedInGame)) |>
  mutate(fo_success = as.factor(ifelse(eventTypeDescKey == "shot-on-goal" | eventTypeDescKey =="goal", 1, 0))) |>
  mutate(is_shot_atmpt = as.numeric(eventTypeDescKey == "shot-on-goal" | eventTypeDescKey == "missed-shot" | eventTypeDescKey == "goal"))
#mutate shot attempts looking at missed, shots on goal, and goal

events_after_faceoff2 = events_after_faceoff2 |> mutate(zoneCode = fct_relevel(zoneCode, "N"))

#Trying to answer do Face-offs matter?
mod2 = glm(fo_success ~ xG + zoneCode + xCoordNorm + yCoordNorm + scoreState, 
           data = events_after_faceoff2, family = binomial)
summary(mod2)

#fairly significant model may add on some additional variables later
mod3 = glm(fo_success~xG*zoneCode + xCoordNorm + distance, 
           data = events_after_faceoff2, family = binomial)
summary(mod3)

#when xG is used as a predictor the model will fail to converge when using goal in shot atmpts
#taking out xG puts the model back to where it was prior to putting goals in variable shot atmpts
mod4 = glm(is_shot_atmpt ~  zoneCode +xCoordNorm + distance,
           data = events_after_faceoff2, family = binomial)
summary(mod4)

#looking at GAM models
require(mgcv)
events_after_faceoff2$is_shot_atmpt = as.factor(events_after_faceoff2$is_shot_atmpt)
events_after_faceoff2$zoneCode = as.factor(events_after_faceoff2$zoneCode)
#when putting in goals as a success for the variable "is_shot_attmpt" 
#can not use xG as a predictor
#look at just the `is_shot_atmpt` variable since it has an overall higher adj r^2
gam.mod = gam(is_shot_atmpt~ s(zoneCode, bs = "re") + s(xCoordNorm) + s(distance), 
           data = events_after_faceoff2, family = binomial(link = logit) , method = "REML")
summary(gam.mod)

gam.mod2 = gam(fo_success~s(zoneCode, bs = "re") + s(xCoordNorm) + s(distance) + s(xG),
               data = events_after_faceoff2, family = binomial(link=logit), method = "REML")
summary(gam.mod2)

gam.mod3 = gam(is_shot_atmpt ~ s(zoneCode, bs = "re") + s(xCoordNorm) + s(yCoordNorm) + s(distance) + s(periodNumber, bs = "re"),
               data = events_after_faceoff2, family = binomial(link = logit), method = "REML")
summary(gam.mod3)$s.table

#looking at glmm models (Absolute trash)
glmm.mod = glmer(fo_success~ zoneCode + xCoordNorm + distance + (1| eventOwnerTeamId) + xG,
                 data = events_after_faceoff2, family = binomial(link=logit))
summary(glmm.mod)

require(bbmle)
#gam.mod2 is currently the best, overall it is looking like the best predictor variable might be fo_success
AICtab(mod3, mod4,gam.mod, gam.mod2, gam.mod3, glmm.mod, base=TRUE, sort=TRUE)

#likelihood ratio test for mod3
Anova(mod3, type="II", test = "LR")
Anova(mod4, type = "II", test = "LR")

exp(confint(mod3)) #no negatives
exp(confint(mod4))

require(broom)
tidy(mod3, exponentiate = TRUE, conf.int = TRUE)
tidy(mod4, exponentiate = TRUE, conf.int = TRUE)

#library(gtsummary)
#tbl_regression(mod3, exponentiate = TRUE)

mod3_pred_prob = predict(mod3, type = "response") 
#for every obs the pred prob of winning

mod4_pred_prob = predict(mod4, type = "response") 
#for every obs the pred prob of winning

mod3_pred_class = ifelse(mod3_pred_prob > 0.5, "Win", "Loss") 
#labeling the predictions as a win or loss

mod4_pred_class = ifelse(mod4_pred_prob > 0.5, "Win", "Loss") 
#labeling the predictions as a win or loss

mod3_pred_binary = ifelse(mod3_pred_prob > 0.5, 1, 0)

mod4_pred_binary = ifelse(mod4_pred_prob > 0.5, 1, 0)

#mean(mod3_pred_class != events_after_faceoff2$fo_success) #Not Working

prop.table(table(events_after_faceoff2$fo_success))
prop.table(table(events_after_faceoff2$is_shot_atmpt))

#Brier Score of 0.123
mean((mod3_pred_binary - mod3_pred_prob)^2)

#BRIER scorte extremly low of 0.0024
mean((mod4_pred_binary - mod4_pred_prob)^2)


#attempt at checking the ROC of mod4
#library(pROC)
#want the area under the roc curve to be as large as possible
#filtered = events_after_faceoff2 |> 
#  filter(!is.na(xG), !is.na(zoneCode), !is.na(xCoordNorm), !is.na(distance)) |>
#  mutate(pred_prob = predict(mod4, type = "response"))
#mod4_roc = roc(response = filtered$result, predicted = filtered$pred_prob)
# str(cricket_roc)
#mod4_roc$auc


#attempt at drawing shots on goal and goals on nhl rink
rink = geom_hockey(league = "NHL")
shot_data = events_after_faceoff2[(events_after_faceoff2$eventTypeDescKey == "shot-on-goal") |
                                    (events_after_faceoff2$eventTypeDescKey== "goal"),]

rink + 
  geom_point(data = shot_data, aes(xCoord, yCoord), alpha = 0.5)

events_after_faceoff2 |>
  ggplot(aes(x = distance, y = xG,
             color = as.factor(fo_success))) +
  geom_point(alpha = 0.2)


barplot1 = events_after_faceoff2 |>
  filter(eventOwnerTeamId == 5) |>
  ggplot(aes(x = zoneCode, fill = as.factor(is_shot_atmpt))) +
  geom_bar() +
  labs(title = "Team 5 (PIT)")
barplot1

barplot2 = events_after_faceoff2 |>
  filter(eventOwnerTeamId == 12) |>
  ggplot(aes(x = zoneCode, fill = as.factor(is_shot_atmpt))) +
  geom_bar() +
  labs(fill = "Is Shot Attempt", title = "Team 12 (CAR)") +
  theme(legend.position = "none")
barplot2

barplot3 = events_after_faceoff2 |>
  filter(eventOwnerTeamId == 16) |>
  ggplot(aes(x = zoneCode, fill = as.factor(is_shot_atmpt))) +
  geom_bar() +
  labs(fill = "Is Shot Attempt", title = "Team 16 (CHI)") 
barplot3

require(patchwork)
barplot2 + barplot3


barplot4 = events_after_faceoff2 |>
  filter(eventOwnerTeamId == 12 | eventOwnerTeamId == 16 | eventOwnerTeamId == 5, !is.na(zoneCode)) |> 
  mutate(teams = case_when(eventOwnerTeamId == 12 ~ "CAR",
                           eventOwnerTeamId == 16 ~ "CHI",
                           eventOwnerTeamId == 5 ~ "PIT")) |>
  ggplot(aes(x = zoneCode, fill = as.factor(is_shot_atmpt))) +
  geom_bar() +
  facet_wrap(~ teams) +
  labs(fill = "Is Shot Attempt", title = "Shot Attempts between three NHL Teams") +
  theme(legend.position = "bottom")
barplot4

barplot5 = events_after_faceoff2 |>
  filter(eventOwnerTeamId == 12 | eventOwnerTeamId == 16 | eventOwnerTeamId == 5, !is.na(zoneCode)) |> 
  mutate(teams = case_when(eventOwnerTeamId == 12 ~ "CAR",
                           eventOwnerTeamId == 16 ~ "CHI",
                           eventOwnerTeamId == 5 ~ "PIT")) |>
  ggplot(aes(x = zoneCode, fill = as.factor(fo_success))) +
  geom_bar() +
  facet_wrap(~ teams) +
  labs(fill = "Is face-off Success", title = "Face-off Successes between three NHL Teams") +
  theme(legend.position = "bottom")
barplot5

#proportion table for shot attempts separated by teams
round(prop.table(table(events_after_faceoff2$is_shot_atmpt,
      events_after_faceoff2$eventOwnerTeamId,
      events_after_faceoff2$zoneCode), margin = c(2,3)), 3)

table(events_after_faceoff2$eventOwnerTeamId)

