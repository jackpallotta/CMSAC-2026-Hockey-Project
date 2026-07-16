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
require(pROC)

theme_set(theme_bw())

faceoffs_cleaned = readRDS("faceoffsCleaned.rds")
faceoffData = readRDS("faceoffData.rds")

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
pbp_faceoffs = pbp_cleaned |>
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
  mutate(is_shot_atmpt = as.numeric(eventTypeDescKey == "shot-on-goal" | eventTypeDescKey == "missed-shot" | eventTypeDescKey == "goal")) |>
  mutate(faceoffDotCategory = case_when(xCoord == -69 & yCoord == 22 ~ '1',
                                        xCoord == -20 & yCoord == 22 ~ '2',
                                        xCoord == 20 & yCoord == 22 ~ '3',
                                        xCoord == 69 & yCoord == 22 ~ '4',
                                        xCoord == -69 & yCoord == -22 ~ '5',
                                        xCoord == -20 & yCoord == -22 ~ '6',
                                        xCoord == 20 & yCoord == -22 ~ '7',
                                        xCoord == 69 & yCoord == -22 ~ '8',
                                        xCoord == 0 & yCoord == 0 ~ '0')) |>
  mutate(scoreState = as.factor(case_when(scoreState >= 4 | scoreState <= -4 ~ '+/- 4 or greater',
                                scoreState %in% c(-3,3) ~ '+/- 3',
                                scoreState %in% c(-2,2) ~ '+/- 2',
                                scoreState %in% c(-1,1) ~ '+/- 1',
                                scoreState == 0 ~ '0'))) |>
  mutate(leftRight = as.factor(case_when(homeTeamDefendingSide == 'left' & eventTeamVenue == 'home'~
                                 if_else(faceoffDotCategory %in% c('1','2','3','4'),'L','R'),
                               homeTeamDefendingSide == 'left' & eventTeamVenue == 'away'~
                                 if_else(faceoffDotCategory %in% c('5','6','7','8'),'L','R'),
                               homeTeamDefendingSide == 'right' & eventTeamVenue == 'home'~
                                 if_else(faceoffDotCategory %in% c('5','6','7','8'),'L','R'),
                               homeTeamDefendingSide == 'right' & eventTeamVenue == 'away'~
                                 if_else(faceoffDotCategory %in% c('1','2','3','4'),'L','R')))) |>
  mutate(zoneCode = as.factor(if_else(zoneCode == 'N' & faceoffDotCategory != 'C',
                            case_when(homeTeamDefendingSide == 'left' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('2','6') ~ 
                                        'C-NZ',
                                      homeTeamDefendingSide == 'left' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('3','7') ~
                                        'F-NZ',
                                      homeTeamDefendingSide == 'left' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('2','6') ~
                                        'F-NZ',
                                      homeTeamDefendingSide == 'left' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('3','7') ~
                                        'C-NZ',
                                      homeTeamDefendingSide == 'right' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('2','6') ~
                                        'F-NZ',
                                      homeTeamDefendingSide == 'right' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('3','7') ~
                                        'C-NZ',
                                      homeTeamDefendingSide == 'right' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('2','6') ~
                                        'C-NZ',
                                      homeTeamDefendingSide == 'right' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('3','7') ~
                                        'F-NZ'),zoneCode))) |>
  mutate(zoneCode = as.factor(if_else(zoneCode == 'N','C', zoneCode)))|>
  mutate(situationDescriptor = as.factor(case_when(is.na(zoneCode) | is.na(leftRight) ~ NA_character_,
                                        TRUE ~ paste(zoneCode, leftRight, sep = ' ')))) |>
  mutate(situationLinkage = as.factor(case_when(situationDescriptor %in% c('D L', 'O R') ~
                                        'A',
                                      situationDescriptor %in% c('D R', 'O L') ~
                                        'B',
                                      situationDescriptor %in% c('F-NZ R', 'C-NZ L') ~
                                        'C',
                                      situationDescriptor %in% c('F-NZ L', 'C-NZ R') ~
                                        'D',
                                      situationDescriptor == 'C R' ~
                                        'E'))) |>
  mutate(periodNumber = as.factor(periodNumber)) |>
  mutate(isEmptyNetFor = as.factor(isEmptyNetFor)) |>
  mutate(isEmptyNetAgainst = as.factor(isEmptyNetAgainst)) |>
  mutate(eventTeamVenue = as.factor(eventTeamVenue)) |>
  filter(periodType == "REG")

events_after_faceoff2 = events_after_faceoff2 |> #redoing it so that the shiny app wont stop when a shot is entered in the sequence
  group_by(faceoff_row) |>
  mutate(future_shot = sapply(seq_len(n()), function(i) {
    any(eventTypeDescKey[(i + 1):n()] %in% c("shot-on-goal","missed-shot","goal"))})
  ) |>
  ungroup()

events_model = events_after_faceoff2 |> #stopping it if a goal is entered in the sequence
  filter(eventTypeDescKey != "goal")


saveRDS(events_after_faceoff2, "events_after_faceoffs.rds")

#testing and training new model
set.seed(071526)

faceoff_ids = unique(events_model$faceoff_row)

train_ids = sample(
  faceoff_ids,
  size = 0.75 * length(faceoff_ids))

train_data = events_model |>
  filter(faceoff_row %in% train_ids)

test_data = events_model |>
  filter(!faceoff_row %in% train_ids)

#looking at only interaction terms for score state and strength state because 
#you have to enter those variables into the app before hitting calculate probability
test.mod = bam(future_shot~ eventTypeDescKey + s(xCoord,yCoord, k = 30) + 
                 s(distance, k = 20) + s(secondsElapsedInGame) + strengthState:scoreState +
                 isEmptyNetFor + isEmptyNetAgainst + s(angle, k = 15) , 
               data = train_data, family = binomial(link = logit), method = "fREML", discrete = TRUE)
summary(test.mod)

test_data$pred_prob = predict(test.mod, newdata = test_data, type = "response")

test_data$pred_class = ifelse(test_data$pred_prob > 0.5, 1, 0)

table(Actual = test_data$future_shot, Predicted = test_data$pred_class)

roc_obj4 = roc(test_data$future_shot, test_data$pred_prob)

auc(roc_obj4) #0.8596



#Following Models and code looking at model accuracy (ROC and AUC) all for the question do Face-offs matter

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

gam.mod2 = gam(is_shot_atmpt~s(situationDescriptor, bs = "re") + s(xCoordNorm) + s(distance),
               data = events_after_faceoff2, family = binomial(link=logit), method = "REML")
summary(gam.mod2)

gam.mod3 = gam(is_shot_atmpt ~  s(zoneCode, bs = "re") + s(xCoordNorm) + s(yCoordNorm) + s(distance) + s(periodNumber, bs = "re"),
               data = events_after_faceoff2, family = binomial(link = logit), method = "REML")
summary(gam.mod3)#$s.table

gam.mod4 = gam(is_shot_atmpt ~  s(leftRight,zoneCode, bs = "re") + s(xCoordNorm) + s(yCoordNorm) + s(distance) + 
                 s(periodNumber, bs = "re") + s(angle) + s(isEmptyNetFor,isEmptyNetAgainst, bs = "re"),
               data = events_after_faceoff2, family = binomial(link = logit), method = "REML")
summary(gam.mod4)

gam.mod5 = gam(is_shot_atmpt ~  s(leftRight,zoneCode, bs = "re") + s(xCoord) + s(yCoord) + 
                 s(distance) +s(secondsElapsedInGame)  + s(angle) + strengthState +
                 s(isEmptyNetFor,isEmptyNetAgainst, bs = "re"),
               data = events_after_faceoff2, family = binomial(link = logit), method = "REML")
summary(gam.mod5)

gam.mod6 = gam(is_shot_atmpt ~ leftRight + zoneCode+  s(xCoord, yCoord) +
                 s(distance) +s(secondsElapsedInGame) + s(angle) + strengthState +
                 scoreState + isEmptyNetFor + isEmptyNetAgainst ,
               data = events_after_faceoff2, family = binomial(link = logit), method = "REML")
summary(gam.mod6)


bam.mod = bam(is_shot_atmpt ~  s(xCoord,yCoord, k = 30) + 
                s(distance, k = 20) + s(secondsElapsedInGame) + strengthState*scoreState +
                isEmptyNetFor + isEmptyNetAgainst + s(angle, k = 15) , 
              data = events_after_faceoff2, family = binomial(link = logit), method = "fREML", discrete = TRUE)
summary(bam.mod)


bam.mod2 = bam(USATFor5 ~ zoneCode + coordinates + goalDifferential*manDifferential + 
                 s(secondsRemaining) , data = faceoffData, family = binomial(link = logit),
               method = "fREML", discrete = TRUE)
summary(bam.mod2)

bam.mod3 = bam(future_shot~ eventTypeDescKey + s(xCoord,yCoord, k = 30) + 
                 s(distance, k = 20) + s(secondsElapsedInGame) + strengthState*scoreState +
                 isEmptyNetFor + isEmptyNetAgainst + s(angle, k = 15) , 
               data = events_model, family = binomial(link = logit), method = "fREML", discrete = TRUE)
summary(bam.mod3)

#bam.mod2 = bam(is_shot_atmpt ~  s(xCoord,yCoord, k = 30) + 
#                s(distance, k = 20) + s(secondsElapsedInGame) + strengthState*scoreState +
#                isEmptyNetFor + isEmptyNetAgainst + s(angle, k = 15) , 
#              data = events_after_faceoff2, family = Tw(link = logit), method = "fREML", discrete = TRUE)
#summary(bam.mod2)

require(statmod)
require(tweedie)

tweedie.mod = glm(xG~ xCoord*yCoord + distance + angle + secondsElapsedInGame + 
                    strengthState*scoreState + isEmptyNetAgainst + isEmptyNetFor,
                  data = events_after_faceoff2, family = tweedie(var.power = 1.5, link.power = 0))
summary(tweedie.mod)



#looking at glmm models (Absolute trash)
glmm.mod = glmer(fo_success~ zoneCode + xCoordNorm + distance + (1| eventOwnerTeamId) + xG,
                 data = events_after_faceoff2, family = binomial(link=logit))
summary(glmm.mod)

require(bbmle)
#gam.mod2 is currently the best, overall it is looking like the best predictor variable might be fo_success
AICtab(mod3, mod4,gam.mod, gam.mod2, gam.mod3, gam.mod4, glmm.mod, base=TRUE, sort=TRUE)

#likelihood ratio test for mod3
Anova(mod3, type="II", test = "LR")
Anova(mod4, type = "II", test = "LR")

exp(confint(mod3)) #no negatives
exp(confint(mod4))

require(broom)
tidy(mod3, exponentiate = TRUE, conf.int = TRUE)
tidy(mod4, exponentiate = TRUE, conf.int = TRUE)

tidy(gam.mod4, exponentiate = TRUE, conf.int = TRUE)

#library(gtsummary)
#tbl_regression(mod3, exponentiate = TRUE)

mod3_pred_prob = predict(mod3, type = "response") 
#for every obs the pred prob of winning

mod4_pred_prob = predict(mod4, type = "response") 
#for every obs the pred prob of winning

gam.mod5_pred_prob = predict(gam.mod5, type = "response") 

tweedie.mod_pred_prob = predict(tweedie.mod,type = "response")

mod3_pred_class = ifelse(mod3_pred_prob > 0.5, "Win", "Loss") 
#labeling the predictions as a win or loss

mod4_pred_class = ifelse(mod4_pred_prob > 0.5, "Win", "Loss") 
#labeling the predictions as a win or loss

gam.mod5_pred_class = ifelse(gam.mod5_pred_prob > 0.5, "Win", "Loss") 

tweedie.mod_pred_class = ifelse(tweedie.mod_pred_prob > 0.5, "Win", "Loss")

mod3_pred_binary = ifelse(mod3_pred_prob > 0.5, 1, 0)

mod4_pred_binary = ifelse(mod4_pred_prob > 0.5, 1, 0)

gam.mod5_pred_binary = ifelse(gam.mod5_pred_prob > 0.5, 1, 0)

tweedie.mod_pred_binary = ifelse(tweedie.mod_pred_prob > 0.5, 1, 0)

#mean(mod3_pred_class != events_after_faceoff2$fo_success) #Not Working

prop.table(table(events_after_faceoff2$fo_success))
prop.table(table(events_after_faceoff2$is_shot_atmpt))

#Brier Score of 0.123
mean((mod3_pred_binary - mod3_pred_prob)^2)

#BRIER scorte extremly low of 0.0024
mean((mod4_pred_binary - mod4_pred_prob)^2)

#pretty solid BRIER Score of 0.096 for gam.mod4, brier score of 0.097 for gam.mod5
mean((gam.mod5_pred_binary - gam.mod5_pred_prob)^2)

mean((tweedie.mod_pred_binary - tweedie.mod_pred_prob)^2)

shot_results = events_after_faceoff2 |>
  select(is_shot_atmpt, leftRight,zoneCode, xCoord, yCoord,distance, 
         periodNumber, angle, isEmptyNetFor, isEmptyNetAgainst) |>
  drop_na() |>
  mutate(shot_prob = predict(gam.mod5, type = "response"),
         pred_decile2 = ntile(shot_prob, 10))

shot_calibration_check =  shot_results |>
  group_by(pred_decile2) |>
  summarize(
    predicted = mean(shot_prob),
    actual = mean(is_shot_atmpt),
    n = n(),
    .groups = "drop")

shot_calibration_check #only slightly off

roc_obj2 = roc(
  response = shot_results$is_shot_atmpt,
  predictor = shot_results$shot_prob,
  quiet = TRUE)

auc(roc_obj2)#AUC is 0.828



mean((shot_results$is_shot_atmpt - shot_results$shot_prob)^2)



#creating an ROC graph of gam.mod4
library(pROC)

shot_roc = tibble(threshold = c(roc_obj2$thresholds),
                  specificity = roc_obj2$specificities,
                  sensitivity = roc_obj2$sensitivities)#a goal is actually detected as a goal 

shot_roc |> 
  ggplot(aes(x = 1 - specificity , y = sensitivity)) + #1-specificity (false pos. rate)
  geom_path() +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed")


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

round(prop.table(table(events_after_faceoff2$is_shot_atmpt,
                       events_after_faceoff2$eventOwnerTeamId,
                       events_after_faceoff2$faceoffDotCategory), margin = c(2,3)), 3)

