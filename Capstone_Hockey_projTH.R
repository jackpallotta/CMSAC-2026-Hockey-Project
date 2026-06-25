#Capstone project
#Pittsburgh Penguins Hockey Face-off project
#strength state: power plays etc
#score state: tied games or big score difference
#look at OT?

require(nhlscraper)
require(tidyverse)
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
  mutate(fo_success = as.factor(ifelse(eventTypeDescKey == "shot-on-goal" | eventTypeDescKey =="goal", 1, 0)))

mod2 = glm(fo_success ~ xG + zoneCode + xCoordNorm + yCoordNorm + scoreState, 
           data = events_after_faceoff2, family = binomial)
summary(mod2)

mod3 = glm(fo_success~xG*zoneCode + xCoordNorm, 
           data = events_after_faceoff2, family = binomial)
summary(mod3)

geom_hockey(league = "NHL")

