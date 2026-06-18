#Capstone project
#Pittsburgh Penguins Hockey Face-off project
#strength state: power plays etc
#score state: tied games or big score difference
#look at OT?

require(nhlscraper)
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
game_pbp = gc_play_by_play(game = 2023030417)

