# CMSAC-2026-Hockey-Project
# Leverage Score Steps
1. Run the ‘pbp.R’ script to scrape NHL play-by-play into the console and clean the data.
2. Run the ‘schedule.R’ script to scrape the NHL season schedule into the console and clean the data.
3. Run the ‘players.R’ script to scrape NHL player bios into the console and clean the data.
4. Run the ‘gameLogs.R’ script to scrape NHL player career totals into the console and clean the data.
5. Run the ‘rosters.R’ script to scrape the NHL rosters by game into the console and clean the data.
6. Run the ‘variables.R’ script to build a clean, mirrored event-level faceoff dataset for modeling.
7. Run the ‘leverage score.R’ script to run the models and calculate a leverage score for every faceoff.
8. Run the ‘wp model.R’ and ‘xG model.R’ scripts to compare modeling techniques for goal value and goal likelihood within 5 seconds of the faceoff.
9. Run the ‘leverage score analysis.R’ script to split the leverage scores into natural clusters and create data visualizations.
# Shiny App Steps
1. Save the 'pbp_cleaned.rds' file at the end of the 'pbp.R' script.
2. Run the 'Hockey_shiny_appTH.R' to create the NHL Faceoff Simulator app.
