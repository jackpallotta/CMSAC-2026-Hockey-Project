# CMSAC-2026-Hockey-Project
Raw FOW% treats every faceoff equally, but does not adequately capture players that consistently win faceoffs that matter most. From watching game footage, we know specific game states can lead to future scoring opportunity directly following a faceoff. We call those faceoffs "high leverage," where winning or losing the faceoff could greatly impact the game’s outcome. Our research focuses on two main questions:
1. Are certain players better in high leverage situations? 
2. Can we quantify faceoff value?
# Leverage Score Steps
1. Run the 'model EDA plots.R' script to view exploratory data analyiss (EDA).
2. Run the ‘pbp.R’ script to scrape NHL play-by-play into the console and clean the data.
3. Run the ‘schedule.R’ script to scrape the NHL season schedule into the console and clean the data.
3. Run the ‘players.R’ script to scrape NHL player bios into the console and clean the data.
4. Run the ‘gameLogs.R’ script to scrape NHL player career totals into the console and clean the data.
5. Run the ‘rosters.R’ script to scrape the NHL rosters by game into the console and clean the data.
6. Run the ‘variables.R’ script to build a clean, mirrored event-level faceoff dataset for modeling.
7. Run the ‘leverage score & faceoff win value.R’ script to run the models. These models calculate a leverage score and the expected win value for every faceoff.
8. Run the ‘goal value model validation.R’, ‘goal likelihood model validation.R’, and 'faceoff win value model validation.R' scripts to compare modeling techniques for goal value, goal likelihood, and faceoff win value within 5 seconds of a faceoff.
9. Run the ‘leverage score analysis.R’ script to split the leverage scores into natural clusters and create data visualizations.
# Shiny App Steps
1. Open the shiny folder
2. Run the various RDS files in the folder
3. Run the 'app.R' script to run the app locally or go to this website: https://nhlfaceoffsimulator.shinyapps.io/shiny/
4. To view the model accuracy and model used in the app go to and read the 'Capstone_Hockey_projTH.R' script
