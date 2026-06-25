## Chloe Starter Script

library(nhlscraper)
library(tidyverse)
library(ggplot2)
library(gt)

# critical endpoints for compiling dataset
# nhl_rink = draw_NHL_rink()
# pbp = gc_play_by_play(game = 2023030417)
# shiftchart = shift_chart(game = 2023030417)
# player_summary(player = 8478402)$shootsCatches
# gc_summary(game = 2023030417)

# which players are the best at taking faceoffs ->
# if all faceoffs were weighted equally / seen as equally important

faceoffInfo = skater_season_report(
  season = 20252026,
  game_type = 2,
  category = 'faceoffpercentages'
)

faceoffInfo = faceoffInfo |>
  select(playerId, faceoffWinPct, skaterFullName, totalFaceoffs, positionCode) |>
  filter(positionCode %in% c('C')) |>
  filter(totalFaceoffs > 500) |>
  arrange(desc(totalFaceoffs))

# kmeans clustering to pull formal cluster assignments for the 4 anticipated quadrants
clean_faceoffInfo = faceoffInfo |>
  mutate(
    std_totalFaceoffs = as.numeric(scale(totalFaceoffs, center = TRUE, scale = TRUE)),
    std_faceoffWinPct = as.numeric(scale(faceoffWinPct, center = TRUE, scale = TRUE))
  )

set.seed(91)
std_kmeans = clean_faceoffInfo |> 
  select(std_totalFaceoffs, std_faceoffWinPct) |> 
  kmeans(algorithm = "Lloyd", centers = 4, nstart = 30, iter.max = 20)

clean_faceoffInfo |>
  mutate(
    clusters = as.factor(std_kmeans$cluster)
  ) |>
  ggplot(aes(x = totalFaceoffs, y = faceoffWinPct,
             color = clusters)) +
  geom_hline(yintercept = mean(faceoffInfo$faceoffWinPct), 
             color = 'red', 
             linetype = 'dashed', 
             size = 0.5) + 
  geom_vline(xintercept = mean(faceoffInfo$totalFaceoffs),
             color = 'red',
             linetype = 'dashed',
             size = 0.5) +
  geom_point(size = 2) + 
  theme_minimal() +
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") + 
  labs(x = 'Faceoffs Taken',
       y = 'Faceoff Win Percentage',
       color = 'Clusters')

colnames(clean_faceoffInfo)

# looking at the players in Cluster 3 (proven, successful faceoff takers)
clean_faceoffInfo |>
  select(playerId, skaterFullName, faceoffWinPct, totalFaceoffs) |>
  mutate(clusters = as.factor(std_kmeans$cluster)) |>
  filter(clusters == 3) |>
  arrange(desc(faceoffWinPct)) |>
  gt()

# Sean Couturier (8476461) and Kevin Stenlund (8478831)
# COME BACK HERE LATER :)


## looking at external conditions for a faceoff (20252026 season)
## link faceoff dots

dabble = gc_play_by_play(game = 2023030417)

dabbleFaceoffs = dabble |>
  filter(eventTypeDescKey == 'faceoff')

dabbleFaceoffs

## using Tess' events_after_faceoff2 (dataset of events that fall into our 5-sec window)
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
  mutate(is_shot = as.factor(ifelse(eventTypeDescKey == "shot-on-goal", 1, 0) | 
                               ifelse(eventTypeDescKey == "goal", 1, 0)))


# should delayed penalty be in here??
stoppageEventNames = c('faceoff','stoppage','period-end','penalty','game-end')

# most common events that occur in our time threshold
cleaned_events = events_after_faceoff2 |>
  mutate(
    eventTypeDescKey = if_else(
      eventTypeDescKey %in% stoppageEventNames,
      'stoppageEvent',
      eventTypeDescKey
    ))
  
cleaned_events |>
  count(eventTypeDescKey) |>
  arrange(desc(n)) |>
  ggplot(aes(x = fct_reorder(eventTypeDescKey,desc(n)), y = n)) + 
  geom_col() + 
  theme_minimal() + 
  labs(x = 'Event Type',
       y = 'Number of Occurrences',
       title = 'Most Common Event Types Within 5 Seconds of Faceoff') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## faceoff dot linking attempt
faceoffs = pbp |>
  mutate(row_id = row_number()) |>
  filter(eventTypeDescKey == "faceoff") |>
  mutate(faceoff_row = row_id,
         faceoff_time = secondsElapsedInGame) |>
  mutate(faceoff_end = faceoff_time + 5)

faceoffs = faceoffs |>
  filter(eventTypeDescKey == 'faceoff') |>
  mutate(faceoffDotCategory = case_when(xCoord == -69 & yCoord == 22 ~ '1',
                                        xCoord == -20 & yCoord == 22 ~ '2',
                                        xCoord == 20 & yCoord == 22 ~ '3',
                                        xCoord == 69 & yCoord == 22 ~ '4',
                                        xCoord == -69 & yCoord == -22 ~ '5',
                                        xCoord == -20 & yCoord == -22 ~ '6',
                                        xCoord == 20 & yCoord == -22 ~ '7',
                                        xCoord == 69 & yCoord == -22 ~ '8',
                                        xCoord == 0 & yCoord == 0 ~ 'C'))


