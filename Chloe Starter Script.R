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


  
  
  








