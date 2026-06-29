## Player-by-Player Comparison 
## For: Are some players better in higher leverage situations?

library(nhlscraper)
library(tidyverse)
library(ggplot2)
library(gt)

## compiling the raw data needed to build out the dataset
faceoffInfo_2122 = skater_season_report(
  season = 20212022,
  game_type = 2,
  category = 'faceoffpercentages'
)

faceoffInfo_2223 = skater_season_report(
  season = 20222023,
  game_type = 2,
  category = 'faceoffpercentages'
)

faceoffInfo_2324 = skater_season_report(
  season = 20232024,
  game_type = 2,
  category = 'faceoffpercentages'
)

faceoffInfo_2425 = skater_season_report(
  season = 20242025,
  game_type = 2,
  category = 'faceoffpercentages'
)

faceoffInfo_2526 = skater_season_report(
  season = 20252026,
  game_type = 2,
  category = 'faceoffpercentages'
)

fullFaceoffInfo = rbind(faceoffInfo_2122,
                        faceoffInfo_2223,
                        faceoffInfo_2324,
                        faceoffInfo_2425,
                        faceoffInfo_2526)

## filtering / data wrangling to prepare the data for use
fullFaceoffInfo = fullFaceoffInfo |>
  filter(positionCode == 'C') |>
  select(playerId, faceoffWinPct, skaterFullName, totalFaceoffs) |>
  mutate(faceoffsWon = round(totalFaceoffs*faceoffWinPct, digits = 0)) |>
  select(playerId, skaterFullName, faceoffsWon, totalFaceoffs) |>
  group_by(playerId) |>
  mutate(overallTotalFaceoffs = sum(totalFaceoffs)) |>
  mutate(faceoffPercentage = sum(faceoffsWon) / overallTotalFaceoffs) |>
  ungroup() |>
  select(playerId, skaterFullName, faceoffPercentage, overallTotalFaceoffs) |>
  unique() |>
  filter(overallTotalFaceoffs >= 2500) |>
  arrange(desc(overallTotalFaceoffs))

## kmeans clustering to pull formal cluster assignments for the 4 anticipated quadrants
clean_faceoffInfo = fullFaceoffInfo |>
  mutate(
    std_totalFaceoffs = as.numeric(scale(overallTotalFaceoffs, center = TRUE, scale = TRUE)),
    std_faceoffWinPct = as.numeric(scale(faceoffPercentage, center = TRUE, scale = TRUE))
  )

set.seed(91)
std_kmeans = clean_faceoffInfo |> 
  select(std_totalFaceoffs, std_faceoffWinPct) |> 
  kmeans(algorithm = "Lloyd", centers = 4, nstart = 30, iter.max = 20)

## graphing the kmeans output
clean_faceoffInfo |>
  mutate(
    clusters = as.factor(std_kmeans$cluster)
  ) |>
  ggplot(aes(x = overallTotalFaceoffs, y = faceoffPercentage,
             color = clusters)) +
  geom_hline(yintercept = mean(fullFaceoffInfo$faceoffPercentage), 
             color = 'red', 
             linetype = 'dashed', 
             size = 0.5) + 
  geom_vline(xintercept = mean(fullFaceoffInfo$overallTotalFaceoffs),
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


# looking at the players in Cluster 4 (proven, successful faceoff takers)
clean_faceoffInfo |>
  select(playerId, skaterFullName, faceoffPercentage, overallTotalFaceoffs) |>
  mutate(clusters = as.factor(std_kmeans$cluster)) |>
  filter(clusters == 4) |>
  arrange(desc(faceoffPercentage)) |>
  gt()


