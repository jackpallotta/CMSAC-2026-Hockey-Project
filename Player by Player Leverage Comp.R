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



## making comparative plots for Dylan Strome and Phillip Danault across the 
## variables: goal differential, time remaining, faceoff location & strength state

## goal is to show that there are differences / nuances that go beyond the FOW%
## and total number of faceoffs taken; not necessarily making a judgement about
## which player is better right now -> its a segment to bridge understanding


## faceoff success by location comparison
strome = situationalFaceoffs |>
  filter(winningPlayerId %in% c('8478440') | losingPlayerId %in% c('8478440'))

danault = situationalFaceoffs |>
  filter(winningPlayerId %in% c('8476479') | losingPlayerId %in% c('8476479'))

stromeWins = strome |>
  select(situationDescriptor, winningPlayerId, losingPlayerId) |>
  filter(winningPlayerId == '8478440') |>
  count(situationDescriptor) |>
  mutate(situationDescriptor = if_else(situationDescriptor == 'C right',
                 'Center',
                 situationDescriptor)) |>
  arrange(desc(n)) |>
  rename(faceoffWins = n)

stromeLosses = strome |>
  select(situationDescriptor, winningPlayerId, losingPlayerId) |>
  filter(losingPlayerId == '8478440') |>
  count(situationDescriptor) |>
  mutate(situationDescriptor = if_else(situationDescriptor == 'C right',
                                       'Center',
                                       situationDescriptor)) |>
  arrange(desc(n)) |>
  rename(faceoffLosses = n)
  
stromeBySituation = left_join(stromeWins, stromeLosses, by = 'situationDescriptor')

stromeBySituation = stromeBySituation |>
  mutate(stromeWinPct = faceoffWins / (faceoffWins + faceoffLosses),
         totalFaceoffs = faceoffWins + faceoffLosses) |>
  arrange(desc(stromeWinPct)) |>
  select(situationDescriptor, stromeWinPct)

stromeBySituation



danaultWins = danault |>
  select(situationDescriptor, winningPlayerId, losingPlayerId) |>
  filter(winningPlayerId == '8476479') |>
  count(situationDescriptor) |>
  mutate(situationDescriptor = if_else(situationDescriptor == 'C right',
                                       'Center',
                                       situationDescriptor)) |>
  arrange(desc(n)) |>
  rename(faceoffWins = n)

danaultLosses = danault |>
  select(situationDescriptor, winningPlayerId, losingPlayerId) |>
  filter(losingPlayerId == '8476479') |>
  count(situationDescriptor) |>
  mutate(situationDescriptor = if_else(situationDescriptor == 'C right',
                                       'Center',
                                       situationDescriptor)) |>
  arrange(desc(n)) |>
  rename(faceoffLosses = n)

danaultBySituation = left_join(danaultWins, danaultLosses, by = 'situationDescriptor')

danaultBySituation = danaultBySituation |>
  mutate(danaultWinPct = faceoffWins / (faceoffWins + faceoffLosses),
         totalFaceoffs = faceoffWins + faceoffLosses) |>
  arrange(desc(danaultWinPct)) |>
  select(situationDescriptor, danaultWinPct)

danaultBySituation



compBySituation = left_join(stromeBySituation, danaultBySituation, by = 'situationDescriptor')
compBySituation |>
  pivot_longer(
    cols = c(stromeWinPct, danaultWinPct),
    names_to = "player",
    values_to = "faceoffPct"
  ) |>
  mutate(player = recode(player,
                         stromeWinPct = "Strome",
                         danaultWinPct = "Danault")) |>
  ggplot(aes(x = situationDescriptor, y = faceoffPct, fill = player)) +
  geom_col(position = 'dodge') + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = 'SituationDescriptor', 
       y = 'Faceoff Win Percentage',
       fill = 'Player')



## faceoff success by strength state comparison plot
danaultStateWins = danault |>
  filter(winningPlayerId == '8476479') |>
  select(strengthState, winningPlayerId, losingPlayerId) |>
  count(strengthState) |>
  rename(faceoffWins = n)

danaultStateLosses = danault |>
  filter(losingPlayerId == '8476479') |>
  select(strengthState, winningPlayerId, losingPlayerId) |>
  count(strengthState) |>
  rename(faceoffLosses = n)

danaultByState = left_join(danaultStateWins, danaultStateLosses, by = 'strengthState')
danaultByState = danaultByState |>
  mutate(danaultFaceoffPct = faceoffWins / (faceoffWins + faceoffLosses),
         danaultTotalFaceoffs = faceoffWins + faceoffLosses) |>
  select(strengthState, danaultFaceoffPct)

stromeStateWins = strome |>
  filter(winningPlayerId == '8478440') |>
  select(strengthState, winningPlayerId, losingPlayerId) |>
  count(strengthState) |>
  rename(faceoffWins = n)

stromeStateLosses = strome |>
  filter(losingPlayerId == '8478440') |>
  select(strengthState, winningPlayerId, losingPlayerId) |>
  count(strengthState) |>
  rename(faceoffLosses = n)

stromeByState = left_join(stromeStateWins, stromeStateLosses, by = 'strengthState')
stromeByState = stromeByState |>
  mutate(stromeFaceoffPct = faceoffWins / (faceoffWins + faceoffLosses),
         stromeTotalFaceoffs = faceoffWins + faceoffLosses) |>
  select(strengthState, stromeFaceoffPct)









danaultStateWins
  

  
  
  
  


