## Exploration of Strong Side vs Weak Side Props for Faceoff Takers
## In Different Leverage Situations

## September 26th 2026
## By Chloe Guagliano

library(tidyverse)
library(classInt)
library(gt)
library(gtExtras)
library(ggplot2)

## pulling main faceoff dataset and adding leverage bins
faceoffData = readRDS('faceoffData.rds')

set.seed(91)
leverageBreaks <- faceoffData |>
  distinct(eventId, .keep_all = TRUE) |>
  pull(leverage) |>
  classIntervals(n = 3, style = "jenks")

faceoffData = faceoffData |>
  mutate(
    bins = cut(leverage,
               breaks = leverageBreaks$brks,
               include.lowest = TRUE,
               labels = c("Low", "Medium", "High")
    )
  )

## finding strong side vs weak side props per leverage group
faceoffData |>
  mutate(strongSide = as.factor(strongSide)) |>
  select(strongSide, bins) |>
  filter(bins != 'Medium') |>
  count(strongSide, bins) |>
  arrange(bins) |>
  group_by(bins) |>
  mutate(
    prop = n / sum(n)
  ) |>
  ggplot(aes(x = bins, y = prop, fill = strongSide)) +
  geom_col() + 
  labs(x = 'Leverage Group',
       y = 'Proportion of Faceoff Takers',
       fill = 'Faceoff Side',
       caption = 'Data courtesy of the NHL') +
  scale_fill_manual(
    values = c("0" = "#79BAEC", "1" = "#4682B4"),
    labels = c("Weak Side", "Strong Side")
  ) +
  theme_light() +
  theme(legend.position = "bottom")

## comparing proportion of SS / WW / SW faceoffs for each leverage group
## (i.e. how many high-leverage faceoffs were strong side vs strong side in comparsion
##       with strong side vs weak side etc)

## column chart version (props indicated within the same bar for 3 bars / leverage group)
faceoffData |>
  select(eventId, strongSide, bins) |>
  group_by(eventId) |>
  mutate(sideSum = sum(strongSide)) |>
  ungroup() |>
  select(eventId, bins, sideSum) |>
  distinct() |>
  mutate(sideSum = as.factor(sideSum)) |>
  count(bins, sideSum) |>
  group_by(bins) |>
  mutate(prop = n/sum(n)) |>
  ggplot(aes(x = bins, y = prop, fill = sideSum)) +
  geom_col() +
  scale_fill_manual(
    values = c("0" = "#79BAEC", "1" = "#4682B4", "2" = "#123456"),
    labels = c("Weak Side x2", "Weak vs Strong Side", "Strong Side x2")
  ) + 
  labs(x = 'Leverage Group', y = 'Proportion of Faceoffs', fill = 'Faceoff Matchup Type')

## line chart version
faceoffData |>
  select(eventId, strongSide, bins) |>
  group_by(eventId) |>
  mutate(sideSum = sum(strongSide)) |>
  ungroup() |>
  select(eventId, bins, sideSum) |>
  distinct() |>
  mutate(sideSum = as.factor(sideSum)) |>
  count(bins, sideSum) |>
  group_by(bins) |>
  mutate(prop = n/sum(n)) |>
  ggplot(aes(x = bins, y = prop, color = sideSum, group = sideSum)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(
    values = c("0" = "#79BAEC", "1" = "#4682B4", "2" = "#123456"),
    labels = c("Weak Side x2", "Weak vs Strong Side", "Strong Side x2")
  ) + 
  labs(x = 'Leverage Group', y = 'Proportion of Faceoffs', color = 'Faceoff Matchup Type') +
  theme_light()

## causal inference: faceoff success in each strong side / weak side split per leverage
##                   group; look for trends within those splits and analyze players that way

## making considerations for both faceoffTaker and opponent (4 possible matchupTypes)
faceoffTakerPlusOpponent = faceoffData |>
  select(eventId, player, strongSide, faceoffWon, bins) |>
  group_by(eventId) |>
  mutate(sideSum = sum(strongSide)) |>
  ungroup() |>
  mutate(matchupType = case_when(sideSum == 0 ~ 'WW',
                                 sideSum == 2 ~ 'SS',
                                 sideSum == 1 & strongSide == 0 ~ 'WS',
                                 sideSum == 1 & strongSide == 1 ~ 'SW'
                                 )) |>
  select(player, matchupType, faceoffWon, bins) |>
  group_by(player) |>
  count(matchupType, faceoffWon, bins) |>
  ungroup() |>
  group_by(player, matchupType, bins) |>
  mutate(faceoffsTaken = sum(n)) |>
  ungroup() |>
  filter(faceoffWon == 1) |>
  mutate(situationalPct = n / faceoffsTaken) |>
  select(player, matchupType, bins, situationalPct, faceoffsTaken) |>
  group_by(player) |>
  mutate(highLevCount = sum(faceoffsTaken[bins == "High"])) |>
  filter(highLevCount >= 50) |>
  filter(bins != 'Medium') |>
  ungroup() |>
  select(matchupType, bins, situationalPct) |>
  group_by(matchupType, bins) |>
  summarise(meanPct = mean(situationalPct))




















## making considerations only for faceoffTaker (2 possible matchupTypes)
## (No conclusion drawn here really; realized we needed to go another direction!)
updated = faceoffData |>
  select(player, strongSide, faceoffWon, bins) |>
  count(player, strongSide, faceoffWon, bins) |>
  group_by(player, strongSide, bins) |>
  mutate(faceoffsTaken = sum(n)) |>
  ungroup() |>
  filter(faceoffWon == 1) |>
  mutate(situationalPct = n / faceoffsTaken) |>
  select(player, strongSide, bins, situationalPct, faceoffsTaken) |>
  group_by(player) |>
  mutate(highLevCount = sum(faceoffsTaken[bins == 'High'])) |>
  filter(highLevCount >= 50) |>
  ungroup() |>
  mutate(strongSide = case_when(strongSide == 1 ~ 'S',
                                strongSide == 0 ~ 'W')) |>
  filter(bins != 'Medium')


alphaBeta <- function(meanPct, varPct) {
  
  common <- meanPct * (1 - meanPct) / varPct - 1
  
  alpha <- meanPct * common
  beta <- (1 - meanPct) * common
  
  return(c(alpha = alpha, beta = beta))
}

highleverage_meanPct <- updated |>
  filter(bins == "High") |>
  summarise(meanPct = mean(situationalPct, na.rm = TRUE))

highleverage_varPct <- updated |>
  filter(bins == "High") |>
  summarise(variance = var(situationalPct, na.rm = TRUE))

prior <- alphaBeta(highleverage_meanPct, highleverage_varPct)

alpha <- prior[[1]]
beta  <- prior[[2]]

updated = updated |>
  mutate(shrunkHighLevPct =
           ((situationalPct * faceoffsTaken) + alpha) /
           (faceoffsTaken + alpha + beta))


lowleverage_meanPct <- updated |>
  filter(bins == "Low") |>
  summarise(meanPct = mean(situationalPct, na.rm = TRUE))

lowleverage_varPct <- updated |>
  filter(bins == "Low") |>
  summarise(variance = var(situationalPct, na.rm = TRUE))

prior <- alphaBeta(lowleverage_meanPct, lowleverage_varPct)

alpha <- prior[[1]]
beta  <- prior[[2]]

updated = updated |>
  mutate(shrunkLowLevPct =
           ((situationalPct * faceoffsTaken) + alpha) /
           (faceoffsTaken + alpha + beta))

updated = updated |>
  mutate(relevantShrunkPct = case_when(bins == 'High' ~ shrunkHighLevPct,
                                       bins == 'Low' ~ shrunkLowLevPct)) |>
  select(strongSide, bins, relevantShrunkPct) |>
  group_by(strongSide, bins) |>
  summarise(fowPct = mean(relevantShrunkPct))











#########################################################################
## MAKING THE CORRELATION PLOT TO TEST FOR STRONG SIDE PROP CONFOUNDER ##
#########################################################################

## using the fow_byLeverage data we already have to pull the y-axis info needed
## (FOW% at High Leverage vs Low Leverage with Shrinkages calculated)
fow_byLeverage = readRDS('fow_byLeverage.RDS')
fow_byLeverage = fow_byLeverage |>
  select(player, difference)

## using faceoffData manipulation to pull the x-axis info needed
## (SS prop at High Leverage vs Low Leverage)
strongSideProp = faceoffData |>
  select(player, strongSide, bins) |>
  count(player, strongSide, bins) |>
  group_by(player, bins) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  filter(bins != 'Medium') |>
  filter(strongSide == '1') |>
  select(player, strongSide, bins, prop) |>
  pivot_wider(names_from = bins, values_from = prop) |>
  mutate(ss_PropDiff = High - Low) |>
  select(player, ss_PropDiff) 

## looking at prop of faceoffs taken on SS vs WS across leverage groups
faceoffData |>
  select(player, strongSide, bins) |>
  count(player, strongSide, bins) |>
  group_by(player, bins) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  filter(bins != 'Medium') |>
  count(strongSide, bins) |>
  group_by(bins) |>
  mutate(prop = n / sum(n)) |>
  arrange(bins)

## putting everything altogether!
confounderData = fow_byLeverage |>
  left_join(strongSideProp, by = 'player')

correlation <- cor(
  confounderData$ss_PropDiff,
  confounderData$difference,
  use = "complete.obs"
)

confounderData |>
  ggplot(aes(x = ss_PropDiff, y = difference)) +
  geom_point() +
  labs(x = 'Difference in Proportion of Faceoffs on Strong Side',
       y = 'Difference in Faceoff Win Percentage',
       caption = 'Data Courtesy of the NHL') +
  theme_light() +
  geom_smooth(method = "lm", se = TRUE) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("r = ", round(correlation, 3)),
    hjust = 1.2,
    vjust = 1.5,
    size = 4
  ) 