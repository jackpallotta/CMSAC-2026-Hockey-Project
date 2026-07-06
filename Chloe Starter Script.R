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
  kmeans(algorithm = "Lloyd", centers = 4, nstart = 30, iter.max = 40)

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

## adding columns based on game situation logic to group faceoffs by similar
## physical game situation (zone / side)
situationalFaceoffs = faceoffs |>
  mutate(leftRight = case_when(homeTeamDefendingSide == 'left' & eventTeamVenue == 'home'~
                               if_else(faceoffDotCategory %in% c('1','2','3','4'), 
                                       'left',
                                       'right'),
                               homeTeamDefendingSide == 'left' & eventTeamVenue == 'away'~
                               if_else(faceoffDotCategory %in% c('5','6','7','8'),
                                       'left',
                                       'right'),
                               homeTeamDefendingSide == 'right' & eventTeamVenue == 'home'~
                               if_else(faceoffDotCategory %in% c('5','6','7','8'),
                                       'left',
                                       'right'),
                               homeTeamDefendingSide == 'right' & eventTeamVenue == 'away'~
                               if_else(faceoffDotCategory %in% c('1','2','3','4'),
                                       'left',
                                       'right')
                              )
                                       
        ) |>
  mutate(zoneCode = if_else(zoneCode == 'N' & faceoffDotCategory != 'C',
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
                     'F-NZ'),
         zoneCode)
  ) |>
  mutate(zoneCode = if_else(zoneCode == 'N',
                            'C',
                            zoneCode))



situationalFaceoffs = situationalFaceoffs |>
  mutate(situationDescriptor = paste(zoneCode, leftRight, sep = ' ')) |>
  mutate(situationLinkage = case_when(situationDescriptor %in% c('D left', 'O right') ~
                                        'A',
                                      situationDescriptor %in% c('D right', 'O left') ~
                                        'B',
                                      situationDescriptor %in% c('F-NZ right', 'C-NZ left') ~
                                        'C',
                                      situationDescriptor %in% c('F-NZ left', 'C-NZ right') ~
                                        'D',
                                      situationDescriptor == 'C right' ~
                                        'E'))

## ripping EDA with the situational Faceoffs


## which faceoff situations occur the most frequently
situationalFaceoffs |>
  count(situationLinkage) |>
  mutate(prop = n/sum(n)) |>
  arrange(desc(prop)) |>
  ggplot(aes(x = fct_reorder(situationLinkage,desc(prop)), y = prop)) +
  geom_col() +
  theme_minimal() + 
  labs(x = 'Faceoff Situation Type',
       y = 'Proportion of Total Faceoffs',
       title = 'Proportion of NHL Faceoffs by Situation')

## causes for one OZ / DZ faceoff situation over another (comparing linkage A and B)
situationalFaceoffs |>
  filter(situationLinkage %in% c('A','B')) |>
  count(situationLinkage, lastEvent) |>
  filter(lastEvent %in% c('penalty','stoppage')) |>
  ggplot(aes(x = lastEvent, y= n, fill = situationLinkage)) +
  geom_col(position = 'dodge') + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Last Event',
       y = 'Number of Faceoffs',
       title = 'Causes for One OZ / DZ Faceoff Situation Over Another')

## which outcome occurs most frequently at each situational faceoff location
situationalFaceoffs |>
  filter(situationLinkage != 'E') |>
  count(situationLinkage, leftRight) |>
  group_by(situationLinkage) |>
  mutate(prop = prop.table(n)) |>
  ungroup() |>
  ggplot(aes(x = situationLinkage, y = prop, fill = leftRight)) + 
  geom_col(position = 'dodge') + 
  labs(x = 'Faceoff Situation Type',
       y = 'Proportion for Each Outcome',
       fill = 'Left / Right Win',
       title = 'Proportion Win Splits by Faceoff Situation')

## simple left vs right across all faceoff situations graph (minus center ice)
situationalFaceoffs |>
  filter(situationLinkage != 'E') |>
  count(leftRight) |>
  mutate(prop = n/sum(n)) |>
  ggplot(aes(x = leftRight, y = prop)) +
  geom_col() +
  labs(x = 'Side of Ice for Faceoff Winner',
       y = 'Proportion of Total Faceoff Wins',
       title = 'Proportion of Faceoff Wins by Side of Ice')

## EDA involving faceoff win locations and subsequent shot attempts 
shotAttemptsAfterFaceoff = events_after_faceoff2 |>
  filter(eventTypeDescKey %in% c('shot-on-goal','missed-shot','goal'))

faceoffsWithShotAttempts = unique(shotAttemptsAfterFaceoff$faceoff_row)

situationalFaceoffs = situationalFaceoffs |>
  mutate(shotAttempt_yn = if_else(faceoff_row %in% faceoffsWithShotAttempts,
                                  'Y',
                                  'N'))

faceoffsWon_byLocation = situationalFaceoffs |>
  count(situationDescriptor) |>
  rename(totalFaceoffsWon = n)

situationalFaceoffs |>
  filter(shotAttempt_yn == 'Y') |>
  count(situationDescriptor) |>
  rename(wins_withAttempt = n) |>
  left_join(faceoffsWon_byLocation, by = 'situationDescriptor') |>
  mutate(prop = wins_withAttempt / totalFaceoffsWon) |>
  arrange(desc(prop)) |>
  mutate(situationDescriptor = if_else(situationDescriptor == 'C right',
                                       'Center',
                                       situationDescriptor)) |>
  ggplot(aes(x = fct_reorder(situationDescriptor,desc(prop)), y = prop)) + 
  geom_col() + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = 'Faceoff Win Location',
       y = 'Proportion',
       title = 'Proportion of Faceoff Wins with Subsequent Unblocked Shot Attempt by Faceoff Win Location')


## what proportion of faceoffs with at least one shot attempt have multiple 
## subsequent unblocked shot attempts associated with the draw
shotAttemptsAfterFaceoff |>
  count(faceoff_row) |>
  arrange(desc(n)) |>
  count(n) |>
  rename(numShotAttempts = n) |>
  rename(n = nn) |>
  mutate(numShotAttempts = as.character(numShotAttempts)) |>
  mutate(numShotAttempts = if_else(numShotAttempts != '1',
                                   '2+',
                                   numShotAttempts)) |>
  mutate(prop = n/sum(n)) |>
  ggplot(aes(x = numShotAttempts, y = prop)) +
  geom_col() + 
  labs(x = 'Number of Unblocked Shot Attempts',
       y = 'Proportion',
       title = 'Proportion of Number of Unblocked Shot Attempts for Faceoffs with Shot Attempts')

## cumulative number of faceoffs as it pertains to time in game
situationalFaceoffs = situationalFaceoffs |>
  mutate(minutesElapsedInGame = secondsElapsedInGame %/% 60)

situationalFaceoffs |>
  count(minutesElapsedInGame) |>
  arrange(desc(n)) |>
  ggplot(aes(x = minutesElapsedInGame, y = n)) +
  geom_vline(xintercept = 20, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = 40, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = 60, color = 'red', linetype = 'dashed') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
  geom_line() +
  coord_cartesian(xlim = c(0,65)) + 
  labs(x = 'Minutes Elapsed In Game',
       y = 'Number of Faceoffs',
       title = 'Faceoff Frequency by Game Time Elapsed')




situationalFaceoffs |>
  mutate(scoreState = case_when(scoreState >= 4 | scoreState <= -4 ~ '+/- 4 or greater',
                                scoreState %in% c(-3,3) ~ '+/- 3',
                                scoreState %in% c(-2,2) ~ '+/- 2',
                                scoreState %in% c(-1,1) ~ '+/- 1',
                                scoreState == 0 ~ '0')
  )|>
  count(minutesElapsedInGame, scoreState) |>
  arrange(desc(n)) |>
  ggplot(aes(x = minutesElapsedInGame, y = n, color = scoreState)) +
  geom_line() + 
  theme_minimal() +
  coord_cartesian(xlim = c(40,60))


situationalFaceoffs |>
  mutate(scoreState = case_when(scoreState >= 4 | scoreState <= -4 ~ '+/- 4 or greater',
                                scoreState %in% c(-3,3) ~ '+/- 3',
                                scoreState %in% c(-2,2) ~ '+/- 2',
                                scoreState %in% c(-1,1) ~ '+/- 1',
                                scoreState == 0 ~ '0')
  )|>
  count(scoreState) |>
  rename(numberOfFaceoffs = n) |>
  arrange(desc(numberOfFaceoffs)) |>
  gt() |>
  fmt_number(use_seps = TRUE,
             decimals = 0) |>
  tab_header(title = 'Number of Faceoffs by Score State')
  