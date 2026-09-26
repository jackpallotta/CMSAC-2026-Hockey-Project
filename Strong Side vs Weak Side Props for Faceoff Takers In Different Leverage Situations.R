## Exploration of Strong Side vs Weak Side Props for Faceoff Takers
## In Different Leverage Situations

## September 26th 2026
## By Chloe Guagliano


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
       fill = 'Faceoff Side') +
  scale_fill_manual(
    values = c("0" = "#79BAEC", "1" = "#4682B4"),
    labels = c("Weak Side", "Strong Side")
  )
  
  