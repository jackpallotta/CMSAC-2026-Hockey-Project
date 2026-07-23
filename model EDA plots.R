# create plots shown in EDA section of the report 
leverageVariables |>
  distinct(eventId, .keep_all = TRUE) |>
  filter(isOT == "Regulation") |>
  mutate(timeBin = cut(secondsRemaining, breaks = seq(0, 3600, by = 300), include.lowest = TRUE)) |>
  group_by(goalDifferential, timeBin) |>
  summarize(secondsRemaining = mean(secondsRemaining),
            winPct = mean(wonGame),
            .groups = "drop") |>
  ggplot(aes(secondsRemaining, winPct,
             color = goalDifferential)) +
  geom_line(linewidth = 1) +
  scale_x_reverse() +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.25)) +
  scale_color_manual(
    values = c("-3" = "#B2182B", "-2" = "#EF8A62", "-1" = "#FDB863",
      "0"  = "#404040", "1"  = "#92C5DE", "2"  = "#67A9CF", "3"  = "#2166AC")) +
  labs(title = "Empirical Win Percentage by Goal Differential and Time Remaining",
       x = "Seconds Remaining",
       y = "Empirical Win Percentage",
       color = "Score Differential") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title = element_text(size = 12))

leverageVariables |>
  distinct(eventId, .keep_all = TRUE) |>
  group_by(situationCode) |>
  summarize(WinPct = mean(wonGame), Faceoffs = n()) |>
  ggplot(aes(x = reorder(situationCode, WinPct), y = WinPct, fill = WinPct)) +
  geom_col() +
  scale_fill_gradient2(low = "red", mid = "gold", high = "forestgreen", 
                       midpoint = 0.5, labels = scales::percent) +
  geom_text(aes(label = scales::percent(WinPct, accuracy = 0.1)),
            size = 3, vjust = -0.5) +
  labs(title = "Empirical Win Percentage by Situation Code",
       x = "Situation Code",
       y = "Empirical Win Percentage",
       fill = "Win Percentage") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title = element_text(size = 12))

leverageVariables |>
  distinct(eventId, .keep_all = TRUE) |>
  group_by(zoneCode, faceoffWon) |>
  summarize(mean_xG = mean(xGFor5), .groups = "drop") |>
  mutate(faceoffWon = factor(faceoffWon,
                             levels = c(0, 1),
                             labels = c("Loss", "Win"))) |>
  ggplot(aes(zoneCode, mean_xG, fill = faceoffWon)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  labs(title = "Mean xG Within 5 Seconds by Faceoff Zone",
    x = "Faceoff Zone",
    y = "Mean xG Within 5 Seconds",
    fill = "Faceoff Outcome"
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title = element_text(size = 12))
