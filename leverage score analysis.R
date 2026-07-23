rm(list=ls())
library(tidyverse)
library(gt)
library(scales)
library(ggridges)
library(classInt)
library(BAMMtools)
library(lme4)
library(lmerTest)
library(emmeans)

faceoffData <- readRDS("faceoffData.rds")
faceoffValueData <- readRDS("faceoffValueData.rds")
players <- readRDS("players.rds")

# jenks natural breaks 1D clustering 
set.seed(91)

leverageBreaks <- faceoffData |>
  distinct(eventId, .keep_all = TRUE) |>
  pull(leverage) |>
  classIntervals(n = 3, style = "jenks")

print(leverageBreaks)

faceoffData <- faceoffData |>
  mutate(leverageGroup = cut(leverage, breaks = leverageBreaks$brks,
                             include.lowest = TRUE,
                             labels = c("Low", "Medium", "High")))

eventLeverage <- faceoffData |>
  distinct(eventId, leverage, leverageGroup)

faceoffValueData <- faceoffValueData |>
  select(-any_of(c("leverage", "leverageGroup"))) |>
  left_join(eventLeverage, by = "eventId", relationship = "many-to-one")

faceoffPlots <- faceoffData |>
  select(gameId, eventId, faceoffPlayerId, player, leverage, leverageGroup, faceoffWon, zoneCode)

players <- readRDS("players.rds")

players <- players |>
  select(playerId, player) |>
  distinct(playerId, .keep_all = TRUE)

# distribution of leverage score with density & rug plot
faceoffData |>
  distinct(eventId, .keep_all = TRUE) |>
  ggplot(aes(x = leverage)) +
  geom_density(fill = "grey80", alpha = 0.6) +
  geom_rug(aes(color = leverageGroup), alpha = 0.3, sides = "b") +
  scale_color_manual(values = c(Low = "#79BAEC", Medium = "#4682B4", High = "#123456")) +
  labs(title = "Faceoffs by Leverage Score",
       caption = "Data courtesy of NHL",
       x = "Leverage Score", 
       y = "Density", 
       color = "Leverage Group") +
  guides(color = guide_legend(override.aes = list(linewidth = 3, alpha = 1))) +
  theme_light() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11))

# percentages of faceoffs by leverage group
faceoffData |>
  distinct(eventId, leverageGroup) |>
  count(leverageGroup) |>
  mutate(proportion = n / sum(n),
         percent = scales::percent(proportion, accuracy = 0.1))

# tweedie GAM justification comparing goals vs xG within 5 seconds of faceoffs
faceoffData |>
  distinct(eventId, .keep_all = TRUE) |>
  summarize(faceoffs = n(),
    goals = sum(goalFor5),
    pct_goal = mean(goalFor5))

xg_summary <- faceoffData |>
  distinct(eventId, xGFor5)

pct_zero <- mean(xg_summary$xGFor5 == 0) * 100

faceoffData |>
  distinct(eventId, xGFor5) |>
  ggplot(aes(x = xGFor5)) +
  geom_histogram(binwidth = 0.02, boundary = 0, fill = "#4682B4",
                 color = "white") +
  coord_cartesian(xlim = c(0, 0.10)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.10),
                     labels = c("0", "0.025", "0.05", "0.075", "0.10")) +
  labs(title = "Distribution of xG Within 5 Seconds After Faceoffs",
    x = "xG Values",
    y = "Faceoffs") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title = element_text(size = 12)) +
  annotate("label", x = 0.022, y = 300000,
    label = paste0(round(pct_zero, 1), "% of faceoffs\nproduced 0 xG"),
    size = 7, hjust = 0, fill = "white", label.size = 0.3)

# additional analysis
offensive <- faceoffPlots |>
  filter(zoneCode == "O")

defensive <- faceoffPlots |>
  filter(zoneCode == "D")

playerSummaryOFF <- faceoffPlots |>
  filter(zoneCode == "O") |>
  group_by(faceoffPlayerId, leverageGroup) |>
  summarize(FO = n(),
    Wins = sum(faceoffWon),
    .groups = "drop") |>
  left_join(players, by = c("faceoffPlayerId" = "playerId")) |>
  filter(FO >= 25) |>
  group_by(faceoffPlayerId) |>
  filter(n_distinct(leverageGroup) == 3) |>
  ungroup()

model_off <- glmer(cbind(Wins, FO - Wins) ~ leverageGroup + (1 | faceoffPlayerId),
                   data = playerSummaryOFF, family = binomial)

model_off_null <- glmer(cbind(Wins, FO - Wins) ~ 1 + (1 | faceoffPlayerId),
                        data = playerSummaryOFF, family = binomial)

anova(model_off_null, model_off, test = "Chisq")

emmeans(model_off, pairwise ~ leverageGroup, type = "response", adjust = "tukey")

playerSummaryDEF <- faceoffPlots |>
  filter(zoneCode == "D") |>
  group_by(faceoffPlayerId, leverageGroup) |>
  summarize(FO = n(),
    Wins = sum(faceoffWon),
    .groups = "drop") |>
  left_join(players, by = c("faceoffPlayerId" = "playerId")) |>
  filter(FO >= 25) |>
  group_by(faceoffPlayerId) |>
  filter(n_distinct(leverageGroup) == 3) |>
  ungroup()

model_def <- glmer(cbind(Wins, FO - Wins) ~ leverageGroup + (1 | faceoffPlayerId),
                   data = playerSummaryDEF, family = binomial)

model_def_null <- glmer(cbind(Wins, FO - Wins) ~ 1 + (1 | faceoffPlayerId),
                        data = playerSummaryDEF, family = binomial)

anova(model_def_null, model_def, test = "Chisq")

emmeans(model_def, pairwise ~ leverageGroup, type = "response", adjust = "tukey")

off_df <- emmeans(model_off, ~ leverageGroup, type = "response") |>
  as.data.frame() |>
  mutate(zone = "Offensive Zone")

def_df <- emmeans(model_def, ~ leverageGroup, type = "response") |>
  as.data.frame() |>
  mutate(zone = "Defensive Zone")

plot_df <- bind_rows(off_df, def_df)

plot_df |>
  ggplot(aes(x = leverageGroup, y = prob, group = 1)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "grey80") +
  facet_wrap(~zone) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Estimated Faceoff Win Probability by Zone and Leverage Group",
    x = "Leverage Group",
    y = "Estimated Faceoff Win %",
    caption = "Data courtesy of NHL") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.background = element_rect(fill = "#123456"),
    strip.text = element_text(face = "bold"))

colnames(faceoffValueData)

playerWinValueRankings <- faceoffValueData |>
  filter(faceoffWon == 1) |>
  group_by(faceoffPlayerId, player) |>
  summarize(faceoffWins = n(),
    avgOffensiveWinValue = mean(offensiveWinValue, na.rm = TRUE),
    avgDefensiveWinValue = mean(defensiveWinValue, na.rm = TRUE),
    totalOffensiveWinValue = sum(offensiveWinValue, na.rm = TRUE),
    totalDefensiveWinValue = sum(defensiveWinValue, na.rm = TRUE),
    avgNetWinValue = mean(netxGWinValue, na.rm = TRUE),
    totalNetWinValue = sum(netxGWinValue, na.rm = TRUE),
    .groups = "drop") |>
  filter(faceoffWins >= 100)

offensiveTable <- playerWinValueRankings |>
  slice_max(totalOffensiveWinValue, n = 10, with_ties = FALSE) |>
  arrange(desc(totalOffensiveWinValue)) |>
  mutate(Rank = row_number()) |>
  select(Rank, Player = player, `Faceoff Wins` = faceoffWins,
         `Offensive Win Value` = totalOffensiveWinValue) |>
  gt() |>
  tab_header(title = md("**Top 10 Players by Offensive Faceoff Win Value**"),
    subtitle = "Cumulative xG generated from faceoff wins") |>
  fmt_number(columns = `Offensive Win Value`, decimals = 2) |>
  data_color(
    columns = `Offensive Win Value`,
    palette = c("#deebf7", "#9ecae1", "#4292c6", "#08519c"),
    domain = range(
      playerWinValueRankings$totalOffensiveWinValue,
      na.rm = TRUE)) |>
  cols_align(align = "center", columns = c(Rank, `Faceoff Wins`, `Offensive Win Value`)) |>
  cols_align(align = "left", columns = Player) |>
  cols_width(Rank ~ px(60), Player ~ px(160), `Faceoff Wins` ~ px(110), `Offensive Win Value` ~ px(160)) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = Player)) |>
  tab_options(heading.align = "center", table.font.size = px(14),
    column_labels.font.weight = "bold", data_row.padding = px(6))

offensiveTable

defensiveTable <- playerWinValueRankings |>
  slice_max(totalDefensiveWinValue, n = 10, with_ties = FALSE) |>
  arrange(desc(totalDefensiveWinValue)) |>
  mutate(Rank = row_number()) |>
  select(Rank, Player = player, `Faceoff Wins` = faceoffWins,
         `Defensive Win Value` = totalDefensiveWinValue) |>
  gt() |>
  tab_header(title = md("**Top 10 Players by Defensive Faceoff Win Value**"),
             subtitle = "Cumulative xG prevented from faceoff wins") |>
  fmt_number(columns = `Defensive Win Value`, decimals = 2) |>
  data_color(
    columns = `Defensive Win Value`,
    palette = c("#deebf7", "#9ecae1", "#4292c6", "#08519c"),
    domain = range(
      playerWinValueRankings$totalDefensiveWinValue,
      na.rm = TRUE)) |>
  cols_align(align = "center", columns = c(Rank, `Faceoff Wins`, `Defensive Win Value`)) |>
  cols_align(align = "left", columns = Player) |>
  cols_width(Rank ~ px(60), Player ~ px(160), `Faceoff Wins` ~ px(110), `Defensive Win Value` ~ px(150)) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = Player)) |>
  tab_options(heading.align = "center", table.font.size = px(14),
              column_labels.font.weight = "bold", data_row.padding = px(6))

defensiveTable

netWinValueTable <- playerWinValueRankings |>
  slice_max(totalNetWinValue, n = 10, with_ties = FALSE) |>
  arrange(desc(totalNetWinValue)) |>
  mutate(Rank = row_number()) |>
  select(Rank, Player = player, `Faceoff Wins` = faceoffWins,
         `Net Win Value` = totalNetWinValue) |>
  gt() |>
  tab_header(title = md("**Top 10 Players by Net Faceoff Win Value**"),
             subtitle = "Cumulative net xG from faceoff wins") |>
  fmt_number(columns = `Net Win Value`, decimals = 2) |>
  data_color(
    columns = `Net Win Value`,
    palette = c("#deebf7", "#9ecae1", "#4292c6", "#08519c"),
    domain = range(
      playerWinValueRankings$totalNetWinValue,
      na.rm = TRUE)) |>
  cols_align(align = "center", columns = c(Rank, `Faceoff Wins`, `Net Win Value`)) |>
  cols_align(align = "left", columns = Player) |>
  cols_width(Rank ~ px(60), Player ~ px(160), `Faceoff Wins` ~ px(110), `Net Win Value` ~ px(150)) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = Player)) |>
  tab_options(heading.align = "center", table.font.size = px(14),
              column_labels.font.weight = "bold", data_row.padding = px(6))

netWinValueTable
