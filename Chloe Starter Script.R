## Chloe Starter Script

library(nhlscraper)

nhl_rink = draw_NHL_rink()

pbp = gc_play_by_play(game = 2023030417)

shiftchart = shift_chart(game = 2023030417)

player_summary(player = 8478402)$shootsCatches

gc_summary(game = 2023030417)
