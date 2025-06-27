c(1, 2, 3, 4, 5)

# Identifying separation and likely targets during plays
# Amazon does some version of this, or used to, but it wasn't very good
library(gganimate)
library(sportyR)
field_params <- list(
  field_apron = "springgreen3",
  field_border = "springgreen3",
  offensive_endzone = "springgreen3",
  defensive_endzone = "springgreen3",
  offensive_half = "springgreen3",
  defensive_half = "springgreen3"
)
field_background <- geom_football(
  league = "nfl",
  display_range = "in_bounds_only",
  x_trans = 60,
  y_trans = 26.6667,
  xlims = c(60, 90),
  color_updates = field_params
)
field_background
tracking_1_4 |>
  filter(gameId == 2022091200) |>
  ungroup()

example_play <- tracking_1_4 |> 
  mutate(team = club) |>
  select(!club) |>
  filter(gameId == 2022091200, playId == 64) |> 
  mutate(pt_color = case_when(jerseyNumber == 7 ~ "red",
                              team == "DEN" ~ "orange",
                              team == "SEA" ~ "grey",
                              team == "football" ~ "yellow"))
mean(tracking_1_4$playDirection == "left")
field_background +
  geom_point(data = example_play,
             aes(120 - x, 160 / 3 - y),
             size = 3,
             color = example_play$pt_color) +
  transition_time(example_play$frameId)



games <- read_csv("games.csv")
plays <- read_csv("plays.csv")
players <- read_csv("players.csv")
player_plays <- read_csv("player_play.csv")

tracking_1_4 <- tracking_1_4 |>
  mutate(game_play = as.numeric(paste(gameId, playId, sep = ""))) |>
  select(game_play, everything(), -gameId, -playId)
view(head(tracking_1_4, n = 150))
glimpse(players)

plays <- plays |>
  mutate(game_play = as.numeric(paste(gameId, playId, sep = ""))) |>
  select(game_play, everything(), -gameId, -playId)
view(head(plays))

plays_sub <- plays |>
  select(game_play, pff_manZone)

names(plays)
table(plays$pff_manZone) 

joint <- left_join(tracking_1_4, plays_sub, by = "game_play")

