library(tidyverse)
library(arrow)
library(gganimate)
library(sportyR)
#reading data
tracking <-  arrow::read_parquet("nfl-big-data-bowl-2025/tracking.parquet")
plays <- read.csv("nfl-big-data-bowl-2025/plays.csv")
player_play <- read.csv("nfl-big-data-bowl-2025/player_play.csv")
games <- read.csv("nfl-big-data-bowl-2025/games.csv")

#animating a play

game_id = 2022091200
play_id = 64
tracking_play <- tracking |> dplyr::filter(gameId == game_id & playId == play_id)

tracking_play <- tracking_play |>
  dplyr::mutate(
    color = case_when(
      club == "DEN" ~ "#c83803",
      club == "SEA"~ "lightblue",
      club == "football"~ "#624a2e"
    )
  )
nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)

# Display the field
play_anim <- nfl_field +
  geom_point(
    data = tracking_play,
    aes(x = x, y = y, color = color)
  ) +
  transition_time(frameId)

# Show the animation
animation <- animate(play_anim, fps = 2, duration = 20, renderer = gifski_renderer())
animation
