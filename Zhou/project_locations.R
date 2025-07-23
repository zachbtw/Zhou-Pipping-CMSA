#project movement using speed vectors - unused in final project
library(tidyverse)
library(arrow)
library(sportyR)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(deldir)
library(sf)

source("Zhou/generate_features.R")

#reading data
tracking <- lazy_dt(arrow::read_parquet("nfl-big-data-bowl-2025/tracking.parquet"))
plays <- lazy_dt(read.csv("nfl-big-data-bowl-2025/plays.csv"))
player_play <- lazy_dt(read.csv("nfl-big-data-bowl-2025/player_play.csv"))
games <- lazy_dt(read.csv("nfl-big-data-bowl-2025/games.csv"))
players <- lazy_dt(read.csv("nfl-big-data-bowl-2025/players.csv"))

#clean tracking and remove multi-throw plays
multi_throws <- tracking |> filter(event == "pass_arrived")  |> select(gameId, playId)  |>
  group_by(gameId, playId) |>
  count(nn = n()) |> filter(nn != 23) |> select(gameId, playId) |> as_tibble()
multi_throws_game = multi_throws$gameId
multi_throws_play = multi_throws$playId
receiver <- player_play |> filter(wasTargettedReceiver == 1) |> 
  select(gameId, playId, nflId) |> rename(targeted_receiver = nflId)
passing_frames <- tracking |> filter(event == "pass_forward")
passing_frames <- passing_frames |>
  clean_tracking() |> filter(!(gameId %in% multi_throws_game & playId %in% multi_throws_play))
passing_frames <- passing_frames |>
  merge(receiver, by  = c("gameId", "playId"))
positions <- players |> select(nflId, position)

#get projections in the future
throw_model <- get_throw_model(tracking, passing_frames)

throw_distance <- passing_frames |> merge(positions, by = "nflId") |>
  filter(targeted_receiver == nflId | position == "QB") |> 
  select(playId, gameId, x, y) |>
  group_by(playId, gameId) |> reframe(count = n(),
                                      qb_dist = sqrt(diff(x)^2 + diff(y)^2)) |> filter(count == 2) |> as_tibble() 
throw_distance <- throw_distance |>
  mutate(est_time = predict(throw_model, newdata = throw_distance))
  
throw_times <- tracking |> select(playId, gameId, frameId, event) |>
  filter(event %in% c("pass_forward", "pass_arrived")) |> distinct() |>
  group_by(playId, gameId) |> summarise(
    air_time = diff(frameId),
    .groups = "drop") |> as_tibble()

#air times from throw distance
air_times <- throw_distance |> merge(throw_times, by = c("playId", "gameId")) |>
  select(gameId, playId, est_time)

throwing_frames <- tracking |> filter(event == "pass_forward") |> 
  merge(air_times, by = c("playId", "gameId"))

#generate point estimates
estimated_reception <- throwing_frames |> clean_tracking() |> 
  mutate(
  seconds = est_time / 10,
  account_acceleration = seconds < .5,
  "x_est_throw" = x + seconds * s_x, #+ (0.5) * a_x * seconds^2 * account_acceleration,
  "y_est_throw" = y + seconds * s_y #+ (0.5) * a_y * seconds^2 * account_acceleration
) |> rename(
  throw_x = x,
  throw_y = y,
) |> select(playId, gameId, nflId, x_est_throw, y_est_throw, est_time, throw_x, throw_y) 


#rule based accounting of acceleration
actual_reception <- tracking |> filter(event == "pass_arrived") |> clean_tracking() |> 
  select(playId, gameId, nflId, x, y, club) |>
  merge(estimated_reception, by = c("playId", "gameId", "nflId")) |> 
  select(playId, gameId, nflId, x, y, x_est_throw, y_est_throw, est_time, throw_x, throw_y,club) |>
  mutate(distance = sqrt((x - x_est_throw)**2 + (y - y_est_throw)**2))

mean_distance <- actual_reception |> filter(!is.na(distance)) |> summarise(mean(distance))
#generate examples
short_sample_play = 2655
short_sample_game = 2022102302

long_sample_play = 110
long_sample_game = 2022101000

sample_play <- actual_reception |> filter(playId == long_sample_play, 
                                          gameId == long_sample_game)

point_color_map = list(
  "ATL" = "red",
  "CIN" = "orange",
  "football" = "#654321"
)

point_color_map = list(
  "KC" = "red",
  "LV" = "black",
  "football" = "#654321"
)
library(patchwork)
long_title = "D.Carr pass incomplete deep right to M.Hollins."
short_title = "J.Burrow pass short middle to T.Boyd to CIN 30 for 9 yards"
p1 <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.6667) +
  geom_segment(
    data = sample_play,
    aes(x = x_est_throw, y = y_est_throw, xend = x, yend = y),
    color = "gray60", linetype = "dashed", linewidth = 0.5
  ) +
  geom_point(data = sample_play, aes(x = x, y = y, color = club), size = 2.5) +
  geom_point(data = sample_play, aes(x = x_est_throw, y = y_est_throw, color = club), alpha = 0.5, size = 2.5) +
  scale_color_manual(values = point_color_map) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title =  "D.Carr Pass Incomplete Deep Right to M.Hollins.", 
       subtitle = "Projections Generated from Extending Player Speed",
       caption = "Opaque = Actual Location, Transparent = Projected Location")


p2 <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.6667) +
  geom_point(data = sample_play, aes(x = throw_x, y = throw_y, color = club), size = 2.5) +
  scale_color_manual(values = point_color_map) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title =  long_title, 
       subtitle = "Locations at Throw")
combined_plot <- p2 / p1  # Use `p1 + p2` for side-by-side layout

combined_plot


