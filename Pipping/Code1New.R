c(1, 2, 3, 4, 5, 6, 7)

# Identifying separation and likely targets during plays
# Amazon does some version of this, or used to, but it wasn't very good
library(tidyverse)
library(arrow)
library(sportyR)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(deldir)
library(sf)
#comment
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


tracking <- lazy_dt(arrow::read_parquet("Pipping/tracking.parquet"))
plays <- lazy_dt(read.csv("Pipping/nfl-big-data-bowl-2025/plays.csv"))
player_play <- lazy_dt(read.csv("Pipping/nfl-big-data-bowl-2025/player_play.csv"))
games <- lazy_dt(read.csv("Pipping/nfl-big-data-bowl-2025/games.csv"))
players <- lazy_dt(read.csv("Pipping/nfl-big-data-bowl-2025/players.csv"))


plays_sub <- plays |>
  select(gameId, playId, possessionTeam, passResult, pff_manZone)

tracking <- left_join(tracking, plays_sub, by = c("gameId", "playId"))
passes_man <- tracking |> 
  filter(pff_manZone == "Man", passResult %in% c("C", "I", "IN")) |> 
  mutate(side = ifelse(club == possessionTeam, "offense", "def"))

receiving_stats <- player_play |> 
  select(gameId, playId, nflId, wasTargettedReceiver)

passes_man_targets <- left_join(passes_man, receiving_stats, by = c("gameId", "playId", "nflId")) |> 
  filter(wasTargettedReceiver == 1)
targets_at_pass <- passes_man_targets |> 
  filter(event == "pass_forward")
# view(head(targets_at_pass))

passes_mandef <- passes_man |> 
  filter(side == "def")
mandef_at_pass <- passes_mandef |> 
  filter(event == "pass_forward")
view(head(mandef_at_pass))


# passes_zone <- tracking |> 
  # filter(pff_manZone == "Zone", passResult %in% c("C", "I", "IN")) |> 
  # mutate(side = ifelse(club == possessionTeam, "off", "def"))

pass_man_frames <- passes_man |> 
  group_by(gameId, playId) |> 
  filter(event == "pass_forward") |> 
  distinct(frameId) |> 
  select(gameId, playId, frameId) |> 

passes_man |> 
  filter(gameId %in% pass_man_frames,
         playId %in% pass_man_frames, frameId %in% pass_man_frames)

view(pass_man_frames)

view(head(passes_man))


#all_events <- 
 # passes_man |> 
  # distinct(event) 

