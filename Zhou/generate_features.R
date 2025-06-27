library(tidyverse)
library(data.table)
library(dtplyr)
library(pbapply)
get_closest_opponent <- function(tracking) {
  player_pos <- tracking |> filter(club != "football")
  
  teams <- unique(player_pos$club)
  
  team_1_pos <- player_pos |> filter(club == teams[1]) |> select(nflId, x, y)
  team_2_pos <- player_pos |> filter(club == teams[2]) |> select(nflId, x, y)
  #there has to be a faster way to do this...
  cross_df <- team_1_pos |> cross_join(team_2_pos, suffix = c("_1", "_2")) |>
    mutate(distance = sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2))
  
  closest_1 <- cross_df |>
    group_by(nflId_1) |>
    slice_min(distance, n = 1) |>
    ungroup() |>
    select(nflId = nflId_1, closestOpponentId = nflId_2, closestOpponentDistance = distance)
  
  closest_2 <- cross_df |>
    group_by(nflId_2) |>
    slice_min(distance, n = 1) |>
    ungroup() |>
    select(nflId = nflId_2, closestOpponentId = nflId_1, closestOpponentDistance = distance)
  closest_all <- bind_rows(closest_1, closest_2)
  tracking_out <- tracking |>
    left_join(closest_all, by = "nflId")
  
  return(tracking_out)
}

get_motion_difference <- function(tracking) {
  tracking_cleaned <- tracking |> select(nflId, x, y, s_x, s_y, closestOpponentId)
  
  # Separate player info and opponent info
  player_info <- tracking_cleaned
  opponent_info <- tracking_cleaned |> select(-c(closestOpponentId)) |>
    rename(
      closestOpponentId = nflId,
      opp_x = x,
      opp_y = y,
      opp_s_x = s_x,
      opp_s_y = s_y
    )
  
  combined <- player_info |> 
    left_join(opponent_info, by = "closestOpponentId") |>
    mutate(
      motion_diff = (s_x - opp_s_x)^2 + (s_y - opp_s_y)^2
    ) |> select(nflId, motion_diff)
  tracking |> left_join(combined, by = "nflId")
}


main <- function(){
  #reading data
  tracking <- lazy_dt(arrow::read_parquet("nfl-big-data-bowl-2025/tracking.parquet"))
  plays <- lazy_dt(read.csv("nfl-big-data-bowl-2025/plays.csv"))
  player_play <- lazy_dt(read.csv("nfl-big-data-bowl-2025/player_play.csv"))
  games <- lazy_dt(read.csv("nfl-big-data-bowl-2025/games.csv"))
  players <- lazy_dt(read.csv("nfl-big-data-bowl-2025/players.csv"))
  #left to right cleaning
  tracking <- tracking |>
    mutate(
      # make all plays go from left to right
      x = ifelse(playDirection == "left", 120 - x, x),
      y = ifelse(playDirection == "left", 160 / 3 - y, y),
      # flip player direction and orientation
      dir = ifelse(playDirection == "left", dir + 180, dir),
      dir = ifelse(dir > 360, dir - 360, dir),
      o = ifelse(playDirection == "left", o + 180, o),
      o = ifelse(o > 360, o - 360, o),
      o_rad = pi * (o / 180),
      o_x = ifelse(is.na(o), NA_real_, sin(o_rad)),
      o_y = ifelse(is.na(o), NA_real_, cos(o_rad)),
      s_x = dir_x * s,
      s_y = dir_y * s,
      a_x = dir_x * a,
      a_y = dir_y * a
    )
  sample_game = 2022091200
  sample_play = 64
  #get positions and throwing frames
  positions <- players |> select(nflId, position)
  throws <- plays |> filter(passResult %in% c("C", "I", "IN"))
  passing_frames <- tracking |> filter(event == "pass_forward")
  receiver <- player_play |> filter(wasTargettedReceiver == 1) |> 
    select(gameId, playId, nflId) |> rename(targeted_receiver = nflId)
  
  passing_frames <- passing_frames |>
    merge(receiver, by  = c("gameId", "playId"))
  
  #find closest opponent for each player
  closest <- passing_frames |>
    as_tibble() |>
    group_by(gameId, playId) |>
    group_split() |>
    pblapply(function(df) {
      get_closest_opponent(df)
    }) |>
    bind_rows()
  
  #get motion difference for the closest opponent
  closest <- closest |>
    group_by(gameId, playId) |>
    group_split() |>
    pblapply(function(df) {
      get_motion_difference(df)
    }) |> bind_rows()
}
main()
