library(tidyverse)
library(data.table)
library(dtplyr)
library(pbapply)
library(xgboost)
library(caret)
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
  tracking_cleaned <- tracking |> select(nflId, x, y, s_x, s_y, closestOpponentId, club) |>
    filter(club != "football")
  
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
  
  sample_game = 2022091200
  sample_play = 64
  #get positions and throwing frames
  positions <- players |> select(nflId, position)
  throws <- plays |> filter(passResult %in% c("C", "I", "IN"))
  passing_frames <- tracking |> filter(event == "pass_forward")
  receiver <- player_play |> filter(wasTargettedReceiver == 1) |> 
    select(gameId, playId, nflId) |> rename(targeted_receiver = nflId)
  passing_frames <- passing_frames |>
    mutate(
      # make all plays go from left to right
      x = ifelse(playDirection == "left", 120 - x, x),
      y = ifelse(playDirection == "left", 160 / 3 - y, y),
      # flip player direction and orientation
      dir = ifelse(playDirection == "left", dir + 180, dir),
      dir = ifelse(dir > 360, dir - 360, dir),
      o = ifelse(playDirection == "left", o + 180, o),
      o = ifelse(o > 360, o - 360, o),
      dir_rad = pi * (dir / 180),
      dir_x = ifelse(is.na(dir), NA_real_, sin(dir_rad)),
      dir_y = ifelse(is.na(dir), NA_real_, cos(dir_rad)),
      s_x = dir_x * s,
      s_y = dir_y * s,
      a_x = dir_x * a,
      a_y = dir_y * a
    )
  passing_frames <- passing_frames |>
    merge(receiver, by  = c("gameId", "playId"))
  
  #find closest opponent for each player - have to include frameId for multiple throw plays
  closest <- passing_frames |>
    as_tibble() |>
    group_by(gameId, playId, frameId) |>
    group_split() |>
    pblapply(function(df) {
      get_closest_opponent(df)
    }) |>
    bind_rows()
  
  #get motion difference for the closest opponent
  closest <- closest |>
    group_by(gameId, playId, frameId) |>
    group_split() |>
    pblapply(function(df) {
      get_motion_difference(df)
    }) |> bind_rows()
  multi_throws_game = c(2022103002, 2022110608)
  multi_throws_play = c(1263, 2351)
  #remove multi-throw plays
  closest <- closest |> filter(!(gameId %in% multi_throws_game & playId %in% multi_throws_play)) |>
    mutate(is_targetted = nflId == targeted_receiver)
  closest <- closest |> filter(!is.na(motion_diff)) |> merge(positions, by = "nflId")
  closest <- closest |> filter(position %in% c("WR", "RB", "TE", "HB", "RB"))
  closest <- closest |>
    fastDummies::dummy_cols(select_columns = "position", remove_selected_columns = TRUE)
  features = c("closestOpponentDistance", "motion_diff")
  #names(closest)[grepl("^position_", names(closest))]
  cv_top1_acc <- run_xgbRanker_cv(closest, features = features, 
                                  label = "is_targetted", k = 5, nrounds = 100, top_n = 1)
    
}
main()

