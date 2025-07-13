library(tidyverse)
library(data.table)
library(dtplyr)
library(pbapply)
library(xgboost)
library(caret)

clean_tracking <- function(tracking) {
  tracking|>
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
}

get_nth_closest_opponent <- function(tracking, n = 1) {
  player_pos <- tracking |> filter(club != "football")
  
  teams <- unique(player_pos$club)
  
  team_1_pos <- player_pos |> filter(club == teams[1]) |> select(nflId, x, y)
  team_2_pos <- player_pos |> filter(club == teams[2]) |> select(nflId, x, y)
  #there has to be a faster way to do this...
  cross_df <- team_1_pos |> cross_join(team_2_pos, suffix = c("_1", "_2")) |>
    mutate(distance = sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2))
  
  closest_1 <- cross_df |>
    group_by(nflId_1) |>
    arrange(distance) |>
    dplyr::slice(n) |>
    ungroup() |>
    select(nflId = nflId_1, closestOpponentId = nflId_2, closestOpponentDistance = distance)
  
  closest_2 <- cross_df |>
    group_by(nflId_2) |>
    arrange(distance) |>
    dplyr::slice(n) |>
    ungroup() |>
    select(nflId = nflId_2, closestOpponentId = nflId_1, closestOpponentDistance = distance)
  closest_all <- bind_rows(closest_1, closest_2) |>
    rename(!!paste0("closestOpponentId_", n) := closestOpponentId,
           !!paste0("closestOpponentDistance_",n) := closestOpponentDistance)
  tracking_out <- tracking |>
    left_join(closest_all, by = "nflId")
  
  return(tracking_out)
}

get_motion_difference <- function(tracking, n = 1) {
  id <- paste0("closestOpponentId_", n)
  tracking_cleaned <- tracking |>
    select(nflId, x, y, s_x, s_y, !!sym(id), club) |>
    filter(club != "football")
  
  # Separate player info and opponent info
  player_info <- tracking_cleaned
  opponent_info <- tracking_cleaned |> select(-c(!!sym(id))) |>
    rename(
      !!sym(id) := nflId,
      opp_x = x,
      opp_y = y,
      opp_s_x = s_x,
      opp_s_y = s_y
    )
  combined <- player_info |> 
    left_join(opponent_info, by = id) |>
    mutate(
      motion_diff = sqrt((s_x - opp_s_x)^2 + (s_y - opp_s_y)^2)
    ) |> select(nflId, motion_diff) |>
    rename(!!paste0("motion_diff_",n) := motion_diff)
  tracking |> left_join(combined, by = "nflId")
}

simulate_movement <- function(n, player, frame, closest_n = 1) {
  player_locs <- frame |> filter(nflId == player)
  player_position <- player_locs$position[1]
  if(!(player_position %in% c("WR",  "TE", "HB", "RB"))){
    #not a position we care about
    return(NA)
  }
  team <- player_locs$club[1]
  seconds <- n / 10
  frame <- frame |> mutate(
    "x_est_throw" = x + seconds * s_x, #+ (0.5) * a_x * seconds^2,
    "y_est_throw" = y + seconds * s_y #+ (0.5) * a_y * seconds^2
  )#yd/s <- yd / f
  player_coords <- frame |> filter(nflId == player) |> select(x_est_throw, y_est_throw)
  opponents <- frame |> filter(club != team, !is.na(club)) |>
    mutate(player_distance = sqrt((x_est_throw - player_coords$x_est_throw[1])^2 + 
                                   (y_est_throw - player_coords$y_est_throw[1])^2)) |>
    arrange(player_distance) 
  opponents <- opponents |>
    dplyr::slice(closest_n)
  
  opponents$player_distance[1]
}
get_throw_model <- function(tracking, passing_frames){
  #generates linear relationship between player distance and aerial throw time
  #could(and did) do better with gam and other methods but not by much to justify
  #intepretation differences
  
  #if qb not throwing, trick play probably disregard
  
  throw_distance <- passing_frames |> merge(positions, by = "nflId") |>
    filter(targeted_receiver == nflId | position == "QB") |> 
    select(playId, gameId, x, y) |>
    group_by(playId, gameId) |> reframe(count = n(),
      qb_dist = sqrt(diff(x)^2 + diff(y)^2),
      .groups = "drop") |> filter(count == 2) |> as_tibble()
    
  throw_times <- tracking |> select(playId, gameId, frameId, event) |>
    filter(event %in% c("pass_forward", "pass_arrived")) |> distinct() |>
                       group_by(playId, gameId) |> summarise(
                         air_time = diff(frameId),
                         .groups = "drop") |> as_tibble()
  throw_times <- throw_times |> merge(throw_distance, by = c("gameId", "playId"))
    #select(air_time, dist)
  #train_control <- trainControl(method = "cv", number = 5)
  
  #model <- train(air_time ~ dist, data = throw_times, 
  #               method = "lm", trControl = train_control)
  #RMSE      Rsquared  MAE     
  #2.857105  0.5865621  1.993898
  lm(air_time ~ qb_dist, data = throw_times)
}
get_game_context <- function(throws, games) {
  #features:
  # preSnapWinProb, scorediff, absoluteYardlineNumber, time_left, quarter, dropbackType,
  team_info <- games |> select(gameId, homeTeamAbbr, visitorTeamAbbr)
  throws |> merge(team_info, by = "gameId") |> separate_wider_delim(gameClock, delim = ":", 
                                                                    names = c("minute", "second")) |>
    mutate(
    preSnapWinProb = ifelse(possessionTeam == homeTeamAbbr, preSnapHomeTeamWinProbability,
                            preSnapVisitorTeamWinProbability),
    scorediff = ifelse(possessionTeam == homeTeamAbbr, preSnapHomeScore - preSnapVisitorScore,
                       preSnapVisitorScore - preSnapHomeScore),
    time_left = as.numeric(minute) * 60 + as.numeric(second),
    isZone = ifelse(pff_manZone == "Zone", 1, 0) )|> filter(qbSpike == 0) |>
    select(gameId, playId, preSnapWinProb, scorediff, absoluteYardlineNumber, time_left, quarter, isZone, 
           pff_passCoverage)
}

project_movement <- function(sample_frame, throw_model, n = 1) {
  qb_coords <- sample_frame |> filter(position == "QB") |> select(x, y)
  #binary for end of game situations - explicitly encode
  #cross-body throws - orientation and angle of throw
  # Assuming only one QB per frame:
  qb_x <- qb_coords$x[1]
  qb_y <- qb_coords$y[1]
  
  # Compute Euclidean distance from each player to the QB
  sample_frame <- sample_frame |> 
    mutate(qb_dist = sqrt((x - qb_x)^2 + (y - qb_y)^2))
  
  sample_frame <- sample_frame |> 
    mutate(est_time = predict(throw_model, newdata = sample_frame))
  
  sample_frame |> 
    mutate(closestEstDefender = pmap_dbl(
      list(est_time, nflId),
      ~ simulate_movement(..1, ..2, sample_frame, n)
    )) |> 
    rename(!!paste0("closestEstDefender_", n) := closestEstDefender)
}

main <- function(){
  #reading data
  tracking <- lazy_dt(arrow::read_parquet("nfl-big-data-bowl-2025/tracking.parquet"))
  plays <- lazy_dt(read.csv("nfl-big-data-bowl-2025/plays.csv"))
  player_play <- lazy_dt(read.csv("nfl-big-data-bowl-2025/player_play.csv"))
  games <- lazy_dt(read.csv("nfl-big-data-bowl-2025/games.csv"))
  players <- lazy_dt(read.csv("nfl-big-data-bowl-2025/players.csv"))
  #left to right cleaning
  multi_throws <- tracking |> filter(event == "pass_arrived")  |> select(gameId, playId)  |>
    group_by(gameId, playId) |>
    count(nn = n()) |> filter(nn != 23) |> select(gameId, playId) |> as_tibble()
  multi_throws_game = multi_throws$gameId
  multi_throws_play = multi_throws$playId
  
  #get positions and throwing frames
  positions <- players |> select(nflId, position)
  throws <- plays |> filter(passResult %in% c("C", "I", "IN"))
  
  receiver <- player_play |> filter(wasTargettedReceiver == 1) |> 
    select(gameId, playId, nflId) |> rename(targeted_receiver = nflId)
  passing_frames <- tracking |> filter(event == "pass_forward", !(gameId %in% multi_throws_game & playId %in% multi_throws_play)) |> 
    clean_tracking() |> merge(receiver, by  = c("gameId", "playId"))
  
  closest <- passing_frames |> merge(positions, by = "nflId")
  num_closest <- 5
  throw_model <- get_throw_model(tracking, passing_frames)
  #find closest opponent for each player - have to include frameId for multiple throw plays
  for(n in 1:num_closest){
  closest <- closest |>
    as_tibble() |>
    group_by(gameId, playId, frameId) |>
    group_split() |>
    pblapply(function(df) {
      get_nth_closest_opponent(df, n = n)
    }) |>
    bind_rows()
  
  #get motion difference for the closest opponent
  closest <- closest |>
    group_by(gameId, playId, frameId) |>
    group_split() |>
    pblapply(function(df) {
      get_motion_difference(df, n = n)
    }) |> bind_rows()
  
  closest <- closest |>
    group_by(gameId, playId, frameId) |>
    group_split() |>
    pblapply(function(df) {
      project_movement(df, throw_model, n = n) 
    }) |> bind_rows()
  }
  write.csv(closest,"features.csv", row.names = FALSE)
  
  
  #remove multi-throw plays
  motion_feats <- paste0("motion_diff_", 1:num_closest)
  distance_feats <- paste0("closestOpponentDistance_", 1:num_closest)
  est_distance_feats <- paste0("closestEstDefender_", 1:num_closest)
  closest <- closest |> mutate(is_targetted = nflId == targeted_receiver) |> 
    filter(!is.na(motion_diff_1), position %in% c("WR", "RB", "TE", "HB", "RB")) |>
    fastDummies::dummy_cols(select_columns = "position", remove_selected_columns = TRUE)
  
  game_context <- get_game_context(throws, games) |>
    fastDummies::dummy_cols(select_columns = "pff_passCoverage", remove_selected_columns = TRUE)
  closest <- closest |> merge(game_context, by = c("gameId", "playId")) 
  context_feats <- c(colnames(game_context))
  context_feats <- context_feats[!context_feats %in% c("gameId", "playId")]
  
  features = c("qb_dist", motion_feats, distance_feats, est_distance_feats, names_feat, context_feats)
  #
  #cv_top1_acc <- run_xgbRanker_cv(closest, features = features, 
  #                                label = "is_targetted", k = 5, nrounds = 100, top_n = 1)
    
}
