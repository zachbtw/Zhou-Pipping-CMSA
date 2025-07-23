#build feature set
library(tidyverse)
library(data.table)
library(dtplyr)
library(pbapply)
library(xgboost)
library(caret)

clean_tracking <- function(tracking) {
  #cleans tracking data so that all is standardized
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

get_top_n_closest_opponents <- function(pass_frame, n = 1) {
  #gets the n closest opponents by cross product
  player_pos <- pass_frame |> dplyr::filter(club != "football")
  teams <- unique(player_pos$club)
  
  team_1_pos <- player_pos |> filter(club == teams[1]) |> select(nflId, x, y, o, s_x, s_y)
  team_2_pos <- player_pos |> filter(club == teams[2]) |> select(nflId, x, y, o, s_x, s_y)
  
  cross_df <- team_1_pos |> 
    cross_join(team_2_pos, suffix = c("_1", "_2")) |>
    mutate(
      distance = sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2),
      change_x = abs(x_1 - x_2),
      change_y = abs(y_1 - y_2),
      o_diff = abs(o_1 - o_2),
      motion_diff_x = (x_1 + s_x_1) - (x_2 + s_x_2),
      motion_diff_y = (y_1 + s_y_1) - (y_2 + s_y_2),
      motion_diff = sqrt(motion_diff_x^2 + motion_diff_y^2)
    )
  get_top_n <- function(group_col, id_col_self, id_col_opp) {
    cross_df |> 
      group_by(.data[[group_col]]) |>
      arrange(distance, .by_group = TRUE) |>
      slice_head(n = n) |>
      mutate(rank = row_number()) |>
      ungroup() |>
      transmute(
        nflId = .data[[id_col_self]],
        rank = rank,
        closestOpponentId = .data[[id_col_opp]],
        closestOpponentDistance = distance,
        closestOpponentX = change_x,
        closestOpponentY = change_y,
        closestOpponentO = o_diff,
        closestOpponent_XSpeed = motion_diff_x,
        closestOpponent_YSpeed = motion_diff_y,
        closestOpponent_SDiff= motion_diff,
      )
  }
  
  closest_1 <- get_top_n("nflId_1", "nflId_1", "nflId_2")
  closest_2 <- get_top_n("nflId_2", "nflId_2", "nflId_1")
  closest_all <- bind_rows(closest_1, closest_2) |>
    pivot_wider(
      id_cols = nflId,
      names_from = rank,
      values_from = c(closestOpponentId, closestOpponentDistance, closestOpponentX, closestOpponentY, closestOpponentO,
                      closestOpponent_XSpeed, closestOpponent_YSpeed, closestOpponent_SDiff),
      names_glue = "{.value}_{rank}"
    )
  
  tracking_out <- pass_frame |>
    left_join(closest_all, by = "nflId")
  return(tracking_out)
}


simulate_movement <- function(n, player, frame, closest_n = 1) {
  #get point estimate for future position using speed
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
  #generates linear relationship between player distance and aerial throw time to get point estimates
  #of future position
  
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
    isZone = ifelse(pff_manZone == "Zone", 1, 0)
    ) |> filter(qbSpike == 0) |>
    select(gameId, playId, preSnapWinProb, scorediff, absoluteYardlineNumber, time_left, quarter, isZone, yardsToGo,
           down)
}

get_qb_angles <- function(sample_frame) {
  #gets qb movement vector features
  qb_coords <- sample_frame |> filter(position == "QB") |> select(x, y)
  
  qb_x <- qb_coords$x[1]
  qb_y <- qb_coords$y[1]
  
  sample_frame <- sample_frame |> 
    mutate(qb_x_diff = x - qb_x,
           qb_y_diff = y - qb_y,
           qb_dist = sqrt(qb_x_diff^2 + qb_y_diff^2),
           qb_deg = atan2(qb_y_diff, qb_x_diff) * 180 / pi)
}

project_movement <- function(sample_frame, throw_model, n = 1) {
  #apply movement projections functions to entire group 
  qb_coords <- sample_frame |> filter(position == "QB") |> select(x, y)
  qb_x <- qb_coords$x[1]
  qb_y <- qb_coords$y[1]
  
  sample_frame <- sample_frame |> 
    mutate(qb_x_diff = x - qb_x,
           qb_y_diff = y - qb_y,
           qb_dist = sqrt(qb_x_diff^2 + qb_y_diff^2))
  
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
  #remove plays with multiple throws
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

  #find closest opponent for each player - have to include frameId for multiple throw plays
  closest <- closest |>
    group_by(gameId, playId, frameId) |>
    group_split() |>
    pblapply(function(df) {
      get_top_n_closest_opponents(df, n = num_closest)
    }) |> bind_rows()
  
  #get qb differences
  closest <- closest |>
    group_by(gameId, playId, frameId) |>
    group_split() |>
    pblapply(function(df) {
      get_qb_angles(df) 
    }) |> bind_rows()
  
  
  #add positions
  closest <- closest |> mutate(is_targetted = nflId == targeted_receiver) |> 
    filter(!is.na(closestOpponentId_1), position %in% c("WR", "RB", "TE", "HB", "RB")) |>
    fastDummies::dummy_cols(select_columns = "position", remove_selected_columns = TRUE)
  
  #add game context
  game_context <- get_game_context(throws, games) 
  
  
  #small custom features
  closest <- closest |> merge(game_context, by = c("gameId", "playId")) |>
    mutate(
      givesFirst = yardsToGo < qb_dist,
      lastPlay = down %in% c(3,4)
    )
  
  #get if under pressure
  pressures <- player_play |> group_by(gameId, playId) |>
    summarise(under_pressure = mean(causedPressure) > 0) |> as_tibble()

  closest <- closest |> merge(pressures, on = c("gameId", "playId"))
  
  
  #reception count as a proxy for receiver skill - unused
  reception_count <- player_play |> group_by(nflId) |> 
    summarise(targetTotal = sum(wasTargettedReceiver))
  closest <- closest |> merge(reception_count, on = "nflId")
  
  
  write.csv(closest,"Zhou/features.csv", row.names = FALSE)
}
