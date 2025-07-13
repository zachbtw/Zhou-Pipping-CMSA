library(testthat)
library(tidyverse)
library(arrow)
library(sportyR)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(deldir)
library(sf)
set.seed(123)
features <- read.csv("Zhou/features.csv")

source("Zhou/generate_features.R")

tracking <- lazy_dt(arrow::read_parquet("nfl-big-data-bowl-2025/tracking.parquet"))
plays <- lazy_dt(read.csv("nfl-big-data-bowl-2025/plays.csv"))
player_play <- lazy_dt(read.csv("nfl-big-data-bowl-2025/player_play.csv"))
games <- lazy_dt(read.csv("nfl-big-data-bowl-2025/games.csv"))
players <- lazy_dt(read.csv("nfl-big-data-bowl-2025/players.csv"))

positions <- players |> select(nflId, position)

# test closest Opponent
throwing_frames <- tracking |> filter(event == "pass_forward") |> clean_tracking()

get_qb_dist <- function(s_gameId, s_playId, s_nflId) {
  frame <- throwing_frames |> filter(gameId == s_gameId, playId == s_playId) |> as_tibble() |>
    merge(positions, by = c("nflId"))
  player_locs <- frame |> filter(nflId == s_nflId) |> select(x, y) 
  player_x = player_locs$x[1]
  player_y = player_locs$y[1]
  qb_loc <- frame |> filter(position == "QB") |> select(x, y)
  qb_x = qb_loc$x[1]
  qb_y = qb_loc$y[1]
  sqrt((player_x - qb_x)**2 + (player_y - qb_y)**2)
}

sim_movement_test <- function(s_gameId, s_playId, s_nflId, s_time, top_n = 11) {
  frame <- throwing_frames |> filter(gameId == s_gameId, playId == s_playId) |> as_tibble()
  frame <- frame |> mutate(
    x = x + s_x * (s_time / 10),
    y = y + s_y * (s_time / 10)
  )
  get_opponent_distance_motion(s_gameId, s_playId, s_nflId, top_n, "distance", frame)
}

get_opponent_distance_motion <- function(s_gameId, s_playId, s_nflId, top_n = 11, type = "distance", frame = NA) {
  #slow function that we know is correct, used to compare against more efficient, vectorized version
  if(identical(frame, NA)) {
    frame <- throwing_frames |> filter(gameId == s_gameId, playId == s_playId) |> as_tibble()
  }
  player_locs <- frame |> filter(nflId == s_nflId) |> select(x, y, s_x, s_y, club) 
  player_x = player_locs$x[1]
  player_y = player_locs$y[1]
  player_sx = player_locs$s_x[1]
  player_sy = player_locs$s_y[1]
  p_club = player_locs$club[1]
  opponents <- frame |> filter(club != p_club, !is.na(nflId)) |>
    mutate(dist = sqrt((x - player_x)**2 + (y - player_y)**2),
           speed_diff = (s_x - player_sx)^2 + (s_y - player_sy)^2) |>
    arrange(dist) |> head(top_n)
  if(type == "distance") {
    return(opponents |> select(dist))
  }
  return(opponents |> select(speed_diff))
}

test_that("qb_dist", {
  n_tests <- 20
  pb <- txtProgressBar(min = 0, max = n_tests, style = 3)
  test_outputs = rep(NA, n_tests)
  for(i in 1:n_tests){
    rand_row = features[sample(nrow(features), 1), ]
    s_gameId <- rand_row$gameId[1]
    s_playId <- rand_row$playId[1]
    s_nflId <- rand_row$nflId[1]
    s_qb_dist <- rand_row$qb_dist[1]
    actual <- get_qb_dist(s_gameId, s_playId, s_nflId)
    test_outputs[i] <- all.equal(actual, s_qb_dist, tolerance = 1e-6)#floating point addition :)
    setTxtProgressBar(pb, i)
  }
  expect_true(all(test_outputs))
  close(pb)
})

test_that("test_projected_movement", {
  top_n <- 5
  col_names = "closestEstDefender_"
  cols <- paste0(col_names, 1:top_n)
  n_tests <- 20
  p_features <- features |> filter(position %in% c("WR", "RB", "FB", "HB", "TE"))
  pb <- txtProgressBar(min = 0, max = n_tests, style = 3)
  test_outputs = rep(NA, n_tests)
  for (i in 1:n_tests) {
    rand_row = p_features[sample(nrow(p_features), 1), ]
    s_gameId <- rand_row$gameId[1]
    s_playId <- rand_row$playId[1]
    s_nflId <- rand_row$nflId[1]
    s_time <- rand_row$est_time[1]
    testing_dist <- p_features |> filter(gameId == s_gameId, playId == s_playId, nflId == s_nflId) |>
      select(all_of(cols)) |> t()
    actual_dist <- sim_movement_test(s_gameId, s_playId, s_nflId, s_time, top_n)
    d1 <- as.numeric(unlist(actual_dist))
    d2 <- as.numeric(testing_dist)
    test_outputs[i] = all.equal(d1, d2, tolerance = 1e-6)
    setTxtProgressBar(pb, i)
  }
  expect_true(all(test_outputs))
  close(pb)
})

test_that("test_random_player_dist", {
  top_n <- 5
  type = "motion"
  col_names = ifelse(type == "distance", "closestOpponentDistance_", "motion_diff_")
  cols <- paste0(col_names, 1:top_n)
  n_tests <- 20
  
  pb <- txtProgressBar(min = 0, max = n_tests, style = 3)
  test_outputs = rep(NA, n_tests)
  for (i in 1:n_tests) {
    rand_row = features[sample(nrow(features), 1), ]
    s_gameId <- rand_row$gameId[1]
    s_playId <- rand_row$playId[2]
    s_nflId <- rand_row$nflId[3]
    testing_dist <- features |> filter(gameId == s_gameId, playId == s_playId, nflId == s_nflId) |>
      select(all_of(cols)) |> t()
    actual_dist <- get_opponent_distance_motion(s_gameId, s_playId, s_nflId, top_n, type)
    d1 <- as.numeric(unlist(actual_dist))
    d2 <- as.numeric(testing_dist)
    test_outputs[i] = all.equal(d1, d2, tolerance = 1e-6)
    setTxtProgressBar(pb, i)
  }
  expect_true(all(test_outputs))
  close(pb)
})




