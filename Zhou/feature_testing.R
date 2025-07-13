library(tidyverse)
library(arrow)
library(sportyR)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(deldir)
library(sf)

features <- read.csv("Zhou/features.csv")

source("Zhou/generate_features.R")

tracking <- lazy_dt(arrow::read_parquet("nfl-big-data-bowl-2025/tracking.parquet"))
plays <- lazy_dt(read.csv("nfl-big-data-bowl-2025/plays.csv"))
player_play <- lazy_dt(read.csv("nfl-big-data-bowl-2025/player_play.csv"))
games <- lazy_dt(read.csv("nfl-big-data-bowl-2025/games.csv"))
players <- lazy_dt(read.csv("nfl-big-data-bowl-2025/players.csv"))

features

# test closest Opponent
throwing_frames <- tracking |> filter(event == "pass_forward")

