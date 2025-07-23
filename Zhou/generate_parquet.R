#turns csvs into more efficient parquet
library(arrow)
library(tidyverse)
tracking <- read_csv("nfl-big-data-bowl-2025/tracking_week_1.csv") |>
  bind_rows(read_csv("nfl-big-data-bowl-2025/tracking_week_2.csv")) |>
  bind_rows(read_csv("nfl-big-data-bowl-2025/tracking_week_3.csv")) |>
  bind_rows(read_csv("nfl-big-data-bowl-2025/tracking_week_4.csv")) |>
  bind_rows(read_csv("nfl-big-data-bowl-2025/tracking_week_5.csv")) |>
  bind_rows(read_csv("nfl-big-data-bowl-2025/tracking_week_6.csv")) |>
  bind_rows(read_csv("nfl-big-data-bowl-2025/tracking_week_7.csv")) |>
  bind_rows(read_csv("nfl-big-data-bowl-2025/tracking_week_8.csv")) |>
  bind_rows(read_csv("nfl-big-data-bowl-2025/tracking_week_9.csv"))
arrow::write_parquet(tracking, "nfl-big-data-bowl-2025/tracking.parquet")
