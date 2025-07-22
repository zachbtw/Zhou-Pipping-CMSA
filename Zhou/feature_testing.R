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

predictions <- read.csv("")

# test closest Opponent
features |>
  group_by(gameId, playId) |>
  filter(
    closestOpponentDistance_1 == max(closestOpponentDistance_1, na.rm = TRUE) |
      closestOpponentDistance_2 == max(closestOpponentDistance_2, na.rm = TRUE)
  ) |>
  summarise(flag = as.integer(any(is_targetted == 1)), .groups = "drop") |>
  summarise(mean_flag = mean(flag))

#throw to player who is most open - 32% of time
# throw to top-2 player who is most open 46.7% of time

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
passing_frames <- tracking |> filter(event == "pass_arrived", !(gameId %in% multi_throws_game & playId %in% multi_throws_play)) |> 
  clean_tracking() |> merge(receiver, by  = c("gameId", "playId"))

throw_frames <- tracking |> filter(event == "pass_forward")

closest <- passing_frames |> merge(positions, by = "nflId")
num_closest <- 5
#throw_model <- get_throw_model(tracking, passing_frames)
#find closest opponent for each player - have to include frameId for multiple throw plays
closest <- closest |>
  group_by(gameId, playId, frameId) |>
  group_split() |>
  pblapply(function(df) {
    get_top_n_closest_opponents(df, n = num_closest)
  }) |> bind_rows()

at_catch <- closest |> filter(targeted_receiver == nflId) |> select(gameId, playId, nflId, closestOpponentDistance_1)

throw_frames <- throw_frames |> clean_tracking() |> as_tibble() |> 
  group_by(gameId, playId, frameId) |>
  group_split() |>
  pblapply(function(df) {
    get_top_n_closest_opponents(df, n = num_closest)
  }) |> bind_rows()

at_throw <- throw_frames |> select(gameId, playId, nflId, closestOpponent_SDiff_1, closestOpponentDistance_1)

corr <- at_catch |> merge(at_throw , by = c("gameId", "playId", "nflId"),
                          suffix = c("_catch", "_throw"))

corr_plot <- corr |> ggplot(aes(x = closestOpponent_SDiff_1, y = closestOpponentDistance_1_catch)) +
  geom_point() + labs(
    title = "",
    subtitle = paste0("Pearson Correlation: ", round(cor(corr$closestOpponentDistance_1_catch, corr$closestOpponent_SDiff_1), 4))
  ) + xlab("Magnitude Between Speed Vectors at Throw") + ylab("Distance from Closest Defender at Reception")
ggsave("Zhou/Final/Assets/corr_plot_title.png")

u <- c(3, 4)
v <- c(1, 2)
diff <- u - v  # u - v = (2, 2)

# Create a data frame for all three vectors
vectors <- data.frame(
  x = c(2, 0, 1),
  y = c(0, 0, 2),
  xend = c(u[1], v[1], 3),
  yend = c(u[2], v[2], 4),
  label = c("Player Movement Vectors", "Player Movement Vectors", "Difference")
)
points <- data.frame(
  x = c(0,2),
  y = c(0, 0)
)
# Plot
close <- ggplot(vectors) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = label),
               arrow = arrow(length = unit(0.1, "inches")), size = 0.6) +
  geom_point(aes(x = x, y = y), data = points) + 
  xlim(-2,5) +
  ylim(-2, 5) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Magnitude of Difference of Speed Vectors", color = "Vectors:") +
  scale_color_manual(values = c("Player Movement Vectors" = "black", "Difference"= "red")) +
  theme(plot.title = element_markdown(hjust = 0.5), legend.position = "bottom") 

cor(corr$closestOpponentDistance_1_throw, corr$closestOpponentDistance_1_catch)
#using current separation as a proxy: 0.7277
#using separation + speeds: 0.811