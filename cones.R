# N defenders in traffic plot
library(tidyverse)
library(gganimate)
library(geosphere)  # or sf if you're more familiar

gameid2 = 2022091111
playid2 = 923

theplay2 <- pass_tracking |> filter(gameId == gameid2, playId == playid2) |> 
  inner_join(select(player_play, gameId, playId, nflId, wasRunningRoute, wasTargettedReceiver), by = c('gameId', 'playId', 'nflId'))


library(dplyr)
library(tidyr)
library(purrr)

receivers <- theplay2 %>% filter(wasRunningRoute == TRUE)
defenders <- theplay2 %>% filter(club != "LAC")

library(dplyr)

# Split out defenders
defenders <- theplay2 |> 
  filter(club != "LAC") |> 
  select(gameId, playId, frameId, nflId, x, y, -dir)

# Join receivers to defenders in same frame
receiver_defender_pairs <- receivers |> 
  select(gameId, playId, frameId, receiverId = nflId, rx = x, ry = y, dir) |> 
  left_join(defenders, by = c("gameId", "playId", "frameId")) |> 
  rename(defId = nflId, dx = x, dy = y)

receiver_defender_pairs <- receiver_defender_pairs %>%
  mutate(
    dist = sqrt((dx - rx)^2 + (dy - ry)^2),
    angle_to_def = atan2(dy - ry, dx - rx) * 180 / pi,
    angle_to_def = (angle_to_def + 360) %% 360,
    angle_diff = (angle_to_def - dir + 360) %% 360,
    in_front = angle_diff <= 90 | angle_diff >= 270,
    in_sector = dist <= 5 & in_front
  )

defenders_in_sector <- receiver_defender_pairs %>%
  group_by(gameId, playId, frameId, receiverId) %>%
  summarise(defenders_in_sector = sum(in_sector), .groups = "drop") |> 
  rename(nflId = receiverId)

receivers <- left_join(receivers, defenders_in_sector, by = c('gameId', 'playId', 'frameId', 'nflId'))

library(ggplot2)
library(gganimate)
library(dplyr)
library(ggforce)    # for drawing arcs / semicircles

tracking_with_pressure <- receivers |> 
  mutate(
    pressure_color = case_when(
      defenders_in_sector == 0 ~ "green",
      defenders_in_sector == 1 ~ "yellow",
      defenders_in_sector >= 2 ~ "red",
      TRUE ~ "gray"
    )
  )
# Prepare just one play for clarity
play_id <- 1234  # replace with your actual play ID
game_id <- 2022090800  # your gameId

# Subset for animation
play_data <- theplay2 |> 
  select(-defenders_in_sector)

receiver_data <- tracking_with_pressure 

# Function to compute semicircle arc coordinates
draw_cones <- receiver_data %>%
  mutate(
    start_angle = (dir - 45) %% 360,
    end_angle = (dir + 45) %% 360,
    r0 = 0,
    r = 5
  )



# Convert angles to radians for ggforce
draw_cones <- draw_cones %>%
  mutate(
    start = start_angle * pi / 180,
    end = end_angle * pi / 180
  )

library(ggplot2)

field <- ggplot() +
  # Field rectangle
  geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 53.3), fill = "darkgreen") +
  # Yard lines every 10 yards
  geom_segment(aes(x = seq(10, 110, by = 10), xend = seq(10, 110, by = 10), y = 0, yend = 53.3),
               color = "white", linetype = "dashed") +
  # End zones
  geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 53.3), fill = "blue", alpha = 0.3) +
  geom_rect(aes(xmin = 110, xmax = 120, ymin = 0, ymax = 53.3), fill = "red", alpha = 0.3) +
  coord_fixed() +
  theme_void()

field

p <- ggplot() +
  geom_point(data = play_data %>% filter(club == "LAC"), aes(x = x, y = y), color = "blue", alpha = 0.6) +
  geom_point(data = play_data %>% filter(club != "LAC", wasRunningRoute == FALSE), aes(x = x, y = y), color = "gray") +
  geom_arc_bar(
    data = draw_cones,
    aes(
      x0 = x,
      y0 = y,
      r0 = r0,
      r = r,
      start = start,
      end = end,
      fill = pressure_color
    ),
    alpha = 0.5,
    color = NA
  ) +
  geom_point(data = receiver_data, aes(x = x, y = y), color = "black", size = 3) +
  coord_fixed() +
  scale_fill_identity() +
  theme_minimal() +
  labs(title = "Frame: {closest_state}", x = "X", y = "Y") +
  transition_states(frameId, transition_length = 1, state_length = 1, wrap = FALSE)

animate(
  p,
  fps = 10,
  width = 800,
  height = 500,
  renderer = magick_renderer()
)


receiver_data <- receiver_data %>%
  mutate(
    start = dir * pi / 180 - pi/4,
    end = dir * pi / 180 + pi/4,
    r0 = 0,
    r = 5,
    pressure_color = case_when(
      defenders_in_sector == 0 ~ "green",
      defenders_in_sector == 1 ~ "yellow",
      defenders_in_sector >= 2 ~ "red",
      TRUE ~ "gray"
    )
  )


p <- ggplot() +
  # All players
  geom_point(data = play_data, aes(x = x, y = y, color = side), size = 2) +
  
  # Semicircle arcs in front of receivers
  geom_arc_bar(
    data = receiver_data,
    aes(
      x0 = x, y0 = y,
      r0 = 0, r = 5,
      start = start, end = end,
      fill = pressure_color
    ),
    alpha = 0.4, color = NA
  ) +
  
  # Receiver markers
  geom_point(data = receiver_data, aes(x = x, y = y), color = "black", size = 3) +
  
  scale_fill_identity() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Frame: {closest_state}", x = "X", y = "Y") +
  transition_states(frameId, transition_length = 1, state_length = 1, wrap = FALSE)

animate(
  p,
  fps = 10,
  width = 800,
  height = 500,
  renderer = gifski_renderer("receiver_danger.gif")
)


