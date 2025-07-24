# Visualizations for paper
library(gt)
library(gtExtras)
library(dplyr)
library(dtplyr)
library(rlang)
library(sportyR)
#install.packages('ggnewscale')
library(ggnewscale)
library(flexclust)
library(stringr)
library(gganimate)
library(purrr)
library(ggtext)
library(patchwork)
library(magick)
#install.packages('ggforce')
library(ggforce)
library(dtplyr)

tracking <- lazy_dt(arrow::read_parquet("nfl-big-data-bowl-2025/tracking.parquet"))
rec_qb_distances <- read.csv("Zhou/Features-Results/theplay.csv")
pass_tracking_recreated <- read.csv("Zhou/Features-Results/passtracking.csv")


gameid = 2022103000
playid = 1395
nflIds = c(42358, 46109, 38605)

subset <- tracking |> filter(gameId == gameid, playId == playid, nflId %in% nflIds)



samplegame3 <- gameid
sampleplay3 <- playid

field_params <- list(
  field_apron = "springgreen3",
  field_border = "springgreen3",
  offensive_endzone = "springgreen3",
  defensive_endzone = "springgreen3",
  offensive_half = "springgreen3",
  defensive_half = "springgreen3"
)
library(ggtext)
feature <- rec_qb_distances |> 
  select(frameId, qb_vision, nflId = qbId)
vision_play <- filter(pass_tracking_recreated, gameId == samplegame3, playId == sampleplay3)
vision_play <- left_join(vision_play, feature, by = c('frameId', 'nflId'))

dist_to_qb_vision <- function(df) {
  df %>%
    mutate(
      v_x = sin(qb_vision * pi / 180),
      v_y = cos(qb_vision * pi / 180),
      dx = rec_x - qb_x,
      dy = rec_y - qb_y,
      t_proj = dx * v_x + dy * v_y,  # projection along vision line
      qb_vision_dist = (dx * v_y - dy * v_x)  # signed perpendicular distance
    )
}
source("Zhou/generate_features.R")
qb_data <- subset |> as_tibble() |> filter(nflId == 38605) |> clean_tracking() |>
  select(frameId, qb_x = x , qb_y  = y, o) |>
  mutate(qb_vision = o - 25)
  
subset <- subset |> filter(nflId %in% c(42358, 46109)) |> 
  merge(qb_data, by = "frameId")


subsetdiff <- select(subset, frameId, nflId, event, qb_x, qb_y, rec_x = x, rec_y = y, qb_vision) |> 
  mutate(qb_vision = ifelse(qb_vision >= 360, qb_vision %% 360, qb_vision)) |> 
  dist_to_qb_vision() |> 
  mutate(qb_vision_dist, abs(qb_vision_dist))

subsetdiff_by_frame <- subsetdiff %>%
  arrange(frameId)  # optional: sort by frame

library(dplyr)

closest_per_frame <- subsetdiff %>%
  group_by(frameId) %>%
  slice_min(abs(qb_vision_dist), n = 1, with_ties = FALSE) %>%
  ungroup()



wide_distances <- subsetdiff %>%
  pivot_wider(names_from = nflId, values_from = `abs(qb_vision_dist)`)


snap_frame <- unique(filter(vision_play, event == "ball_snap")$frameId)


snap_frame <- subset |> filter(event == "ball_snap") |> select(frameId) |>
  first() |> as.integer()

ball_release <- subset |> filter(event == "pass_forward") |> select(frameId) |>
  first() |> as.integer()

catch_frame <- subset |> filter() |> select(frameId) |>
  first() |> as.integer()

add_pauses <- function(df){
  df <- bind_rows(
    df,
    map_dfr(1:50, ~ df|> filter(event == "ball_snap"))
  ) |> arrange(frameId) |>
    mutate(new_frameId = cumsum(is.na(nflId)))
  
  df <- bind_rows(
    df,
    map_dfr(1:50, ~ df|> filter(event == "pass_forward"))
  ) |> arrange(frameId) |>
    mutate(new_frameId = cumsum(is.na(nflId)))
}


vision_play <- add_pauses(vision_play)
point_color_map <- c(
  "DEN" = "orange",
  "JAX" = "lightblue",
  "football" = "#654321",
  "42358" = "#9B870C",
  "46109" = "red"
)
vision_play <- vision_play |> mutate(
  club = ifelse(nflId %in% c(42358, 46109), nflId, club)
)
field_plot <- ggplot(vision_play, aes(x = x, y = y)) +
  geom_football(league = "NFL", x_trans = 60, y_trans = 26.6667) +
  geom_point(data = vision_play, aes(color = club, x = x, y = y), size = 3) +
  geom_segment(
    data = filter(vision_play, position == "QB", frameId >= snap_frame),
    aes(
      x = x,
      y = y,
      xend = (x + 30 * sin(qb_vision * pi / 180)),
      yend = (y + 30 * cos(qb_vision * pi / 180)),
      group = interaction(gameId, playId, frameId)
    )
  ) +
  scale_color_manual(values = point_color_map) +
  labs(title = "R.Wilson pass short right to C.Sutton") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(hjust = 0.5, size = 16, family = "helvetica"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 10, family = "helvetica")
  ) +
  transition_states(new_frameId, transition_length = 1, state_length = 1)


filtered_distances <- rec_qb_distances %>% 
  filter(recId %in% c(42358, 46109)) |> 
  select(frameId, recId, qb_vision_dist)
#NA, qb_vision_dist)
filtered_distances <- filtered_distances |> 
  arrange(recId, frameId)

add_pauses <- function(df){
  df <- bind_rows(
    df,
    map_dfr(1:50, ~ df|> filter(frameId == snap_frame))
  ) |> arrange(frameId)
  
  df <- bind_rows(
    df,
    map_dfr(1:50, ~ df|> filter(frameId == ball_release))
  ) 
  
  df %>%
    group_by(frameId, recId) %>%
    mutate(row_num = row_number()) %>%  # count within recId
    ungroup() %>%
    arrange(frameId, row_num, recId) %>%  # interleave recIds
    mutate(new_frameId = (row_number() - 1) %/% 2 + 1)
}
rec_colors = c(
"42358" = "#9B870C",
"46109" = "red")

max_dist = max(abs(filtered_distances$qb_vision_dist), na.rm = TRUE)
filtered_distances <- add_pauses(filtered_distances)
line_plot <- filtered_distances |> ggplot() +
  geom_line(aes(x = frameId, y = abs(qb_vision_dist), color = as.character(recId), group = recId), size = 1) +
  scale_color_manual(values = rec_colors) + 
  geom_vline(xintercept = snap_frame, linetype = "dashed", color = "red") +
  geom_vline(xintercept = ball_release, linetype = "dashed", color = "red") +
  annotate("text", x = snap_frame, 
           y = max_dist * 0.8,
           label = "Ball Snap", color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -0.3) +
  annotate("text", x = ball_release, 
           y = max_dist * 0.9,
           label = "Ball Throw", color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -0.3) +
  labs(
    title = "Distance from QB Vision Line Over Time",
    x = "Frame Number",
    y = "Absolute Distance",
    color = "Player ID"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  transition_reveal(new_frameId)




animated_line <- animate(line_plot, fps = 10, width = 600, height = 400, renderer = magick_renderer())


animated_field <- animate(field_plot, fps = 10, width = 600, height = 400, renderer = magick_renderer())

new_gif <- image_append(c(animated_field[1], animated_line[1]))
#combine animations
for(i in 2:length(animated_field)) {
  combined <- image_append(c(animated_field[i], animated_line[i]))
  new_gif <- c(new_gif, combined)
}

image_write(new_gif, path = "Zhou/Final/Assets/vision_example.gif")
