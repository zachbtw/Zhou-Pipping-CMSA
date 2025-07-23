library(tidyverse)
library(arrow)
library(sportyR)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(deldir)
library(sf)
library(gganimate)
#reading data
tracking <- lazy_dt(arrow::read_parquet("nfl-big-data-bowl-2025/tracking.parquet"))
plays <- lazy_dt(read.csv("nfl-big-data-bowl-2025/plays.csv"))
player_play <- lazy_dt(read.csv("nfl-big-data-bowl-2025/player_play.csv"))
games <- lazy_dt(read.csv("nfl-big-data-bowl-2025/games.csv"))
players <- lazy_dt(read.csv("nfl-big-data-bowl-2025/players.csv"))

positions <- players |> select(nflId, position)
throws <- plays |> filter(passResult %in% c("C", "I", "IN"))
passing_frames <- tracking |> filter(event == "pass_forward")
receiver <- player_play |> filter(wasTargettedReceiver == 1) |> 
  select(gameId, playId, nflId) |> rename(targeted_receiver = nflId)

passing_frames <- passing_frames |>
  merge(receiver, by  = c("gameId", "playId"))


teams <- throws |>
  select(gameId, playId, possessionTeam, defensiveTeam)

#voronoi areas as spacing
get_voronoi_areas <- function(play) {
  play <- play |> filter(club != "football")
  #clipping to players to be in-bounds, rarish  and relatively unimpactful
  #edge case but should be dealt with later
  play <- play |>
    mutate(
      x = pmin(pmax(x, 0), 120),
      y = pmin(pmax(y, 0), 53.3)
    )
  
  vor <- deldir(play$x, play$y, rw = c(0, 120, 0, 53.3))
  tiles <- tile.list(vor)
  vor_polygons <- lapply(seq_along(tiles), function(i) {
    tile <- tiles[[i]]
    coords <- cbind(tile$x, tile$y)
    coords <- rbind(coords, coords[1, ])
    st_polygon(list(coords))
  })
  
  vor_sf <- st_sf(
    nflId = play$nflId,
    club = play$club,
    geometry = st_sfc(vor_polygons)
  )
  
  # Clip to field
  field <- st_polygon(list(matrix(c(
    0, 0,
    120, 0,
    120, 53.3,
    0, 53.3,
    0, 0
  ), ncol = 2, byrow = TRUE))) |> st_sfc()
  
  #vor_sf <- st_intersection(vor_sf, st_sf(geometry = field))
  
  vor_sf <- vor_sf |>
    mutate(area = as.numeric(st_area(geometry))) |>
    select(nflId, area)
  
  play |> merge(vor_sf, by = "nflId")
}


#get voronoi areas
passing_frames <- passing_frames |>
  merge(teams, by = c("gameId", "playId")) |> merge(positions, on = "nflId") |>
  filter(!(position %in% c("T", "G", "C"))) |>
  group_by(gameId, playId) |>
  group_modify(~ get_voronoi_areas(.x)) |>
  ungroup()
  
max_separation_per_play <- passing_frames |>
  filter(position %in% c("TE", "RB", "WR", "FB")) |>
  group_by(gameId, playId) |>
  slice_max(order_by = area, n = 1, with_ties = FALSE) |>
  mutate(targetted_most_open = any(nflId == targeted_receiver)) |>
  ungroup()


mean(max_separation_per_play$targetted_most_open) 
#target most "open" less than 1/3 of time(0.2332)
#target top 2 most "open" 50.4% of the time

random_guess <- passing_frames |> merge(positions, on = "nflId") |>
  filter(position %in% c("TE", "RB", "WR", "FB")) |>
  group_by(gameId, playId) |>
  summarize(num_options = n())

mean(random_guess$num_options)#on throws, 5.3 possible receiving options

passing_frames |> filter(nflId == targeted_receiver) |> arrange(area) |> 
  merge(plays, on = c("gameId", "playId")) |> 
    filter(passResult == "C") |> select(gameId, playId) |> head(10)


library(gganimate)
library(colorspace)
#animate voronoi diagrams and spacing
animate_voronoi <- function(viz_game, viz_play, team1, team1_color, team2, team2_color,
                            plotPlay = TRUE) {
  recipient <- player_play |> filter(gameId == viz_game, playId == viz_play, wasTargettedReceiver) |>
    pull(nflId) |> first() |> as.character()
  play_tracking <- tracking |> filter(gameId == viz_game, playId == viz_play)
  
  description <- plays |> filter(gameId == viz_game, playId == viz_play) |>
    pull(playDescription) |> first() |> as.character()
  
  throw_time <- play_tracking |> filter(event == "pass_forward") |>
    pull(frameId) |>
    first() |>
    as.integer()
  
  catch_time <- play_tracking |> filter(event == "pass_arrived") |>
    pull(frameId) |>
    first() |>
    as.integer()

  snap_time <- play_tracking |> filter(event == "ball_snap") |>
    pull(frameId) |>
    first() |>
    as.integer()
  
  viz_tracking <- play_tracking |> mutate(
    club = ifelse(nflId == recipient, "recipient", club),
    club = ifelse(is.na(club), "football", club)
  ) |> left_join(positions, by = "nflId")  |> as_tibble() |>
    filter(frameId > snap_time - 10)
  
  
  viz_tracking <- viz_tracking |> arrange(frameId)
  with_dia <- viz_tracking |> filter(!(position %in% c("T", "G", "C"))) |> group_by(frameId) |> 
    group_modify(\(.x, .y) {
      print(glue::glue("\nProcessing frame: { .y$frameId }")) 
      get_voronoi_areas(.x)
    }) 
  team_colors <- setNames(c(team1_color, team2_color), c(team1, team2))
  
  point_color_map <- c(
    recipient = "#222222",
    football = "#654321",
    team_colors
  )
  team_colors <- setNames(c(lighten(team1_color, .3), lighten(team2_color, .3)), c(team1, team2))
  dia_color_map <- c(
    recipient = "pink",
    football = "#654321",
    team_colors
  )
  
  ordered_frames <- sort(unique(viz_tracking$frameId))
  viz_tracking$frameId <- factor(viz_tracking$frameId, levels = ordered_frames)
  with_dia$frameId <- factor(with_dia$frameId, levels = ordered_frames)
  
  frame_ids <- sort(unique(viz_tracking$frameId))
  
  catch_index <- which(frame_ids == catch_time)
  throw_index <- which(frame_ids == throw_time)
  snap_index <- which(frame_ids == snap_time)
  #generate pauses
  state_lengths = rep(1, length(frame_ids))
  state_lengths[c(catch_index, throw_index, snap_index)] = 900
  if(plotPlay) {
  static_plot <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.6667) +
    geom_sf(data = with_dia, aes(fill = club, geometry = geometry), color = "black", alpha = 0.4) +
    geom_point(data = viz_tracking, aes(x = x, y = y, color = club), size = 3) +
    scale_fill_manual(values = dia_color_map) +
    scale_color_manual(values = point_color_map) +
    coord_sf(xlim = c(0, 120), ylim = c(0, 53.3), expand = FALSE) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title =  paste(description," - Frame: {closest_state}"))

  animated_plot <- static_plot +
    transition_states(
      states = frameId,
      transition_length = 0,
      state_length = state_lengths,
      wrap = FALSE
    )
  
  } else{
    receiver_frames <- with_dia |> filter(nflId == recipient)
    display_name <- receiver_frames |> select(displayName) |> first() |> as.character()
    pause_frames <- c(catch_time, throw_time, snap_time)
    
    #add pause
    receiver_frames <- receiver_frames |>
      mutate(frameId = as.numeric(as.character(frameId))) |>
      mutate(show_time = case_when(
        frameId %in% pause_frames ~ 14,
        TRUE                      ~ 1
      )) |>
      tidyr::uncount(show_time) |>
      arrange(frameId) 

    receiver_frames$reveal_time = 1:nrow(receiver_frames) 
    #add lines which only occur at certain frames in line plot
    receiver_frames <- receiver_frames |>
      mutate(show_throw = ifelse(frameId >= throw_time, 1, 0),
             show_catch = ifelse(frameId >= catch_time, 1, 0),
             show_snap = ifelse(frameId >= snap_time, 1, 0))

    animated_plot <- receiver_frames |>
      ggplot(aes(x = frameId, y = area, group = 1)) +
      geom_line() + geom_point() + 
      geom_vline(aes(xintercept = snap_time, alpha = show_snap),
                 color = "red", linetype = "dashed") + 
      geom_text(aes(x = snap_time, y = max(area) * 1.05, label = "Ball Snap", alpha = show_snap),
                color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -.3) +
      geom_vline(aes(xintercept = throw_time, alpha = show_throw),
                 color = "red", linetype = "dashed") + 
      geom_text(aes(x = throw_time, y = max(area) * 1.05, label = "Ball Thrown", alpha = show_throw),
                color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -.3) +
      geom_vline(aes(xintercept = catch_time, alpha = show_catch),
                 color = "red", linetype = "dashed") + 
      geom_text(aes(x = catch_time, y = max(area) * 1.05, label = "Ball Caught", alpha = show_catch),
                color = "red", size = 2.5, vjust = 0, hjust = -.3) +
      scale_alpha(range = c(0, 1), guide = "none") + 
      labs(title = paste(display_name[2], "| Voronoi Area")) +
      xlab("Frame Number") + ylab("Voronoi Area") +
      coord_cartesian(clip = 'off') + 
      theme_minimal() + transition_reveal(along = reveal_time)
  }
  animate(animated_plot, fps = 10, width = 500, height = 400, renderer = magick_renderer())
}

viz_game = 2022090800 
viz_play = 1030
games |> filter(gameId == viz_game)
area_line <- animate_voronoi(viz_game, viz_play, team1 = "LA", team1_color = "blue", 
                team2 = "BUF", team2_color = "red", plotPlay = FALSE)

play_animated <- animate_voronoi(viz_game, viz_play, team1 = "LA", team1_color = "blue", 
                team2 = "BUF", team2_color = "red", plotPlay = TRUE)

library(magick)

new_gif <- image_append(c(play_animated[1], area_line[1]))

for(i in 2:length(play_animated)) {
  combined <- image_append(c(play_animated[i], area_line[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
anim_save("testing.gif", new_gif)



#animate a high wp play for illustrative purposes
plays |> filter(homeTeamWinProbabilityAdded  >= .50 | visitorTeamWinProbilityAdded >= .5)
plays |> select(homeTeamWinProbabilityAdded) |> summarise(n = max(homeTeamWinProbabilityAdded)) |>
  select()

sample_game = 2022102302   
sample_play = 2655
tracking_game <- tracking |> filter(gameId == sample_game, playId == sample_play) |> as_tibble()

point_color_map = list(
  "CIN" = "orange",
  "ATL" = "red",
  "football" = "#654321"
)
description = plays |>filter(gameId == sample_game, playId == sample_play) |> select(playDescription) |> 
  as_tibble() |> first() |> as.character()
title <- "(1:54) (Shotgun) J.Burrow pass short middle to T.Boyd to CIN 30 for 9 yards (J.Hawkins)."
static_plot <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.6667) +
  geom_point(data = tracking_game, aes(x = x, y = y, color = club), size = 3) +
  scale_color_manual(values = point_color_map) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title =  title)

animated_plot <- static_plot +
  transition_states(
    states = frameId,
    transition_length = 1,
    state_length = 1,
    wrap = FALSE
  )
animated_plot
anim_save("highWP.gif", animated_plot)


#Toy example for difference of vector

u <- c(3, 4)
v <- c(1, 2)
diff <- u - v  

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
# Plot toy vector
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

ggsave("Zhou/Final/Assets/toyexample.png", close)

#Toy example for QB vision features 


qb <- c(2, 0)
wr <- c(-1, 4)

# QB vision: 30 degrees to the right
qb_angle <- 45 * pi / 180  
vision_len <- 1
vision <- c(vision_len * cos(qb_angle), vision_len * sin(qb_angle))

v2 <- wr - qb
wr_angle <- atan2(v2[2], v2[1])

vision_diff <- (wr_angle - qb_angle) * 180 / pi
vision_diff <- (vision_diff + 360) %% 360 
if (vision_diff > 180) vision_diff <- 360 - vision_diff 

arc_seq <- seq(qb_angle, wr_angle, length.out = 100)
arc_df <- data.frame(
  x = qb[1] + 0.5 * cos(arc_seq),
  y = qb[2] + 0.5 * sin(arc_seq)
)

df_points <- data.frame(
  x = c(qb[1], wr[1]),
  y = c(qb[2], wr[2]),
  label = c("QB", "WR")
)
df_vision <- data.frame(
  x = qb[1], y = qb[2],
  xend = qb[1] + vision[1], yend = qb[2] + vision[2],
  labels = c("QB Vision")
)
df_pass <- data.frame(
  x = qb[1], y = qb[2],
  xend = wr[1], yend = wr[2]
)

Los_wr<- ggplot() +
  geom_point(data = df_points, aes(x, y), size = 2) +
  geom_text(data = df_points, aes(x, y, label = label), vjust = +2, hjust = 1) +
  geom_segment(data = df_vision, aes(x = x, y = y, xend = xend, yend = yend,  color = labels),
               arrow = arrow(length = unit(0.1, "in")), linewidth = .6) +
  geom_segment(data = df_pass, aes(x = x, y = y, xend = xend, yend = yend),
               linetype = "dashed", color = "black") +
  geom_path(data = arc_df, aes(x, y), color = "purple", size = 1) +
  annotate("text",
           x = qb[1] ,  
           y = qb[2] + 1, 
           label = "Vision
  Diff",
           color = "purple"
  ) +
  theme_minimal() +
  labs(title = "Difference Between QB LoS and Pass Angle", color = "Vectors:") +
  xlim(-2, 5) +
  ylim(-2, 5) +
  coord_fixed()+
  scale_color_manual(values = c("QB Vision" = "blue")) +
  theme(plot.title = element_markdown(hjust = 0.5), legend.position = "bottom")
