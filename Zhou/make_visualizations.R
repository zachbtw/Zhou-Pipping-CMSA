library(gt)
library(gtExtras)
library(dplyr)
library(dtplyr)
library(rlang)
library(sportyR)
library(ggnewscale)
library(flexclust)
library(stringr)
library(gganimate)
library(purrr)
library(ggtext)
library(patchwork)
library(magick)
library(ggforce)

tracking <- lazy_dt(arrow::read_parquet("nfl-big-data-bowl-2025/tracking.parquet"))
qb_results <- read.csv("Zhou/Features-Results/QB_Results.csv")
players <- lazy_dt(read.csv("nfl-big-data-bowl-2025/players.csv"))
player_play <- lazy_dt(read.csv("nfl-big-data-bowl-2025/player_play.csv"))
predictions <- read.csv("Zhou/Features-Results/XGB_Results.csv")

teams <- player_play |> select(nflId, teamAbbr) |> distinct() |> as_tibble()
qb_results <- qb_results |> left_join(teams, by = c("QB_nflId"= "nflId")) 
qb_results <- qb_results |> mutate(agreement = 100 * round(agreement, 4),
                                   first_minus_actual_yards = round(first_minus_actual_yards, 4)) |> 
  select(displayName, first_minus_actual_completion, first_minus_actual_yards,agreement,
         throws, teamAbbr)
#players who gain more yards throwing to model choice
tooReckless_yrd <- qb_results |>  
  arrange(-first_minus_actual_yards) |> head(10) 

#players who gain less yards throwing to model choice
tooConservative_yrd <- qb_results |>  
  arrange(first_minus_actual_yards) |> head(10) 

#players who agree most with first choice
predictable <- qb_results |>  arrange(-agreement) |> head(10) 

#players who disagree most with first choice
unpredictable <- qb_results |>  arrange(agreement) |> head(10) 


#Most predictable QBs:
buildTable <- function(df, column, title, subtitle, custom_pal = "red") {
  vals <- df[[column]]
  df |>
    select(
      displayName, teamAbbr, throws, agreement
    ) |>
    gt() |>
    data_color(
      columns = !!sym(column),
      colors = scales::col_numeric(
        palette = custom_pal,  # gradient from white to your color
        domain = range(vals, na.rm = TRUE)
      )
    ) |>
    fmt_number(
      columns = !!sym(column),
      decimals = 3
    ) |>
    tab_header(
      title = md(title),
      subtitle = subtitle
    ) |> cols_label(
      displayName = "Quarterback",
      agreement = "% Throws to Predicted Receiver",
      teamAbbr = "Team",
      throws = "Total Throws"
    )
}


ptable <- predictable |> 
  buildTable("agreement", "**Top 10 Most Predictable QBs**", 
             "2022 NFL Season, Weeks 1-9 - Minimum 50 Throws", custom_pal = c("lightblue", "blue"))

  gtsave(filename = "Zhou/Final/Assets/MostPredictable.png", vwidth = 1040, vheight = 898)

utable <- unpredictable |> 
  buildTable("agreement", "**Top 10 Least Predictable QBs**", 
             "2022 NFL Season, Weeks 1-9 - Minimum 50 Throws", custom_pal =  c("red", "pink"))

  gtsave(filename = "Zhou/Final/Assets/MostUnPredictable.png", vwidth = 1040, vheight = 898)

#Elbow Plot - 4 clusters:

gapminder_kmpp <- function(k) {
  
  kmeans_results <- qb_results |> 
    select(first_minus_actual_completion, first_minus_actual_yards) |>
    kcca(k = k, control = list(initcent = "kmeanspp"))
  
  kmeans_out <- tibble(
    clusters = k,
    total_wss = sum(kmeans_results@clusinfo$size * 
                      kmeans_results@clusinfo$av_dist)
  )
  return(kmeans_out)
}


init_kmeanspp <- qb_results |> 
  select(first_minus_actual_completion, first_minus_actual_yards) |> 
  kcca(k = 4, control = list(initcent = "kmeanspp"))
#Cluster QBs

qb_cluster <- qb_results |>
  mutate(
    QB_clusters = as.factor(case_when(
      first_minus_actual_yards < -1.5 ~ 1,
      first_minus_actual_yards > 0.5 ~ 3,
      .default = 2
    )),
    short_name = str_extract(displayName, "^\\w") |> str_c(". ", word(displayName, -1))
  ) |>
  ggplot(aes(y = first_minus_actual_completion, x = first_minus_actual_yards,
             color = QB_clusters)) +
  geom_point(size = 1) +
  geom_text(aes(label = short_name), vjust = 1.5, size = 2, show.legend = FALSE, fontface = "bold") +
  xlab("Yards Per Attempt(YPA) Difference: Throws to Predicted vs. Other Targets") + 
  ylab("Comp% Difference: Throws to Predicted vs. Other Targets") + 
  geom_mark_ellipse(aes(filter = QB_clusters == 1, label = "Having Success with Atypical Passes")) +
  geom_mark_ellipse(aes(filter = QB_clusters == 3, label = "Potential Optimization by Throwing More Conventional Passes")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_markdown(hjust = 0.5, size = 16, family = "Roboto"),
        plot.subtitle = element_markdown(hjust = 0.5, size = 10)) + labs(title = "**Opportunities for Optimization in QB Target Decision-Making**",
                                            color = "Cluster",
                                         subtitle = "2022 NFL Season, Weeks 1-9 | Minimum 50 Throws")
ggsave("Zhou/Final/Assets/QBClustering.png", qb_cluster)
#Example Play

play_of_interest <- tracking |> filter(gameId == 2022103001, playId == 3953, 
                                       event == "pass_forward") |> as_tibble()


point_color_map <- c(
  "CAR" = "lightblue",
  "ATL" = "red",
  "football" = "#654321"
)

preds <- predictions |> select(gameId, playId, nflId, prediction) |> 
  filter(gameId == 2022103001, playId == 3953)
# Merge predictions
play_with_preds <- play_of_interest |> 
  full_join(preds, by = "nflId") |> mutate(
    short_name = str_extract(displayName, "^\\w") |> str_c(". ", word(displayName, -1)
  ))

pjmoore <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.6667) +
  geom_text(data = filter(play_with_preds, !is.na(prediction)), 
            aes(x = x, y = y, label = short_name), 
            color = "white", size = 1.5, vjust = 3) +
  geom_point(data = filter(play_with_preds, !is.na(prediction)), 
             aes(x = x, y = y, fill = prediction), shape = 23, stroke = 1,color = "black",
             size = 2) +
  scale_fill_gradient(high = "darkblue", low = "lightblue") +
  new_scale("color") + 
  geom_point(data = filter(play_with_preds, is.na(prediction)), 
             aes(x = x, y = y, color = club), size = 2) +
  
  scale_color_manual(values = point_color_map, na.value = "grey") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "P.J. Walker Game Tying Touchdown Throw | Predicting Targets",
       caption = "Darker Color Implies More Likely Target, Diamond Implies Possible Receivers")
ggsave("Zhou/Final/Assets/PJMoorePredictions.png", pjmoore)

#build table
positions <- players |> select(nflId, position)
model_outputs <- play_with_preds |> filter(!is.na(prediction)) |> merge(positions, on = c("playId", "gameId"))
  model_outputs |> 
    select(
      displayName, position, prediction
    ) |> arrange(-prediction) |> 
    gt() |>
    data_color(
      columns = prediction,
      fn = scales::col_numeric(
        palette = c("pink", "red"),  # gradient from white to your color
        domain = range(model_outputs$prediction, na.rm = TRUE)
      )
    ) |>
    fmt_number(
      columns = prediction,
      decimals = 3
    ) |>
    tab_header(
      title = md("**Relative Rankings**"),
    ) |> cols_label(
      displayName = "Receiver",
      position = "Position",
      prediction = "Relative Scores"
    ) |> gtsave("Zhou/Final/Assets/moore_throw_rankings.png")

  #animate example
  viz_game = 2022091111   
  viz_play = 1790
  receiver = 41282
  point_color_map <- c(
    "recipient" = "red",
    football = "#654321",
    "LV" = "black",
    "LAC" = "powderblue"
  )
source("Zhou/generate_features.R")
#receiver = 53074
poi <- tracking |> filter(gameId == viz_game, playId == viz_play)  |> clean_tracking() 



opponent_dists <- poi |>
  inner_join(poi, by = "frameId", suffix = c("", "_opp")) |>
  filter(club != club_opp, club_opp != "football", nflId != nflId_opp) |>
  mutate(distance = sqrt((x - x_opp)^2 + (y - y_opp)^2)) |>
  group_by(nflId, frameId) |>
  slice_min(order_by = distance, with_ties = FALSE) |>
  ungroup() |>
  select(nflId, frameId, min_opponent_distance = distance, x_opp, y_opp)


poi <- poi |>
  left_join(opponent_dists, by = c("nflId", "frameId")) |>
  mutate(club = ifelse(nflId == receiver, "recipient", club),
         club = ifelse(is.na(club), "football", club)) |> as_tibble()
poi <- poi |> mutate(
  x_opp = ifelse(nflId == receiver, x_opp, x),
  y_opp = ifelse(nflId == receiver, y_opp, y)
)



# Create 10 copies of the ball_snap rows and bind them with the original
poi <- bind_rows(
  poi,
  map_dfr(1:50, ~ poi |> filter(event == "ball_snap"))
) |> arrange(frameId) |>
  mutate(new_frameId = cumsum(is.na(nflId)))

poi <- bind_rows(
  poi,
  map_dfr(1:50, ~ poi |> filter(event == "pass_forward"))
) |> arrange(frameId) |>
  mutate(new_frameId = cumsum(is.na(nflId)))

poi <- bind_rows(
  poi,
  map_dfr(1:50, ~ poi |> filter(event == "pass_arrived"))
) |> arrange(frameId) |>
  mutate(new_frameId = cumsum(is.na(nflId)))

receiver_dists <- poi |> filter(nflId == receiver) 

throw_time <- receiver_dists |> filter(event == "pass_forward") |>
  pull(frameId) |>
  first() |>
  as.integer()

catch_time <- receiver_dists|> filter(event == "pass_arrived") |>
  pull(frameId) |>
  first() |>
  as.integer()

snap_time <- receiver_dists|> filter(event == "ball_snap") |>
  pull(frameId) |>
  first() |>
  as.integer()

receiver_dists <- receiver_dists |>
  mutate(show_throw = ifelse(frameId >= throw_time, 1, 0),
         show_catch = ifelse(frameId >= catch_time, 1, 0),
         show_snap = ifelse(frameId >= snap_time, 1, 0))


title = "<b>L.V. Raiders</b> at <span style = 'color: powderblue;'><b>
L.A. Chargers </span></b>, 2022 NFL Week 1"
subtitle = "(14:19) (Shotgun) D.Carr pass deep left to<span style = 'color: red;'>
D. Adams </span>to LAC 23 for 41 yards"
lineplot_title = "<span style = 'color: red;'><b>Davante Adams </b></span> distance from closest defender"
field_plot <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.6667) +
  geom_point(data = poi, aes(x = x, y = y, color = club), size = 3) +
  geom_segment(data = poi, aes(x = x, y = y, xend = x_opp, yend = y_opp, color = "black")) + 
  scale_color_manual(values = point_color_map) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = title, subtitle = subtitle) + 
  theme(plot.title = element_markdown(hjust = 0.5, size = 16, family = "helvetica"),
        plot.subtitle = element_markdown(hjust = 0.5, size = 10, family = "helvetica")) + 
  transition_states(new_frameId) 
field_plot <- animate(field_plot, fps = 5, width = 500, height = 400, renderer = magick_renderer())
line_plot <- ggplot(receiver_dists, 
                    aes(x = frameId, y = min_opponent_distance)) +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(aes(xintercept = snap_time, alpha = show_snap),
             color = "red", linetype = "dashed") + 
  geom_text(aes(x = snap_time, y = max(min_opponent_distance) * .9, label = "Ball Snap", alpha = show_snap),
            color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -.3) +
  geom_vline(aes(xintercept = throw_time, alpha = show_throw),
             color = "red", linetype = "dashed") + 
  geom_text(aes(x = throw_time, y = max(min_opponent_distance) * .95, label = "Ball Thrown", alpha = show_throw),
            color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -.3) +
  geom_vline(aes(xintercept = catch_time, alpha = show_catch),
             color = "red", linetype = "dashed") + 
  geom_text(aes(x = catch_time, y = max(min_opponent_distance) * 1.0, label = "Ball Caught", alpha = show_catch),
            color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -.3) +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title = lineplot_title, y = "Distance of Closest Defender", x = "Frame Number") +  
  theme(plot.title = element_markdown(hjust = 0.5, size = 13, family = "helvetica"))+# adjust as needed
  transition_reveal(new_frameId)
line_plot <- animate(line_plot, fps = 5, width = 500, height = 400, renderer = magick_renderer())
new_gif <- image_append(c(field_plot[1], line_plot[1]))

for(i in 2:length(field_plot)) {
  combined <- image_append(c(field_plot[i], line_plot[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, path = "Zhou/Final/Assets/dadamsthrow.gif")
plotLine <- function(df) {
line_forward <- ggplot(df, 
       aes(x = frameId, y = min_opponent_distance)) +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(aes(xintercept = snap_time, alpha = show_snap),
             color = "red", linetype = "dashed") + 
  geom_text(aes(x = snap_time, y = max(min_opponent_distance) * .8, label = "Ball Snap", alpha = show_snap),
            color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -.3) +
  geom_vline(aes(xintercept = throw_time, alpha = show_throw),
             color = "red", linetype = "dashed") + 
  geom_text(aes(x = throw_time, y = max(min_opponent_distance) * .9, label = "Ball Thrown", alpha = show_throw),
            color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -.3) +
  geom_vline(aes(xintercept = catch_time, alpha = show_catch),
             color = "red", linetype = "dashed") + 
  geom_text(aes(x = catch_time, y = max(min_opponent_distance) * 1, label = "Ball Caught", alpha = show_catch),
            color = "red", size = 2.5, fontface = "bold", vjust = 0, hjust = -.3) +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title = lineplot_title, y = "Distance of Closest Defender", x = "Frame Number") +  
  theme(plot.title = element_markdown(hjust = 0.5, size = 13, family = "helvetica"))


#lineplot_title
}

plotPlay <- function(df, df_2) {
  p1 <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.6667) +
    geom_point(data = df, aes(x = x, y = y, color = club), size = 2) +
    geom_segment(data = df, aes(x = x, y = y, xend = x_opp, yend = y_opp, color = "black")) + 
    scale_color_manual(values = point_color_map) +
    theme_minimal() +
    labs(caption = "Play at Throw") + 
    theme(plot.title = element_markdown(hjust = 0.5, size = 16, family = "helvetica"),
          plot.subtitle = element_markdown(hjust = 0.5, size = 10, family = "helvetica"),
          legend.position = "none")
  
  p2 <- geom_football(league = "NFL", x_trans = 60, y_trans = 26.6667) +
    geom_point(data = df_2, aes(x = x, y = y, color = club), size = 2) +
    geom_segment(data = df_2, aes(x = x, y = y, xend = x_opp, yend = y_opp, color = "black")) + 
    scale_color_manual(values = point_color_map) + ylab("") + 
    theme_minimal() + theme(legend.position = "none") + labs(caption = "Play at Reception")
  (p1 | p2) + 
    plot_annotation(
      title = title,
      subtitle = subtitle, 
      theme = theme(
        plot.title = element_markdown(hjust = 0.5, size = 16, family = "Helvetica"),
        plot.subtitle = element_markdown(hjust = 0.5, size = 10, family = "helvetica")
      )
    )
}

play_forward <- plotPlay(poi |> filter(frameId == 169), poi |> filter(frameId == 185))

play_forward
ggsave("dadams_poster.png", play_forward)

line_arrive<- plotLine(receiver_dists)

ggsave("dadams_catch.png", arrive_plot)

play_forward + line_arrive
