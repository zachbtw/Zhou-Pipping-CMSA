tracking <- tracking |>
  mutate(
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )

tracking <- games |>
  select(gameId, week) |>
  left_join(tracking, by = "gameId")
  

sample_game <- 2022091100
sample_play <- 14

passing_plays <- 
  plays |>
  select(gameId, playId, passResult) |>
  left_join(tracking, by = c("gameId", "playId")) |>
  filter(passResult %in% c("C", "I", "IN"))

# dim(as.data.table(passing_plays))
# tracking_oneplay <- 
#   players |>
#   select(nflId, position) |>
#   left_join(tracking, by = "nflId") |>
#   filter(gameId == 2022091100, playId == 301, position == "TE")

view(tracking_oneplay)

tracking_oneplay <- tracking_oneplay |>
  mutate(side = ifelse(club == "NO", "offense", "defense")) |>
  mutate(dist_nearest_def = )




