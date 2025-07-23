#generate random search, cv, and result generation for xgb
library(tidyverse)
library(data.table)
library(dtplyr)
library(pbapply)
library(xgboost)
library(purrr)
library(progressr)
create_cv_folds <- function(df, k = 5) {
  play_groups <- df |>
    distinct(gameId) |>
    mutate(fold = sample(rep(1:k, length.out = n())))
  
  df <- df |>
    left_join(play_groups, by = c("gameId"))
  
  return(df)
}


run_xgbRanker_cv <- function(df, features, label, params, k = 5, top_n = 1) {
  df_cv <- create_cv_folds(df, k = k)
  fold_results <- map(1:k, function(fold_num) {
    df_train <- df_cv |> filter(fold != fold_num)
    df_val <- df_cv |> filter(fold == fold_num)
    #set X and ys
    X_train <- as.matrix(df_train[, features])
    y_train <- df_train[[label]]
    group_train <- df_train |>
      group_by(gameId, playId) |>
      group_size() |>
      as.numeric()
    dtrain <- xgb.DMatrix(X_train, label = y_train)
    setinfo(dtrain, "group", group_train)
    nrounds_param = params$nrounds
    params <- list(
      objective = "rank:pairwise",
      eval_metric = "ndcg",
      eta = params$eta,
      max_depth = params$max_depth,
      min_child_weight = params$min_child_weight,
      colsample_bytree = params$colsample_bytree,
      gamma = params$gamma
    )
    
    bst <- xgb.train(
      params = params,
      nrounds = nrounds_param,
      data = dtrain,
      verbose = 0
    )
    
    X_val <- as.matrix(df_val[, features])
    dval <- xgb.DMatrix(X_val)
    group_val <- df_val |>
      group_by(gameId, playId) |>
      group_size() |>
      as.numeric()
    setinfo(dval, "group", group_val)
    
    preds <- predict(bst, dval)
    df_val$pred_score <- preds
    #get results and return top-1 
    df_val_ranked <- df_val |>
      group_by(gameId, playId) |>
      arrange(desc(pred_score), .by_group = TRUE) |>
      mutate(rank = row_number()) |>
      summarise(hit = any(is_targetted == 1 & rank <= top_n), .groups = "drop")
    
    top_n_acc <- mean(df_val_ranked$hit)
    return(top_n_acc)
  })
  #get CI
  t_crit <- qt(0.975, df = k - 1)
  avg <- mean(unlist(fold_results))
  std <- sd(unlist(fold_results))
  stderr <- std / k
  moe <-  t_crit * stderr
  c(avg,moe)
}

randomSearch <- function(){
  features <- read.csv("Zhou/Features-Results/features.csv")
  z_features <- read.csv("Zhou/Features-Results/zachfeatures.csv") |> group_by(gameId, playId) |>
    mutate(throw_frame = frameId == max(frameId),
           qb_vision_distlag5 = lag(qb_vision_distlag1, 4),
           vision_difflag5 = lag(vision_difflag1, 4)) |> ungroup() |>
    filter(throw_frame == TRUE) |>
    rename(nflId = runnerId)
  features <- features |> merge(z_features, by = c("gameId", "playId", "nflId"))
  num_closest = 3
  #build vector of feature names
  motion_feats <- crossing(
    prefix = c("closestOpponent_XSpeed_", "closestOpponent_YSpeed_","closestOpponent_SDiff_",
               "closestOpponentDistance_", "closestOpponentX_", "closestOpponentY_", "closestOpponentO_"),
    i = 1:num_closest
  ) |> 
    mutate(name = paste0(prefix, i)) |> 
    pull(name)
  position_feats <- paste0("position_", c("RB", "TE", "WR"))
  feature_names <- c(motion_feats, position_feats, "qb_dist", "qb_deg", "scorediff",
                     "absoluteYardlineNumber", "time_left", "quarter","yardsToGo.x", "down",
                     "givesFirst", "lastPlay", "n_defenders_ahead", "qb_vision_distlag5", "vision_difflag5")
  hyperparam_grid <- expand.grid(
    nrounds = seq(from = 100, to = 300, by = 100),
    eta = c(0.025, 0.05, 0.1, 0.3),
    max_depth = 3:7,
    gamma = c(0.5, 1, 1.5, 2, 5),
    colsample_bytree = c(0.6, 0.8, 1.0),
    min_child_weight = c(1, 5, 10),
    subsample = c(0.6, 0.8, 1.0)
  ) |> as.data.frame()
  best_params = NA
  best_accuracy = 0
  best_sd = NA
  total = 100
  handlers(global = TRUE)
  handlers("cli")
  with_progress({
    #random search
    p <- progressor(steps = total)
      for(i in 1:total) {
        test_params <- hyperparam_grid[sample(1:nrow(hyperparam_grid), 1), ]  |>
          as.list()
        #test_params <- params
        model_vals <- run_xgbRanker_cv(features, features = feature_names, 
                                     label = "is_targetted", params = test_params, k = 5, top_n = 1)
        accuracy <- model_vals[1]
        sd <- model_vals[2]
        if(accuracy > best_accuracy) {
          best_accuracy = accuracy
          best_params = test_params
          best_sd = sd
        }
        p(message = sprintf("Trial %d/100", i))
  }
  })
  print(best_accuracy)
}
test_params <- list(
  nrounds = 300,#300
  eta = .1,#.1
  max_depth = 5,#5
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 0.75,
  subsample = 1
)
getResults <- function(params) {
  features <- read.csv("Zhou/Features-Results/features.csv")
  z_features <- read.csv("Zhou/Features-Results/zachfeatures.csv") |> group_by(gameId, playId) |>
    mutate(throw_frame = frameId == max(frameId),
           qb_vision_distlag5 = lag(qb_vision_distlag1, 4),
           vision_difflag5 = lag(vision_difflag1, 4)) |> ungroup() |>
    filter(throw_frame == TRUE) |>
    rename(nflId = runnerId)
  features <- features |> merge(z_features, by = c("gameId", "playId", "nflId"))
  num_closest = 3
  #"closestOpponent_XSpeed_", "closestOpponent_YSpeed_", "closestOpponent_SDiff_"
  motion_feats <- crossing(
    prefix = c("closestOpponent_XSpeed_", "closestOpponent_YSpeed_","closestOpponent_SDiff_",
               "closestOpponentDistance_", "closestOpponentX_", "closestOpponentY_", "closestOpponentO_"),
    i = 1:num_closest
  ) |> 
    mutate(name = paste0(prefix, i)) |> 
    pull(name)
  position_feats <- paste0("position_", c("RB", "TE", "WR"))
  feature_names <- c(motion_feats, position_feats, "qb_dist", "qb_deg", "scorediff",
                                       "absoluteYardlineNumber", "time_left", "quarter","yardsToGo.x", "down",
                                       "givesFirst", "lastPlay", "n_defenders_ahead", "qb_vision_distlag5", "vision_difflag5")

  # get Results
  label = "is_targetted"
  X_train <- as.matrix(features[, feature_names])
  y_train <- features[[label]]
  group_train <- features |>
    group_by(gameId, playId) |>
    group_size() |>
    as.numeric()
  dtrain <- xgb.DMatrix(X_train, label = y_train)
  setinfo(dtrain, "group", group_train)
  nrounds_param = test_params$nrounds
  params <- list(
    objective = "rank:pairwise",
    eval_metric = "ndcg",
    eta = test_params$eta,
    max_depth = test_params$max_depth,
    min_child_weight = test_params$min_child_weight,
    colsample_bytree = test_params$colsample_bytree,
    gamma = test_params$gamma
  )
  
  bst <- xgb.train(
    params = params,
    nrounds = nrounds_param,
    data = dtrain,
    verbose = 0
  )
  
  plays <- lazy_dt(read.csv("nfl-big-data-bowl-2025/plays.csv"))
  player_play <- lazy_dt(read.csv("nfl-big-data-bowl-2025/player_play.csv"))
  games <- lazy_dt(read.csv("nfl-big-data-bowl-2025/games.csv"))
  players <- lazy_dt(read.csv("nfl-big-data-bowl-2025/players.csv"))

  positions <- player_play |> select(gameId, playId, nflId) |> merge(players |> select(nflId, position), on = "nflId") |>
    filter(position == "QB") |> rename(QB_nflId = nflId) |> select(playId, gameId, QB_nflId)

  df_val <- features |> merge(positions, on = c("playId", "gameId"))
  X_val <- as.matrix(df_val[, feature_names])
  dval <- xgb.DMatrix(X_val)
  group_val <- df_val |>
    group_by(gameId, playId) |>
    group_size() |>
    as.numeric()
  setinfo(dval, "group", group_val)
  
  preds <- predict(bst, dval)
  df_val$prediction <- preds
  names <- players |> select(nflId, displayName) |> rename("QB_nflId" = "nflId")
  #with results groupby QB
  qb_stats <- df_val |> group_by(playId, gameId) |>
    filter(prediction == max(prediction)) |> ungroup() |>
    group_by(QB_nflId) |> summarise(agreement = mean(is_targetted), throws = n()) 
  
  
  passResults <- plays |> select(playId, gameId, passResult, yardsGained) |> filter(
    passResult != ""
  ) |> mutate(completion = ifelse(passResult == "C", 1, 0))
  #get QB stats
  yard_throw_stats <- df_val |> merge(passResults, on = c("playId", "gameId")) |> group_by(playId, gameId) |>
    mutate(is_first_choice = prediction == max(prediction, na.rm = TRUE)) |>
    ungroup() |>
    mutate(threw_first_choice = ifelse(is_targetted == 1 & is_first_choice, 1, 0)) |>
    select(playId, gameId, QB_nflId, threw_first_choice, completion, yardsGained) |> 
    group_by(QB_nflId) |>
    ungroup() |>
    group_by(QB_nflId, threw_first_choice) |> summarise(completionRate = mean(completion), 
                                                        mean_yardsGained = mean(yardsGained))  |>
    pivot_wider(
      names_from = threw_first_choice,
      values_from = c(completionRate, mean_yardsGained),
      names_glue = "{.value}_if_first_choice_{threw_first_choice}"
    )|> mutate(
      first_minus_actual_completion = completionRate_if_first_choice_1 - completionRate_if_first_choice_0,
      first_minus_actual_yards = mean_yardsGained_if_first_choice_1 - mean_yardsGained_if_first_choice_0
    ) |> merge(names, on = c("QB_nflId")) |>  select(displayName, first_minus_actual_completion, first_minus_actual_yards, QB_nflId) |> 
    merge(qb_stats, on = c("QB_nflId")) |> filter(throws > 50) |> filter(QB_nflId != "45244")
  #sorry Taysom..
    write.csv(yard_throw_stats, "Zhou/QB_Results.csv")
    write.csv(df_val |> merge(passResults, on = c("playId", "gameId")), "Zhou/XGB_Results.csv")
  }
getResults(test_params)

get_vip <- function(bst){

new_cols = c("QB Movement Angle", "Difference between QB Angle and LoS 5 Frames Prior",
             "Distance from QB", "Difference in Orientation with Closest Defender", 
             "Distance from Closest Defender", "# of Defenders in 5-yard Radius Facing Receiver Movement",
             "Difference in Orientation with 2nd Closest Defender", "Difference in Orientation with 3rd Closest Defender",
             "Speed Vector Difference with 2nd Closest Defender", "Distance from 3rd Closest Defender")
title = "**The QB Movement Vector is Key to Deciding Throwing Target**"
vip_plot <- bst |> 
  vi() |> 
  head(10) |> 
  mutate(Variable = new_cols) |> 
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance))) +
  geom_col() +
  labs(x = "Importance", y = "Feature") +
  theme_minimal()  +
  labs(title = title) + theme(plot.title = element_markdown(hjust = 0, size = 13))
#0.60137121 0.00491559
ggsave("Zhou/Final/Assets/vip_plot.png", vip_plot)  
}
