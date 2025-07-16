library(tidyverse)
library(data.table)
library(dtplyr)
library(pbapply)
library(xgboost)
library(purrr)
create_cv_folds <- function(df, k = 5) {
  play_groups <- df |>
    distinct(gameId, playId) |>
    mutate(fold = sample(rep(1:k, length.out = n())))
  
  df <- df |>
    left_join(play_groups, by = c("gameId", "playId"))
  
  return(df)
}


run_xgbRanker_cv <- function(df, features, label, params, k = 5, nrounds = 100, top_n = 1) {
  df_cv <- create_cv_folds(df, k = k)
  fold_num = 1
  
  fold_results <- map(1:k, function(fold_num) {
    df_train <- df_cv |> filter(fold != fold_num)
    df_val <- df_cv |> filter(fold == fold_num)
    
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
    
    df_val_ranked <- df_val |>
      group_by(gameId, playId) |>
      arrange(desc(pred_score), .by_group = TRUE) |>
      mutate(rank = row_number()) |>
      summarise(hit = any(is_targetted == 1 & rank <= top_n), .groups = "drop")
    
    top_n_acc <- mean(df_val_ranked$hit)
    return(top_n_acc)
  })
  
  c(mean(unlist(fold_results)), sd(unlist(fold_results)))
}

main <- function(){
  features <- read.csv("Zhou/Features.csv")
  num_closest = 5
  #"closestOpponent_XSpeed_", "closestOpponent_YSpeed_", "closestOpponent_SDiff_"
  motion_feats <- crossing(
    prefix = c("closestOpponent_XSpeed_", "closestOpponent_YSpeed_","closestOpponent_SDiff_",
               "closestOpponentDistance_", "closestOpponentX_", "closestOpponentY_", "closestOpponentO_"),
    i = 1:num_closest
  ) |> 
    mutate(name = paste0(prefix, i)) |> 
    pull(name)
  
  feature_names <- c(motion_feats, "qb_dist", "qb_deg", "preSnapWinProb", "scorediff",
                     "absoluteYardlineNumber", "time_left", "quarter", "isZone", "yardsToGo", "down",
                     "givesFirst", "lastPlay")
  hyperparam_grid <- expand.grid(
    nrounds = seq(from = 100, to = 300, by = 100),
    eta = c(0.025, 0.05, 0.1, 0.3),
    max_depth = c(4, 5, 6),
    gamma = c(0, 1, 2),
    colsample_bytree = c(0.5, 0.75, 1.0),
    min_child_weight = c(1, 3, 5),
    subsample = 1
  ) |> as.data.frame()
  best_params = NA
  best_accuracy = 0
  best_sd = NA
  total = nrow(hyperparam_grid)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  for(i in 129:total) {
    test_params <- hyperparam_grid[i, ]  |> as.list()
    model_vals <- run_xgbRanker_cv(features, features = feature_names, 
                                 label = "is_targetted", params = test_params, k = 5, nrounds = 100, top_n = 1)
    accuracy <- model_vals[1]
    sd <- model_vals[2]
    if(accuracy > best_accuracy) {
      best_accuracy = accuracy
      best_params = test_params
      best_sd = sd
    }
    setTxtProgressBar(pb, i)
    
  }
  close(pb) 
  print(best_accuracy)
  #without speed: 0.52482480 0.01185587
  #with 0.5302462 0.0110015
  
}
k = 5
stderr <- 0.0110015 / k
mean_score = 0.5302462
# 95% CI using t-distribution
t_crit <- qt(0.975, df = k - 1)
ci_lower <- mean_score - t_crit * stderr
ci_upper <- mean_score + t_crit * stderr


print(best_params)
params <- list(
  nrounds = 300,
  eta = .1,
  max_depth = 5,
  gamma = 0,
  colsample_bytree = 0.75,
  min_child_weight = 1,
  subsample = 1
)
#attr(,"out.attrs")$dim
#nrounds              eta        max_depth            gamma colsample_bytree min_child_weight 
#300                  .1             6                0                0.75                1
#subsample 
#1 