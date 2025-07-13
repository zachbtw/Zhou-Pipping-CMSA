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
  label = "is_targetted"
  k = 5
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
  
  mean(unlist(fold_results))
}

main <- function(){
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
  total = nrow(hyperparam_grid)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  for(i in 1:total) {
    test_params <- hyperparam_grid[i, ]  |> as.list()
    accuracy <- run_xgbRanker_cv(closest, features = features, 
                     label = "is_targetted", params = test_params, k = 5, nrounds = 100, top_n = 1)
    if(accuracy > best_accuracy) {
      best_accuracy = accuracy
      best_params = test_params
    }
    setTxtProgressBar(pb, i)
    
  }
  close(pb) 
  print(best_accuracy) #best accuracy - 1/3
  #$nrounds - 100
  #eta - 0.05
  #max_depth - 4
  #gamma - 1
  #colsample_bytree -1
  #min_child_weight - 5
  #subsample - 1

  
  }