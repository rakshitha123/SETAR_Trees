BASE_DIR <- "C:/Projects/Trees/"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


# input_file_name <- "chaotic_logistic_dataset.ts"
# lag <- 10
# key <- "series_name"
# index <- NULL
# forecast_horizon <- 8
# dataset_name = "chaotic_logistic"
# integer_conversion = F
# depth = 4
# significance = 0.001


create_split <- function(data, conditional_lag, threshold){
  left_node <- data[data[[paste0("Lag", conditional_lag)]] < threshold,]
  right_node <- data[data[[paste0("Lag", conditional_lag)]] >= threshold,]
  list("left_node" = left_node, "right_node" = right_node)
}


tree_traverse <- function(instance, split, threshold){
  direction <- "left"
  
  if(instance[[paste0("Lag", split)]] >= threshold)
    direction <- "right"
  
  direction
}


get_leaf_index <- function(instance, splits, thresholds){
  current_split <- 1
  divide_factor <- 2
  
  for(sp in 1:length(splits)){
    
    if(length(which(splits[[sp]] == 0)) > 0){
      zeros <- which(splits[[sp]] == 0)
      
      change_count <- 0
      
      for(d in 1:length(zeros)){
        if(zeros[d] < current_split){
          change_count <- change_count + 1
        }
      }
      
      next_possible_splits <- tail(1: (current_split*divide_factor), divide_factor)
      next_possible_splits <- next_possible_splits - change_count
      
      if(splits[[sp]][current_split] == 0){
        current_split <- next_possible_splits[1]
      }else{
        direction <- tree_traverse(instance, splits[[sp]][current_split], thresholds[[sp]][current_split])
        
        if(direction == "left")
          current_split <- next_possible_splits[1]
        else
          current_split <- next_possible_splits[2]
      }
    }else{
      direction <- tree_traverse(instance, splits[[sp]][current_split], thresholds[[sp]][current_split])
      next_possible_splits <- tail(1: (current_split*divide_factor), divide_factor)
      
      if(direction == "left")
        current_split <- next_possible_splits[1]
      else
        current_split <- next_possible_splits[2]
    }
  }
  current_split
}


do_setar_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 2, key = "series_name", index = "start_timestamp", integer_conversion = F, significance = 0.05){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  result <- create_input_matrix(training_set, lag)
  embedded_series <- result[[1]]
  final_lags <- result[[2]]
  # series_means <- result[[3]]
  
  
  # Start timestamp
  start_time <- Sys.time()
  
  
  initial_predictions <- fit_global_model(embedded_series)$predictions
  initial_error <- sum((embedded_series$y - initial_predictions)^2)
  
  # Set list of defaults:
  start.con <- list(nTh = 15)
  
  
  tree <- list()
  th_lags <- list()
  thresholds <- list()
  level_errors <- NULL
  
  node_data <- list(embedded_series)
    
  for(d in 1:depth){
    print(paste0("Depth: ", d))
    level_th_lags <- NULL
    level_thresholds <- NULL
    level_nodes <- list()
    
    for(n in 1:length(node_data)){
      print(n)
      
      best_cost <- Inf
      th <- NULL
      th_lag <- NULL
      is_splitted <- FALSE
      
      
      if(nrow(node_data[[n]]) > 1){
        for(lg in 1:lag){
          print(lg)
          ths <- seq(min(node_data[[n]][,lg+1]), max(node_data[[n]][,lg+1]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the corresponding lag
          
          for(ids in 1:length(ths)){
            splitted_nodes <- create_split(node_data[[n]], lg, ths[ids])
            left <- splitted_nodes$left_node
            right <-  splitted_nodes$right_node 
            
            if(nrow(left) > 0 & nrow(right) > 0){
              is_splitted <- TRUE
              residuals_l <- left$y - fit_global_model(left)$predictions
              residuals_r <- right$y - fit_global_model(right)$predictions
              current_residuals <- c(residuals_l, residuals_r)
              
              cost <-  sum(current_residuals ^ 2) # sum of squares of residuals
              
              if(cost <= best_cost) { # find gamma and th which minimize the squared errors
                best_cost <- cost;
                th <- ths[ids]
                th_lag <- lg
              }
            }
          }
        }
        
        # print(th)
        # print(th_lag)
        
        if(is_splitted){
          level_th_lags <- c(level_th_lags, th_lag)
          level_thresholds <- c(level_thresholds, th)
          
          splited_nodes <- create_split(node_data[[n]], th_lag, th)
          
          for(s in 1:length(splited_nodes)){
            len <- length(level_nodes)
            level_nodes[[len + 1]] <- splited_nodes[[s]]
          }
        }else{
          level_th_lags <- c(level_th_lags, 0)
          level_thresholds <- c(level_thresholds, 0)
          
          len <- length(level_nodes)
          level_nodes[[len + 1]] <- node_data[[n]]
        }
      }else{
        level_th_lags <- c(level_th_lags, 0)
        level_thresholds <- c(level_thresholds, 0)
        
        len <- length(level_nodes)
        level_nodes[[len + 1]] <- node_data[[n]]
      }
    }
    
    
    # Linearity test
    print("lin test")
    train_residuals <- NULL
    for(ln in 1:length(level_nodes)){
      train_residuals <- c(train_residuals, (level_nodes[[ln]]$y - as.numeric(fit_global_model(level_nodes[[ln]])[["predictions"]]))) 
    }
    
    ss1 <- sum(train_residuals ^ 2)
    
    
    # Compute F-statistic. For details, see https://online.stat.psu.edu/stat501/lesson/6/6.2
    if(d == 1)
      ss0 <- initial_error
    else
      ss0 <- level_errors[d-1]
    
    
    f_stat <- ((ss0 - ss1)/(lag+1))/(ss1/(nrow(embedded_series) - 2*lag - 2))
    p_value <- pf(f_stat, lag+1, nrow(embedded_series) - 2*lag - 2, lower.tail = FALSE)
    
    
    print(paste0("SS0 = ", ss0))
    print(paste0("SS1 = ", ss1))
    print(paste0("F-statistic = ", f_stat))
    print(paste0("P-value = ", p_value))
    
    if(p_value < significance){
      print(paste0("Linearity test is significant. Adding a new level to the tree..."))
      tree[[d]] <- level_nodes
      thresholds[[d]] <- level_thresholds
      th_lags[[d]] <- level_th_lags
      node_data <- tree[[d]]
      level_errors[d] <- ss1
    }else{
      print(paste0("Linearity test is not significant. Stop training..."))
      break
    }
  }
  
  
  # Check whether the tree has any nodes. If not, train a single pooled regression model
  if(length(tree) > 0){
    leaf_nodes <- tree[[length(tree)]]
    leaf_trained_models <- list()
    
    
    # Train a linear model per each leaf node
    for(ln in 1:length(leaf_nodes)){
      leaf_trained_models[[ln]] <- fit_global_model(leaf_nodes[[ln]])[["model"]] 
    }
  }else{
    final_trained_model <- fit_global_model(embedded_series)[["model"]]
  }
  
  
  # Forecasting
  forecasts <- NULL
  
  for(h in 1:forecast_horizon){
    horizon_predictions <- NULL
    
    if(length(tree) > 0){
      for(f in 1:nrow(final_lags)){
        leaf_index <- get_leaf_index(final_lags[f,], th_lags, thresholds)
        horizon_predictions <- c(horizon_predictions, predict.glm(object = leaf_trained_models[[leaf_index]], newdata = as.data.frame(final_lags[f,]))) 
      }
    }else{
      horizon_predictions <- predict.glm(object = final_trained_model, newdata = as.data.frame(final_lags))
    }
    
    
    forecasts <- cbind(forecasts, horizon_predictions)
    
    # Updating the test set for the next horizon
    if(h < forecast_horizon){
      final_lags <- final_lags[-lag]
      
      # Updating lags for the next horizon
      final_lags <- cbind(horizon_predictions, final_lags)
      colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
      
      final_lags <- as.data.frame(final_lags)
    }
  }
  
  
  # Finish timestamp
  end_time <- Sys.time()
  
 
  if(integer_conversion)
    forecasts <- round(forecasts)
  
  file_name <- paste0(dataset_name, "_lag_", lag, "_depth_", depth, "_setar")
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", "setar", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  print(th_lags)
  print(thresholds)
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
  
  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
}


# Experiments

# Chaotic Logistic
# do_setar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 1, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 4, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 5, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 6, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 7, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 8, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 9, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 10, index = NULL, significance = 0.001)


# Mackey glass
# do_setar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 1, index = NULL, significance = 0.001)
# do_setar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 4, index = NULL, significance = 0.001)
# do_setar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 6, index = NULL, significance = 0.001)
# do_setar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 10, index = NULL, significance = 0.001)


# NN5 Daily
# do_setar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 1, significance = 0.001)
# do_setar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 4, significance = 0.001)
# do_setar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 6, significance = 0.001)
# do_setar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 10, significance = 0.001)
do_setar_forecasting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 6, significance = 0.001)


# Kaggle Daily
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", depth = 1, integer_conversion = T, significance = 0.001)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", depth = 4, integer_conversion = T, significance = 0.001)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", depth = 6, integer_conversion = T, significance = 0.001)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 74, 59, "kaggle_daily", depth = 1, integer_conversion = T, significance = 0.001)


# Tourism Quarterly
# do_setar_forecasting("tourism_quarterly_dataset.ts", 10, 8, "tourism_quarterly", depth = 1, significance = 0.001)
# do_setar_forecasting("tourism_quarterly_dataset.ts", 10, 8, "tourism_quarterly", depth = 6, significance = 0.001)




