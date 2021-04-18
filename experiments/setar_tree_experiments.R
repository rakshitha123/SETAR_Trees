BASE_DIR <- "SETAR_Trees"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


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


# Sum of squares function
SS <- function(p, train_data, current_lg) {
  splitted_nodes <- create_split(train_data, current_lg, p)
  
  left <- splitted_nodes$left_node
  right <-  splitted_nodes$right_node 
  
  if(nrow(left) > 0 & nrow(right) > 0){
    residuals_l <- left$y - fit_global_model(left)$predictions
    residuals_r <- right$y - fit_global_model(right)$predictions
    current_residuals <- c(residuals_l, residuals_r)
    cost <-  sum(current_residuals ^ 2)
  }else{
    cost <- Inf
  }
  
  cost
}


check_linearity <- function(parent_node, child_nodes, lag, significance){
  print("lin test")
  
  is_significant <- TRUE
  
  ss0 <- sum((parent_node$y - as.numeric(fit_global_model(parent_node)[["predictions"]])) ^2) 
    
  if(ss0 == 0){
    is_significant <- FALSE
  }else{
    train_residuals <- NULL
    for(ln in 1:length(child_nodes)){
      train_residuals <- c(train_residuals, (child_nodes[[ln]]$y - as.numeric(fit_global_model(child_nodes[[ln]])[["predictions"]]))) 
    }
    
    ss1 <- sum(train_residuals ^ 2)
    
    
    # Compute F-statistic. For details, see https://online.stat.psu.edu/stat501/lesson/6/6.2
    f_stat <- ((ss0 - ss1)/(lag+1))/(ss1/(nrow(parent_node) - 2*lag - 2))
    p_value <- pf(f_stat, lag+1, nrow(parent_node) - 2*lag - 2, lower.tail = FALSE)
    
    if(p_value > significance)
      is_significant <- FALSE
    
    print(paste0("P-value = ", p_value, " Significant ", is_significant))
  }
  
  is_significant
}


check_error_improvement <- function(parent_node, child_nodes, error_threshold){
  print("error improvement")
  
  is_improved <- TRUE
  
  ss0 <- sum((parent_node$y - as.numeric(fit_global_model(parent_node)[["predictions"]])) ^2) 
  
  if(ss0 == 0){
    is_improved <- FALSE
  }else{
    train_residuals <- NULL
    for(ln in 1:length(child_nodes)){
      train_residuals <- c(train_residuals, (child_nodes[[ln]]$y - as.numeric(fit_global_model(child_nodes[[ln]])[["predictions"]]))) 
    }
    
    ss1 <- sum(train_residuals ^ 2)
    
    improvement <- (ss0-ss1)/ss0
    
    if(improvement < error_threshold)
      is_improved <- FALSE
    
    print(paste0("Error improvement = ", improvement, " Enough improvement ", is_improved))
  }
  
  is_improved
}


do_setar_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 2, key = "series_name", index = "start_timestamp", integer_conversion = F, significance = 0.05, scale = FALSE, seq_significance = TRUE, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", fixed_lag = FALSE, external_lag = 0){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  result <- create_input_matrix(training_set, lag, scale)
  embedded_series <- result[[1]]
  final_lags <- result[[2]]
  series_means <- result[[3]]
  
  
  # Start timestamp
  start_time <- Sys.time()
  
  
  # Set list of defaults:
  start.con <- list(nTh = 15)
  
  
  tree <- list()
  th_lags <- list()
  thresholds <- list()
  level_errors <- NULL
  
  node_data <- list(embedded_series)
  
  split_info <- 1
    
  for(d in 1:depth){
    print(paste0("Depth: ", d))
    level_th_lags <- NULL
    level_thresholds <- NULL
    level_nodes <- list()
    level_significant_node_count <- 0
    level_split_info <- NULL
    
    for(n in 1:length(node_data)){
      print(n)
      
      best_cost <- Inf
      th <- NULL
      th_lag <- NULL
      
      
      if(nrow(node_data[[n]]) > (2*lag + 2) & split_info[n] == 1){ # When this condition is not satisfied, the F-statistic will be negative and model will become unidentifiable
        
        if(fixed_lag){
          if(external_lag > 0)
            lg <- external_lag
          else
            lg <- 1
          
          ths <- seq(min(node_data[[n]][,lg+1]), max(node_data[[n]][,lg+1]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the corresponding lag
          
          for(ids in 1:length(ths)){
            cost <- SS(ths[ids], node_data[[n]], lg)
            
            if(cost <= best_cost) { # find th which minimizes the squared errors
              best_cost <- cost;
              th <- ths[ids]
              th_lag <- lg
            }
          }
        }else{
          for(lg in 1:lag){
            print(paste0("Lag ", lg))
            
            # print("Start hyperparameter tuning with grid search")
            ths <- seq(min(node_data[[n]][,lg+1]), max(node_data[[n]][,lg+1]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the corresponding lag
            
            for(ids in 1:length(ths)){
              cost <- SS(ths[ids], node_data[[n]], lg)
              
              if(cost <= best_cost) { # find th which minimizes the squared errors
                best_cost <- cost;
                th <- ths[ids]
                th_lag <- lg
              }
            }
          }
        }
        
        if(best_cost != Inf){
          splited_nodes <- create_split(node_data[[n]], th_lag, th)
          
          if(stopping_criteria == "lin_test")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, lag, significance)
          else if(stopping_criteria == "error_imp")
            is_significant <- check_error_improvement(node_data[[n]], splited_nodes, error_threshold)
          else if(stopping_criteria == "both")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, lag, significance) & check_error_improvement(node_data[[n]], splited_nodes, error_threshold) 
            
          if(is_significant){
            level_th_lags <- c(level_th_lags, th_lag)
            level_thresholds <- c(level_thresholds, th)
            level_split_info <- c(level_split_info, rep(1, 2))
            level_significant_node_count <- level_significant_node_count + 1
            
            for(s in 1:length(splited_nodes)){
              len <- length(level_nodes)
              level_nodes[[len + 1]] <- splited_nodes[[s]]
            }
          }else{
            level_th_lags <- c(level_th_lags, 0)
            level_thresholds <- c(level_thresholds, 0)
            level_split_info <- c(level_split_info, 0)
            
            len <- length(level_nodes)
            level_nodes[[len + 1]] <- node_data[[n]]
          }
        }else{
          level_th_lags <- c(level_th_lags, 0)
          level_thresholds <- c(level_thresholds, 0)
          level_split_info <- c(level_split_info, 0)
          
          len <- length(level_nodes)
          level_nodes[[len + 1]] <- node_data[[n]]
        }
      }else{
        level_th_lags <- c(level_th_lags, 0)
        level_thresholds <- c(level_thresholds, 0)
        level_split_info <- c(level_split_info, 0)
        
        len <- length(level_nodes)
        level_nodes[[len + 1]] <- node_data[[n]]
      }
    }
    
    if(level_significant_node_count > 0){
      tree[[d]] <- level_nodes
      thresholds[[d]] <- level_thresholds
      th_lags[[d]] <- level_th_lags
      node_data <- tree[[d]]
      split_info <- level_split_info
      
      if(seq_significance)
        significance <- significance/significance_divider
    }else
      break
  }
  
  
  # Check whether the tree has any nodes. If not, train a single pooled regression model
  if(length(tree) > 0){
    leaf_nodes <- tree[[length(tree)]]
    leaf_trained_models <- list()
    num_of_leaf_instances <- NULL
      
    # Train a linear model per each leaf node
    for(ln in 1:length(leaf_nodes)){
      leaf_trained_models[[ln]] <- fit_global_model(leaf_nodes[[ln]])[["model"]] 
      num_of_leaf_instances <- c(num_of_leaf_instances, nrow(leaf_nodes[[ln]]))
    }
  }else{
    final_trained_model <- fit_global_model(embedded_series)[["model"]]
  }
  
  
  file_name <- paste0(dataset_name, "_lag_", lag, "_depth_", depth, "_setar_", stopping_criteria)
  
  if(fixed_lag)
    file_name <- paste0(file_name, "_fixed_lag")
    
  if(stopping_criteria != "lin_test")
    file_name <- paste0(file_name, "_error_threshold_", error_threshold)
  
  if(seq_significance)
    file_name <- paste0(file_name, "_seq_significance_", significance_divider)
  
  if(scale)
    file_name <- paste0(file_name, "_with_scaling")
  
  
  if(length(tree) > 0){
    write(paste("No: of nodes in leaf level:", length(leaf_nodes)), file = file.path(BASE_DIR, "results", "tree_info", paste0(file_name, ".txt")), append = T)
    write(paste("Tree depth:", length(tree)), file = file.path(BASE_DIR, "results", "tree_info", paste0(file_name, ".txt")), append = T)
    write.table(num_of_leaf_instances, file.path(BASE_DIR, "results", "tree_info", paste0(file_name, ".txt")), row.names = F, col.names = F, quote = F, append = T)
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
  
  if(scale)
    forecasts <- forecasts * as.vector(series_means)
  
  
  # Finish timestamp
  end_time <- Sys.time()
 
  if(integer_conversion)
    forecasts <- round(forecasts)
  
 
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", "setar", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  print(th_lags)
  print(thresholds)
  print(paste0("Executed for ", length(tree), " levels"))
  
  if(length(tree) > 1)
    print(paste0("Insignificant node count = ", length(tree[[length(tree) - 1]]) - level_significant_node_count))
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
  
  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
}


# Experiments

# Chaotic Logistic
do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1000, index = NULL, stopping_criteria = "both", error_threshold = 0.03)

# Mackey glass
do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 1000, index = NULL, stopping_criteria = "both", error_threshold = 0.03)

# Tourism Quarterly
do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 1000, stopping_criteria = "both", error_threshold = 0.03)

# Kaggle Daily
do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1000, integer_conversion = T, stopping_criteria = "both", error_threshold = 0.03)

# Rossmann
do_setar_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", depth = 1000, integer_conversion = T, stopping_criteria = "both", error_threshold = 0.03)


