# BASE_DIR <- "C:/Projects/SETAR_Trees/"
BASE_DIR <- "/home/rakshitha/Trees/"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


# input_file_name <- "chaotic_logistic_dataset.tsf"
# lag <- 10
# key <- "series_name"
# index <- NULL
# forecast_horizon <- 8
# dataset_name = "chaotic_logistic"
# integer_conversion = F
# depth = 1000
# significance = 0.05
# tune_method = "grid_search"
# seq_significance = T

setar_tree_forecasting <- function(tree_data, lag, forecast_horizon, final_lags, feature_indexes, depth = 1000, significance = 0.5, seq_significance = TRUE, stopping_criteria = "both", error_threshold = 0.03, random_lag = FALSE, random_threshold = FALSE, num_leaves = 64, min_data_in_leaf = 100){
  tree <- list()
  th_lags <- list()
  thresholds <- list()
  level_errors <- NULL
  
  # Set list of defaults:
  start.con <- list(nTh = 15)
  
  node_data <- list(tree_data)
  
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
      
      
      if((nrow(node_data[[n]]) > (2*lag + 2)) & (split_info[n] == 1) & (nrow(node_data[[n]]) >= min_data_in_leaf)){ # When this condition is not satisfied, the F-statistic will be negative and model will become unidentifiable
        
        if(random_lag){
          set.seed(d+n)
          lg <- sample(feature_indexes, 1)
          th_lag <- lg
          
          print(paste0("Lag ", lg))
          
          ths <- seq(min(node_data[[n]][, paste0("Lag", lg)]), max(node_data[[n]][, paste0("Lag", lg)]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the corresponding lag
          
          if(random_threshold){
            set.seed(d+n)
            th <- sample(ths, 1)
            best_cost <- SS(th, node_data[[n]], lg)
          }else{
            for(ids in 1:length(ths)){
              cost <- SS(ths[ids], node_data[[n]], lg)
              
              if(cost <= best_cost) { # find th which minimizes the squared errors
                best_cost <- cost;
                th <- ths[ids]
              }
            }
          }
        }else{
          for(lg in feature_indexes){
            print(paste0("Lag ", lg))
            
            # print("Start hyperparameter tuning with grid search")
            ths <- seq(min(node_data[[n]][, paste0("Lag", lg)]), max(node_data[[n]][, paste0("Lag", lg)]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the corresponding lag
            
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
    
    if(level_significant_node_count > 0 & length(level_nodes) <= num_leaves){
      tree[[d]] <- level_nodes
      thresholds[[d]] <- level_thresholds
      th_lags[[d]] <- level_th_lags
      node_data <- tree[[d]]
      split_info <- level_split_info
      
      if(seq_significance)
        significance <- significance/2
    }else
      break
  }
  
  
  # Model training
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
    final_trained_model <- fit_global_model(tree_data)[["model"]]
  }
  
  # Forecasting
  forecasts <- NULL
  
  for(h in 1:forecast_horizon){
    horizon_predictions <- NULL
    
    if(length(tree) > 0){
      for(f in 1:nrow(final_lags)){
        leaf_index <- get_leaf_index(final_lags[f, feature_indexes], th_lags, thresholds)
        horizon_predictions <- c(horizon_predictions, predict.glm(object = leaf_trained_models[[leaf_index]], newdata = as.data.frame(final_lags[f, feature_indexes]))) 
      }
    }else{
      horizon_predictions <- predict.glm(object = final_trained_model, newdata = as.data.frame(final_lags[,feature_indexes]))
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
  
  print(th_lags)
  print(thresholds)
  print(paste0("Executed for ", length(tree), " levels"))
  
  forecasts
}


do_setar_forest_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 1000, key = "series_name", index = "start_timestamp", integer_conversion = F, significance = 0.05, seq_significance = TRUE, error_threshold = 0.03, stopping_criteria = "both", bagging_fraction = 0.5, bagging_freq = 1, feature_fraction = 1, random_lag = FALSE, random_threshold = FALSE, num_leaves = 64, min_data_in_leaf = 100){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  result <- create_input_matrix(training_set, lag)
  embedded_series <- result[[1]]
  full_final_lags <- result[[2]]
  
  all_tree_forecasts <- list()

  
  # Start timestamp
  start_time <- Sys.time()
  
  num_indexes <- round(nrow(embedded_series) * bagging_fraction)
  num_features <- round(lag * feature_fraction)
  
  
  # Get forecasts from multiple setar trees as required
  for(bag_f in 1:bagging_freq){
    set.seed(bag_f)
    tree_indexes <- sort(sample(1:nrow(embedded_series), num_indexes, replace = FALSE))
    
    set.seed(bag_f)
    feature_indexes <- sort(sample(1:lag, num_features, replace = FALSE))
      
    current_tree_data <- embedded_series[tree_indexes, c(1, (feature_indexes + 1))]
    
    all_tree_forecasts[[bag_f]] <- setar_tree_forecasting(current_tree_data, lag, forecast_horizon, full_final_lags, feature_indexes, depth, significance, seq_significance, stopping_criteria, error_threshold, random_lag, random_threshold, num_leaves, min_data_in_leaf)
  }
 
  final_forecasts <- all_tree_forecasts[[1]]
 
  for(bag_f in 2:bagging_freq)
    final_forecasts <- final_forecasts + all_tree_forecasts[[bag_f]]
  
  final_forecasts <- final_forecasts/bagging_freq
 
  
  # Finish timestamp
  end_time <- Sys.time()
  
  if(integer_conversion)
    final_forecasts <- round(final_forecasts)
  
  file_name <- paste0(dataset_name, "_lag_", lag, "_depth_", depth, "_min_leaf_", min_data_in_leaf, "_leaves_", num_leaves, "_setar_forest_", bagging_freq, "_bagging_frac_", bagging_fraction, "_feature_frac_", feature_fraction, "_", stopping_criteria)

  if(stopping_criteria != "lin_test")
    file_name <- paste0(file_name, "_error_threshold_", error_threshold)
  
  if(seq_significance)
    file_name <- paste0(file_name, "_seq_significance")
  
  if(random_lag)
    file_name <- paste0(file_name, "_random_lag")
  
  if(random_threshold)
    file_name <- paste0(file_name, "_random_threshold")
  
  write.table(final_forecasts, file.path(BASE_DIR, "results", "forecasts", "setar_forest", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", "setar_forest", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
  
  calculate_errors(final_forecasts, test_set, training_set, seasonality, file_name)
}


do_setar_forest_block_bootstrap_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 1000, key = "series_name", index = "start_timestamp", integer_conversion = F, significance = 0.05, seq_significance = TRUE, error_threshold = 0.03, stopping_criteria = "both", bagging_fraction = 0.5, bagging_freq = 1, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  # Start timestamp
  start_time <- Sys.time()
  
  all_tree_forecasts <- list()
  
  num_indexes <- round(length(training_set) * bagging_fraction)
  num_features <- round(lag * feature_fraction)
  
  
  # Get forecasts from multiple setar trees as required
  for(bag_f in 1:bagging_freq){
    set.seed(bag_f)
    tree_indexes <- sort(sample(1:length(training_set), num_indexes, replace = FALSE))
    
    result <- create_input_matrix(training_set[tree_indexes], lag)
    embedded_series <- result[[1]]
    full_final_lags <- result[[2]]
  
    set.seed(bag_f)
    feature_indexes <- sort(sample(1:lag, num_features, replace = FALSE))
    
    current_tree_data <- embedded_series[, c(1, (feature_indexes + 1))]
    
    iter_forecasts <- setar_tree_forecasting(current_tree_data, lag, forecast_horizon, full_final_lags, feature_indexes, depth, significance, seq_significance, stopping_criteria, error_threshold, FALSE, FALSE, num_leaves, min_data_in_leaf)
    all_iter_forecasts <- matrix(NA, nrow = length(training_set), ncol = forecast_horizon)
    all_iter_forecasts[tree_indexes,] <- iter_forecasts
    all_tree_forecasts[[bag_f]] <- all_iter_forecasts
  }
  
  final_forecasts <- apply(simplify2array(all_tree_forecasts), 1:2, mean, na.rm = T)
  
  
  # Finish timestamp
  end_time <- Sys.time()
  
  if(integer_conversion)
    final_forecasts <- round(final_forecasts)
  
  file_name <- paste0(dataset_name, "_lag_", lag, "_depth_", depth, "_min_leaf_", min_data_in_leaf, "_leaves_", num_leaves, "_setar_forest_block_bootstrap_", bagging_freq, "_bagging_frac_", bagging_fraction, "_feature_frac_", feature_fraction, "_", stopping_criteria)
  
  if(stopping_criteria != "lin_test")
    file_name <- paste0(file_name, "_error_threshold_", error_threshold)
  
  if(seq_significance)
    file_name <- paste0(file_name, "_seq_significance")
  
  write.table(final_forecasts, file.path(BASE_DIR, "results", "forecasts", "setar_forest", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", "setar_forest", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
  
  calculate_errors(final_forecasts, test_set, training_set, seasonality, file_name)
}



# Experiments

# Chaotic Logistic
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 1)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 0.5)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 1)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 0.5)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.8, bagging_freq = 15, feature_fraction = 1, error_threshold = 0.1)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 500, feature_fraction = 1, depth = 5)
do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)

# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 1, random_lag = TRUE)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 0.5, random_lag = TRUE)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 1, random_lag = TRUE)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 0.5, random_lag = TRUE)

# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 1, random_lag = TRUE, random_threshold = TRUE)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 0.5, random_lag = TRUE, random_threshold = TRUE)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 1, random_lag = TRUE, random_threshold = TRUE)
# do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 0.5, random_lag = TRUE, random_threshold = TRUE)

# do_setar_forest_block_bootstrap_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)
do_setar_forest_block_bootstrap_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.8, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)



# Mackey glass
# do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 1)
# do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 0.5)
# do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 1)
# do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 0.5)
# do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.8, bagging_freq = 15, feature_fraction = 1, error_threshold = 0.1)
# do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.5, bagging_freq = 500, feature_fraction = 1, depth = 5)
do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.5, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)

do_setar_forest_block_bootstrap_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.8, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)



# Tourism Quarterly
# do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 1)
# do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 0.5)
# do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 1)
# do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 0.5)
# do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.8, bagging_freq = 15, feature_fraction = 1, error_threshold = 0.1)
# do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.5, bagging_freq = 500, feature_fraction = 1, depth = 5)
do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.5, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)

do_setar_forest_block_bootstrap_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.8, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)



# Kaggle Daily
# do_setar_forest_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 1)
# do_setar_forest_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 0.5)
# do_setar_forest_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 1)
# do_setar_forest_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 0.5)
# do_setar_forest_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 500, feature_fraction = 1, depth = 5)
# do_setar_forest_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)

# do_setar_forest_block_bootstrap_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, bagging_fraction = 0.8, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)



# Rossmann
# do_setar_forest_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 1)
# do_setar_forest_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 5, feature_fraction = 0.5)
# do_setar_forest_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 1)
# do_setar_forest_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 10, feature_fraction = 0.5)
# do_setar_forest_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 500, feature_fraction = 1, depth = 5)
# do_setar_forest_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = T, bagging_fraction = 0.5, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)

# do_setar_forest_block_bootstrap_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = T, bagging_fraction = 0.8, bagging_freq = 100, feature_fraction = 1, num_leaves = 64, min_data_in_leaf = 100)

