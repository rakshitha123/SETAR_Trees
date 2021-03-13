BASE_DIR <- "C:/Projects/Trees/"
DATASET_DIR <- "C:/Projects/TSForecasting/"

source(file.path(DATASET_DIR, "utils", "data_loader.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "global_model_helper.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))

set.seed(1)

LOW_FREQUENCIES <- c("4_seconds", "minutely", "10_minutes", "half_hourly", "hourly")
HIGH_FREQUENCIES <- c("daily", "weekly", "monthly", "quarterly", "yearly")
FREQUENCIES <- c(LOW_FREQUENCIES, HIGH_FREQUENCIES)

VALUE_COL_NAME <- "series_value"

# seasonality values corresponding with the frequencies: 4_seconds, minutely, 10_minutes, half_hourly, hourly, daily, weekly, monthly, quarterly and yearly
# consider multiple seasonalities for frequencies less than daily
SEASONALITY_VALS <- list()
SEASONALITY_VALS[[1]] <- c(21600, 151200, 7889400)
SEASONALITY_VALS[[2]] <- c(1440, 10080, 525960)
SEASONALITY_VALS[[3]] <- c(144, 1008, 52596)
SEASONALITY_VALS[[4]] <- c(48, 336, 17532)
SEASONALITY_VALS[[5]] <- c(24, 168, 8766)
SEASONALITY_VALS[[6]] <- 7
SEASONALITY_VALS[[7]] <- 365.25/7
SEASONALITY_VALS[[8]] <- 12 
SEASONALITY_VALS[[9]] <- 4
SEASONALITY_VALS[[10]] <- 1  

SEASONALITY_MAP <- list()

for(f in seq_along(FREQUENCIES))
  SEASONALITY_MAP[[FREQUENCIES[f]]] <- SEASONALITY_VALS[[f]]


# For testing
# input_file_name <- "nn5_daily_dataset_without_missing_values.ts"
# lag <- 10
# key <- "series_name"
# index <- "start_timestamp"
# forecast_horizon <- 56
# dataset_name = "nn5_daily"
# interger_conversion = F
# threshold_function = "median"
# depth = 3


create_split <- function(data, conditional_lag, threshold_function = "median"){
  threshold <- eval(parse(text = paste0(threshold_function, "(data[['Lag", conditional_lag, "']])")))
  left_node <- data[data[[paste0("Lag", conditional_lag)]] < threshold,]
  right_node <- data[data[[paste0("Lag", conditional_lag)]] >= threshold,]
  list("left_node" = left_node, "right_node" = right_node)
}


optimise_split <- function(data, lag, threshold_function = "median"){
  all_node_errors <- NULL
  for(i in 1:lag){
    nodes <- create_split(data, i, threshold_function)
    
    nodes_errors <- NULL
    
    for(n in 1:length(nodes)){
      current_node <- nodes[[n]]
      current_fitted_values <- as.numeric(fit_global_model(current_node)[["predictions"]])
      current_errors <- abs(current_node$y - current_fitted_values)
      nodes_errors <- c(nodes_errors, current_errors)
    }
    
    all_node_errors <- c(all_node_errors, mean(nodes_errors))
  }
  
  which(all_node_errors == min(all_node_errors))
}


fit_global_model <- function(fitting_data) {
  model <- glm(formula = create_formula(fitting_data), data = fitting_data)
  global_predictions  <- predict.glm(object = model, newdata = as.data.frame(fitting_data[-1]))
  list("predictions" = global_predictions, "model" = model)
}


tree_traverse <- function(instance, split, threshold){
  direction <- "left"
  
  if(instance[[paste0("Lag", split)]] >= threshold)
    direction <- "right"
  
  direction
}


get_leaf_index <- function(instance, splits, thresholds){
  current_split <- 1
  
  for(sp in 1:length(splits)){
    
    direction <- tree_traverse(instance, splits[[sp]][current_split], thresholds[[sp]][current_split])
    
    divide_factor <- 2
    next_possible_splits <- tail(1: (current_split*divide_factor), divide_factor)
    
    if(direction == "left"){
      current_split <- next_possible_splits[1]
    }else{
      current_split <- next_possible_splits[2]
    }
  }
  current_split
}


do_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 2, key = "series_name", index = "start_timestamp", threshold_function = "median", interger_conversion = F){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  result <- create_input_matrix(training_set, lag)
  embedded_series <- result[[1]]
  final_lags <- result[[2]]
  # series_means <- result[[3]]
  
  tree <- list()
  splits <- list()
  thresholds <- list()
  
  node_data <- list(embedded_series)
    
  for(d in 1:depth){
    print(paste0("Depth: ", d))
    level_optimal_splits <- NULL
    level_thresholds <- NULL
    level_nodes <- list()
    
    for(n in 1:length(node_data)){
      optimal_split <- optimise_split(node_data[[n]], lag, threshold_function)
      level_optimal_splits <- c(level_optimal_splits, optimal_split)
      
      threshold <- eval(parse(text = paste0(threshold_function, "(node_data[[n]][['Lag", optimal_split, "']])")))
      level_thresholds <- c(level_thresholds, threshold)
      
      splited_nodes <- create_split(node_data[[n]], optimal_split, threshold_function)
      
      for(s in 1:length(splited_nodes)){
        len <- length(level_nodes)
        level_nodes[[len + 1]] <- splited_nodes[[s]]
      }
    }
    
    splits[[d]] <- level_optimal_splits
    thresholds[[d]] <-level_thresholds
    tree[[d]] <- level_nodes
    
    node_data <- tree[[d]]
  }
  
  leaf_nodes <- tree[[depth]]
  leaf_trained_models <- list()
 
  for(ln in 1:length(leaf_nodes)){
    leaf_trained_models[[ln]] <- fit_global_model(leaf_nodes[[ln]])[["model"]]
  }
  
  forecasts <- NULL
  
  for(h in 1:forecast_horizon){
    horizon_predictions <- NULL
    
    for(f in 1:nrow(final_lags)){
      leaf_index <- get_leaf_index(final_lags[f,], splits, thresholds)
      horizon_predictions <- c(horizon_predictions, predict.glm(object = leaf_trained_models[[leaf_index]], newdata = as.data.frame(final_lags[f,]))) 
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
 
  if(interger_conversion)
    forecasts <- round(forecasts)
  
  file_name <- paste0(dataset_name, "_lag_", lag, "_depth_", depth, "_threshold_", threshold_function)
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote=FALSE)
  
  print(splits)
  print(thresholds)
  
  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
}


do_boosting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 2, key = "series_name", index = "start_timestamp", threshold_function = "median", interger_conversion = F, iterations = 1){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  result <- create_input_matrix(training_set, lag)
  embedded_series <- result[[1]]
  final_lags <- result[[2]]
  # series_means <- result[[3]]
  
  original_y <- embedded_series$y
  original_final_lags <- final_lags
  
  # Obtain main predictions
  forecasts <- NULL
  model <- fit_global_model(embedded_series)[["model"]]
  
  for(h in 1:forecast_horizon){
    horizon_predictions <- predict.glm(object = model, newdata = as.data.frame(final_lags))
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
  
  original_forecasts <- forecasts
  
  current_y <- embedded_series$y
 
  temp_predictions <- as.numeric(fit_global_model(embedded_series)[["predictions"]])
  embedded_series$y <- embedded_series$y - temp_predictions
  
  print(paste0("Start training error: ", mean(abs(embedded_series$y))))
    
  
  # Obtain residual predictions iteratively
  for(bi in 1:iterations){
    tree <- list()
    splits <- list()
    thresholds <- list()
    
    
    node_data <- list(embedded_series)
    
    for(d in 1:depth){
      print(paste0("Depth: ", d))
      level_optimal_splits <- NULL
      level_thresholds <- NULL
      level_nodes <- list()
      
      for(n in 1:length(node_data)){
        optimal_split <- optimise_split(node_data[[n]], lag, threshold_function)
        level_optimal_splits <- c(level_optimal_splits, optimal_split)
        
        threshold <- eval(parse(text = paste0(threshold_function, "(node_data[[n]][['Lag", optimal_split, "']])")))
        level_thresholds <- c(level_thresholds, threshold)
        
        splited_nodes <- create_split(node_data[[n]], optimal_split, threshold_function)
        
        for(s in 1:length(splited_nodes)){
          len <- length(level_nodes)
          level_nodes[[len + 1]] <- splited_nodes[[s]]
        }
      }
      
      splits[[d]] <- level_optimal_splits
      thresholds[[d]] <-level_thresholds
      tree[[d]] <- level_nodes
      
      node_data <- tree[[d]]
    }
    
    leaf_nodes <- tree[[depth]]
    leaf_trained_models <- list()
    
    for(ln in 1:length(leaf_nodes)){
      leaf_trained_models[[ln]] <- fit_global_model(leaf_nodes[[ln]])[["model"]]
    }
    
    
    # Add code here for early stopping if required
    for(es in 1:nrow(embedded_series)){
      leaf_index <- get_leaf_index(embedded_series[es,-1], splits, thresholds)
      leaf_predictions <- predict.glm(object = leaf_trained_models[[leaf_index]], newdata = as.data.frame(embedded_series[es,-1]))
      current_y[es] <- current_y[es] + as.numeric(leaf_predictions)
    }
    
    embedded_series$y <- original_y - current_y
    print(paste0("Iteration ", bi, " error: ", mean(abs(embedded_series$y))))
    
    # Get residual predictions for final lags
    current_final_lags <- original_final_lags
    
    for(h in 1:forecast_horizon){
      horizon_predictions <- NULL
      
      for(f in 1:nrow(current_final_lags)){
        leaf_index <- get_leaf_index(current_final_lags[f,], splits, thresholds)
        horizon_predictions <- c(horizon_predictions, predict.glm(object = leaf_trained_models[[leaf_index]], newdata = as.data.frame(current_final_lags[f,]))) 
      }
      
      forecasts[,h] <- forecasts[,h] + horizon_predictions
      
      # Updating the test set for the next horizon
      if(h < forecast_horizon){
        current_final_lags <- current_final_lags[-lag]
        
        # Updating lags for the next horizon
        current_final_lags <- cbind(original_forecasts[,h], current_final_lags)
        colnames(current_final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
        
        current_final_lags <- as.data.frame(current_final_lags)
      }
    }
  }
  
  
  if(interger_conversion)
    forecasts <- round(forecasts)
  
  file_name <- paste0(dataset_name, "_boosting_lag_", lag, "_depth_", depth, "_threshold_", threshold_function, "_iterations_", iterations)
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote=FALSE)
  
  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
}


do_bagging <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 2, key = "series_name", index = "start_timestamp", threshold_function = "median", interger_conversion = F, num_samples = 2){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  result <- create_input_matrix(training_set, lag)
  embedded_series <- result[[1]]
  original_final_lags <- result[[2]]
  # series_means <- result[[3]]
  
  instance_index_list <- list()
  
  for(s in 1:num_samples){
    instance_index_list[[s]] <- sample(1:nrow(embedded_series), ceiling(nrow(embedded_series)/2))
  }
  
  forecasts_list <- list()
  
  for(sp in 1:num_samples){
    tree <- list()
    splits <- list()
    thresholds <- list()
    
    node_data <- list(embedded_series[instance_index_list[[sp]],])
    
    for(d in 1:depth){
      print(paste0("Depth: ", d))
      level_optimal_splits <- NULL
      level_thresholds <- NULL
      level_nodes <- list()
      
      for(n in 1:length(node_data)){
        optimal_split <- optimise_split(node_data[[n]], lag, threshold_function)
        level_optimal_splits <- c(level_optimal_splits, optimal_split)
        
        threshold <- eval(parse(text = paste0(threshold_function, "(node_data[[n]][['Lag", optimal_split, "']])")))
        level_thresholds <- c(level_thresholds, threshold)
        
        splited_nodes <- create_split(node_data[[n]], optimal_split, threshold_function)
        
        for(s in 1:length(splited_nodes)){
          len <- length(level_nodes)
          level_nodes[[len + 1]] <- splited_nodes[[s]]
        }
      }
      
      splits[[d]] <- level_optimal_splits
      thresholds[[d]] <-level_thresholds
      tree[[d]] <- level_nodes
      
      node_data <- tree[[d]]
    }
    
    leaf_nodes <- tree[[depth]]
    leaf_trained_models <- list()
    
    for(ln in 1:length(leaf_nodes)){
      leaf_trained_models[[ln]] <- fit_global_model(leaf_nodes[[ln]])[["model"]]
    }
    
    iter_forecasts <- NULL
    final_lags <- original_final_lags
    
    for(h in 1:forecast_horizon){
      horizon_predictions <- NULL
      
      for(f in 1:nrow(final_lags)){
        leaf_index <- get_leaf_index(final_lags[f,], splits, thresholds)
        horizon_predictions <- c(horizon_predictions, predict.glm(object = leaf_trained_models[[leaf_index]], newdata = as.data.frame(final_lags[f,]))) 
      }
      
      iter_forecasts <- cbind(iter_forecasts, horizon_predictions)
      
      # Updating the test set for the next horizon
      if(h < forecast_horizon){
        final_lags <- final_lags[-lag]
        
        # Updating lags for the next horizon
        final_lags <- cbind(horizon_predictions, final_lags)
        colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
        
        final_lags <- as.data.frame(final_lags)
      }
    }
    
    print(iter_forecasts)
    forecasts_list[[sp]] <- iter_forecasts
  }
  
  forecasts <- matrix(NA, nrow = nrow(original_final_lags), ncol = forecast_horizon)
  
  for(r in 1:nrow(original_final_lags)){
    row_forecasts <- as.numeric(forecasts_list[[1]][r,])
    
    for(s in 2:num_samples){
      row_forecasts <- row_forecasts + as.numeric(forecasts_list[[s]][r,])
    }
    
    row_forecasts <- row_forecasts/num_samples
    forecasts[r,] <- row_forecasts
  }
  
  
  if(interger_conversion)
    forecasts <- round(forecasts)
  
  file_name <- paste0(dataset_name, "_bagging_lag_", lag, "_depth_", depth, "_threshold_", threshold_function, "_samples_", num_samples)
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote=FALSE)
  
  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
}


# Experiments

# NN5 Daily
do_forecasting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 3)
do_forecasting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 3, threshold_function = "mean")
do_forecasting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 4)
do_forecasting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 4, threshold_function = "mean")
do_forecasting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 5)

do_boosting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 3, iterations = 1)
do_boosting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 3, iterations = 2)
do_boosting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 4, iterations = 1)

do_bagging("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 3, num_samples = 2)
do_bagging("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 3, num_samples = 3)
do_bagging("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 3, num_samples = 4)
do_bagging("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 4, num_samples = 2)
do_bagging("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 4, num_samples = 4)


# Ausgrid Weekly
do_forecasting("ausgrid_weekly_dataset.ts", 52, 8, "ausgrid_weekly", depth = 3)
do_forecasting("ausgrid_weekly_dataset.ts", 52, 8, "ausgrid_weekly", depth = 4)

do_boosting("ausgrid_weekly_dataset.ts", 52, 8, "ausgrid_weekly", depth = 3, iterations = 1)
do_boosting("ausgrid_weekly_dataset.ts", 52, 8, "ausgrid_weekly", depth = 3, iterations = 2)

do_bagging("ausgrid_weekly_dataset.ts", 52, 8, "ausgrid_weekly", depth = 3, num_samples = 2)
do_bagging("ausgrid_weekly_dataset.ts", 52, 8, "ausgrid_weekly", depth = 3, num_samples = 3)
do_bagging("ausgrid_weekly_dataset.ts", 52, 8, "ausgrid_weekly", depth = 3, num_samples = 4)
do_bagging("ausgrid_weekly_dataset.ts", 52, 8, "ausgrid_weekly", depth = 4, num_samples = 2)
do_bagging("ausgrid_weekly_dataset.ts", 52, 8, "ausgrid_weekly", depth = 4, num_samples = 4)


# M4 Weekly 
do_forecasting("m4_weekly_dataset.ts", 52, 13, "m4_weekly", depth = 3)
do_bagging("m4_weekly_dataset.ts", 52, 13, "m4_weekly", depth = 3, num_samples = 2)
do_bagging("m4_weekly_dataset.ts", 52, 13, "m4_weekly", depth = 3, num_samples = 3)




# Kaggle Daily
# do_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 74, 59, "kaggle_daily", depth = 3, interger_conversion = T)


# M3 Monthly
# do_forecasting("m3_monthly_dataset.ts", 23, 18, "m3_monthly", depth = 3)
# do_forecasting("m3_monthly_dataset.ts", 23, 18, "m3_monthly", depth = 3, threshold_function = "mean")
# do_forecasting("m3_monthly_dataset.ts", 23, 18, "m3_monthly", depth = 4)
# do_forecasting("m3_monthly_dataset.ts", 23, 18, "m3_monthly", depth = 5)
# 
# do_forecasting("m3_monthly_demo_dataset.ts", 23, 18, "m3_monthly_demo", depth = 3, index = NULL)
# do_forecasting("m3_monthly_micro_dataset.ts", 23, 18, "m3_monthly_micro", depth = 3, index = NULL)
# do_forecasting("m3_monthly_macro_dataset.ts", 23, 18, "m3_monthly_macro", depth = 3, index = NULL)
# do_forecasting("m3_monthly_industry_dataset.ts", 23, 18, "m3_monthly_industry", depth = 3, index = NULL)
# do_forecasting("m3_monthly_finance_dataset.ts", 23, 18, "m3_monthly_finance", depth = 3, index = NULL)
# do_forecasting("m3_monthly_other_dataset.ts", 23, 18, "m3_monthly_other", depth = 3, index = NULL)
# 
# do_bagging("m3_monthly_demo_dataset.ts", 23, 18, "m3_monthly_demo", depth = 3, index = NULL, num_samples = 2)
# do_bagging("m3_monthly_micro_dataset.ts", 23, 18, "m3_monthly_micro", depth = 3, index = NULL, num_samples = 2)
# do_bagging("m3_monthly_macro_dataset.ts", 23, 18, "m3_monthly_macro", depth = 3, index = NULL, num_samples = 2)
# do_bagging("m3_monthly_industry_dataset.ts", 23, 18, "m3_monthly_industry", depth = 3, index = NULL, num_samples = 2)
# do_bagging("m3_monthly_finance_dataset.ts", 23, 18, "m3_monthly_finance", depth = 3, index = NULL, num_samples = 2)
# do_bagging("m3_monthly_other_dataset.ts", 23, 18, "m3_monthly_other", depth = 3, index = NULL, num_samples = 2)
# 
# aggregate_sub_dataset_errors(c("m3_monthly_demo_bagging_lag_23_depth_3_threshold_median_samples_2_smape_errors",
#                                "m3_monthly_micro_bagging_lag_23_depth_3_threshold_median_samples_2_smape_errors",
#                                "m3_monthly_macro_bagging_lag_23_depth_3_threshold_median_samples_2_smape_errors",
#                                "m3_monthly_finance_bagging_lag_23_depth_3_threshold_median_samples_2_smape_errors",
#                                "m3_monthly_industry_bagging_lag_23_depth_3_threshold_median_samples_2_smape_errors",
#                                "m3_monthly_other_bagging_lag_23_depth_3_threshold_median_samples_2_smape_errors"))
# 
# aggregate_sub_dataset_errors(c("m3_monthly_demo_bagging_lag_23_depth_3_threshold_median_samples_2_mase_errors",
#                                "m3_monthly_micro_bagging_lag_23_depth_3_threshold_median_samples_2_mase_errors",
#                                "m3_monthly_macro_bagging_lag_23_depth_3_threshold_median_samples_2_mase_errors",
#                                "m3_monthly_finance_bagging_lag_23_depth_3_threshold_median_samples_2_mase_errors",
#                                "m3_monthly_industry_bagging_lag_23_depth_3_threshold_median_samples_2_mase_errors",
#                                "m3_monthly_other_bagging_lag_23_depth_3_threshold_median_samples_2_mase_errors"))

