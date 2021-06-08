# BASE_DIR <- "C:/Projects/SETAR_Trees/"
BASE_DIR <- "/home/rakshitha/Trees/"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


# input_file_name <- "rossmann_data_with_corvariates.tsf"
# lag <- 10
# key <- "series_name"
# index <- NULL
# forecast_horizon <- 48
# dataset_name = "rossmann"
# integer_conversion = T
# depth = 2
# significance = 0.05
# scale = FALSE
# seq_significance = T
# significance_divider = 2
# error_threshold = 0.03
# stopping_criteria = "both"
# fixed_lag = FALSE
# external_lag = 0
# categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday")
# numerical_covariates = "Customers"
# series_prefix = "T"
# splitter = "_"


do_setar_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 2, key = "series_name", index = "start_timestamp", integer_conversion = F, significance = 0.05, scale = FALSE, seq_significance = TRUE, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", fixed_lag = FALSE, external_lag = 0, categorical_covariates = NULL, numerical_covariates = NULL, series_prefix = NULL, splitter = "_"){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon, categorical_covariates, numerical_covariates, series_prefix, splitter)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  result <- create_input_matrix(training_set, lag, scale, test_set, categorical_covariates, numerical_covariates)
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
  
  if(!is.null(categorical_covariates))
    categorical_indexes <- (which(colnames(embedded_series) %in% categorical_covariates)) - 1
  else
    categorical_indexes <- NULL
  
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
      
      
      if(nrow(node_data[[n]]) > (2 * (ncol(embedded_series) - 1) + 2) & split_info[n] == 1){ # When this condition is not satisfied, the F-statistic will be negative and model will become unidentifiable
        
        if(fixed_lag){
          if(external_lag > 0)
            lg <- external_lag
          else
            lg <- 1
          
          if(!is.null(categorical_covariates) & !is.null(categorical_indexes) & (lg %in% categorical_indexes))
            ths <- as.numeric(as.character(unique(node_data[[n]][,lg+1])))
          else  
            ths <- seq(min(node_data[[n]][,lg+1]), max(node_data[[n]][,lg+1]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the corresponding lag
          
          for(ids in 1:length(ths)){
            cost <- SS(ths[ids], node_data[[n]], lg, categorical_covariates)
            
            if(cost <= best_cost) { # find th which minimizes the squared errors
              best_cost <- cost;
              th <- ths[ids]
              th_lag <- lg
            }
          }
        }else{
          for(lg in 1:(ncol(embedded_series) - 1)){
            print(paste0("Lag ", lg))
            
            # print("Start hyperparameter tuning with grid search")
            if(!is.null(categorical_covariates) & !is.null(categorical_indexes) & (lg %in% categorical_indexes))
              ths <- as.numeric(as.character(unique(node_data[[n]][,lg+1])))
            else 
              ths <- seq(min(node_data[[n]][,lg+1]), max(node_data[[n]][,lg+1]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the corresponding lag
            
            for(ids in 1:length(ths)){
              cost <- SS(ths[ids], node_data[[n]], lg, categorical_covariates)
              
              if(cost <= best_cost) { # find th which minimizes the squared errors
                best_cost <- cost;
                th <- ths[ids]
                th_lag <- lg
              }
            }
          }
        }
        
        if(best_cost != Inf){
          splited_nodes <- create_split(node_data[[n]], th_lag, th, categorical_covariates)
          
          if(stopping_criteria == "lin_test")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, ncol(embedded_series) - 1, significance, categorical_covariates)
          else if(stopping_criteria == "error_imp")
            is_significant <- check_error_improvement(node_data[[n]], splited_nodes, error_threshold, categorical_covariates)
          else if(stopping_criteria == "both")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, ncol(embedded_series) - 1, significance, categorical_covariates) & check_error_improvement(node_data[[n]], splited_nodes, error_threshold, categorical_covariates) 
          
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
      leaf_trained_models[[ln]] <- fit_global_model(leaf_nodes[[ln]], NULL, categorical_covariates)[["model"]] 
      num_of_leaf_instances <- c(num_of_leaf_instances, nrow(leaf_nodes[[ln]]))
    }
  }else{
    final_trained_model <- fit_global_model(embedded_series, NULL, categorical_covariates)[["model"]]
  }
  
  
  file_name <- paste0(dataset_name, "_lag_", lag, "_depth_", depth, "_setar_", stopping_criteria)
  
  if(!is.null(categorical_covariates) | !is.null(numerical_covariates))
    file_name <- paste0(file_name, "_with_covariates")
  
  if(fixed_lag)
    file_name <- paste0(file_name, "_fixed_lag")
  
  if(stopping_criteria != "lin_test")
    file_name <- paste0(file_name, "_error_threshold_", error_threshold)
  
  # if(seq_significance)
  #   file_name <- paste0(file_name, "_seq_significance_", significance_divider)
  
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
    print(paste0("Horizon = ", h))
    horizon_predictions <- NULL
    
    if(length(tree) > 0){
      for(f in 1:nrow(final_lags)){
        pred_instance <- final_lags[f,]
        leaf_index <- get_leaf_index(final_lags[f,], th_lags, thresholds, categorical_covariates)
        leaf_model <- leaf_trained_models[[leaf_index]]
        
        if(!is.null(categorical_covariates)){
          check_result <- check_leaf_data_matching(leaf_nodes[[leaf_index]], pred_instance, categorical_covariates)
          pred_instance <- check_result[[2]]
          
          if(check_result[[3]]){
            leaf_model <- fit_global_model(check_result[[1]], NULL, categorical_covariates)[["model"]] 
          }
        }
        
        horizon_predictions <- c(horizon_predictions, predict.glm(object = leaf_model, newdata = as.data.frame(pred_instance))) 
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
      
      if(!is.null(categorical_covariates)){
        for(cat_cov in categorical_covariates){
          final_lags[[cat_cov]] <- test_set[[cat_cov]][, (h+1)]
          final_lags[[cat_cov]] <- as.factor(final_lags[[cat_cov]])
        }
      }
      
      if(!is.null(numerical_covariates)){
        for(num_cov in numerical_covariates)
          final_lags[[num_cov]] <- test_set[[num_cov]][, (h+1)]
      }
      
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
  
  calculate_errors(forecasts, test_set$series, training_set$series, seasonality, file_name)
}


# Experiments

# Chaotic Logistic
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1, index = NULL, significance = 0.001, tune_method = "genoud")
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 4, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 5, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 6, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 7, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 8, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 9, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 10, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1000, index = NULL, significance = 0.001)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1000, index = NULL, significance = 0.05, seq_significance = TRUE)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1000, index = NULL, stopping_criteria = "error_imp", error_threshold = 0.03)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1000, index = NULL, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1000, index = NULL, stopping_criteria = "lin_test", fixed_lag = T)



# Mackey glass
# do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 1, index = NULL, significance = 0.001)
# do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 4, index = NULL, significance = 0.001)
# do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 6, index = NULL, significance = 0.001)
# do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 10, index = NULL, significance = 0.001)
# do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 1000, index = NULL, significance = 0.001)
# do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 1000, index = NULL, significance = 0.05, seq_significance = TRUE)
# do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 1000, index = NULL, stopping_criteria = "error_imp", error_threshold = 0.03)
# do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 1000, index = NULL, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 1000, index = NULL, stopping_criteria = "lin_test", fixed_lag = T)



# Tourism Quarterly
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 1, significance = 0.001)
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 6, significance = 0.001)
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 10, significance = 0.001)
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 1000, significance = 0.001)
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 1000, significance = 0.05, seq_significance = TRUE)
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 1000, significance = 0.05, seq_significance = TRUE, significance_divider = 10)
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 1000, significance = 0.05, seq_significance = TRUE, significance_divider = 100)
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 1000, stopping_criteria = "error_imp", error_threshold = 0.03)
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 1000, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 1000, stopping_criteria = "lin_test", fixed_lag = T)



# Rossmann
# do_setar_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", depth = 10, significance = 0.001)
# do_setar_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", depth = 1000, significance = 0.05, seq_significance = TRUE)
# do_setar_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", depth = 1000, integer_conversion = T, stopping_criteria = "error_imp", error_threshold = 0.03)
# do_setar_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", depth = 1000, integer_conversion = T, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", depth = 1000, integer_conversion = T, stopping_criteria = "lin_test", fixed_lag = T)

# do_setar_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann", depth = 1000, index = NULL, integer_conversion = T, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
# do_setar_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann", depth = 1000, index = NULL, integer_conversion = T, stopping_criteria = "lin_test", categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
# do_setar_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann", depth = 1000, index = NULL, integer_conversion = T, stopping_criteria = "error_imp", categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")



# Restaurant Visitors
# do_setar_forecasting("restaurant_visitors_dataset.tsf", 10, 39, "restaurant", depth = 1000, index = NULL, integer_conversion = T, stopping_criteria = "both")
do_setar_forecasting("restaurant_with_corvariates.tsf", 10, 39, "restaurant", depth = 1000, index = NULL, integer_conversion = T, stopping_criteria = "both", categorical_covariates = c("AirGenreName", "AreaName", "DayofWeek", "HolidayFlag"), series_prefix = "T")

do_setar_forecasting("restaurant_visitors_dataset.tsf", 10, 39, "restaurant", depth = 1000, index = NULL, integer_conversion = T, stopping_criteria = "lin_test")
do_setar_forecasting("restaurant_visitors_dataset.tsf", 10, 39, "restaurant", depth = 1000, index = NULL, integer_conversion = T, stopping_criteria = "error_imp")

do_setar_forecasting("restaurant_with_corvariates.tsf", 10, 39, "restaurant", depth = 1000, index = NULL, integer_conversion = T, stopping_criteria = "lin_test", categorical_covariates = c("AirGenreName", "AreaName", "DayofWeek", "HolidayFlag"), series_prefix = "T")
do_setar_forecasting("restaurant_with_corvariates.tsf", 10, 39, "restaurant", depth = 1000, index = NULL, integer_conversion = T, stopping_criteria = "error_imp", categorical_covariates = c("AirGenreName", "AreaName", "DayofWeek", "HolidayFlag"), series_prefix = "T")



# Walmart Store Sales
# do_setar_forecasting("walmart_store_sales_dataset.tsf", 10, 39, "walmart", depth = 1000, index = NULL, stopping_criteria = "both")
# do_setar_forecasting("walmart_store_sales_dataset.tsf", 10, 39, "walmart", depth = 1000, index = NULL, stopping_criteria = "lin_test")
# do_setar_forecasting("walmart_store_sales_dataset.tsf", 10, 39, "walmart", depth = 1000, index = NULL, stopping_criteria = "error_imp")

do_setar_forecasting("walmart_data_with_corvariates.tsf", 10, 39, "walmart", depth = 1000, index = NULL, stopping_criteria = "both", categorical_covariates = c("IsHoliday"), numerical_covariates = c("MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5"), series_prefix = "T")
do_setar_forecasting("walmart_data_with_corvariates.tsf", 10, 39, "walmart", depth = 1000, index = NULL, stopping_criteria = "lin_test", categorical_covariates = c("IsHoliday"), numerical_covariates = c("MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5"), series_prefix = "T")
do_setar_forecasting("walmart_data_with_corvariates.tsf", 10, 39, "walmart", depth = 1000, index = NULL, stopping_criteria = "error_imp", categorical_covariates = c("IsHoliday"), numerical_covariates = c("MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5"), series_prefix = "T")



# Kaggle Daily
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 74, 59, "kaggle_daily", depth = 1, integer_conversion = T, significance = 0.001)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1, integer_conversion = T, significance = 0.001)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 4, integer_conversion = T, significance = 0.001)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 6, integer_conversion = T, significance = 0.001)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1000, integer_conversion = T, significance = 0.001)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1000, integer_conversion = T, significance = 0.05, seq_significance = TRUE)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1000, integer_conversion = T, significance = 0.05, seq_significance = TRUE, significance_divider = 10)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1000, integer_conversion = T, significance = 0.05, seq_significance = TRUE, significance_divider = 100)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1000, integer_conversion = T, stopping_criteria = "error_imp", error_threshold = 0.03)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1000, integer_conversion = T, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1000, integer_conversion = T, stopping_criteria = "lin_test", fixed_lag = T)
# do_setar_forecasting("kaggle_web_traffic_997_dataset.tsf", 10, 59, "kaggle_daily_997", depth = 1000, integer_conversion = T, stopping_criteria = "both", error_threshold = 0.03)


# Need to run
# do_setar_forecasting("kaggle_web_traffic_dataset_10000.tsf", 10, 59, "kaggle_daily_10000", depth = 1000, integer_conversion = T, stopping_criteria = "both")
# do_setar_forecasting("kaggle_web_traffic_dataset_10000.tsf", 10, 59, "kaggle_daily_10000", depth = 1000, integer_conversion = T, stopping_criteria = "lin_test")
# do_setar_forecasting("kaggle_web_traffic_dataset_10000.tsf", 10, 59, "kaggle_daily_10000", depth = 1000, integer_conversion = T, stopping_criteria = "error_imp")

# do_setar_forecasting("kaggle_with_corvariates.tsf", 10, 59, "kaggle_daily_10000", index = NULL, depth = 1000, integer_conversion = T, stopping_criteria = "both", categorical_covariates = c("agent", "access", "project"), series_prefix = "T")
# do_setar_forecasting("kaggle_with_corvariates.tsf", 10, 59, "kaggle_daily_10000", index = NULL, depth = 1000, integer_conversion = T, stopping_criteria = "lin_test", categorical_covariates = c("agent", "access", "project"), series_prefix = "T")
# do_setar_forecasting("kaggle_with_corvariates.tsf", 10, 59, "kaggle_daily_10000", index = NULL, depth = 1000, integer_conversion = T, stopping_criteria = "error_imp", categorical_covariates = c("agent", "access", "project"), series_prefix = "T")



# Favourita Sales
# do_setar_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", depth = 1000, index = NULL, stopping_criteria = "both")
# do_setar_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", depth = 1000, index = NULL, stopping_criteria = "both", error_threshold = 0.001)
# do_setar_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", depth = 1000, index = NULL, stopping_criteria = "lin_test")
# do_setar_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", depth = 1000, index = NULL, stopping_criteria = "error_imp", error_threshold = 0.001)


# Need to run
# do_setar_forecasting("favourita_sales_10000_dataset.tsf", 10, 16, "favourita_10000", depth = 1000, index = NULL, stopping_criteria = "both")
# do_setar_forecasting("favourita_sales_10000_dataset.tsf", 10, 16, "favourita_10000", depth = 1000, index = NULL, stopping_criteria = "lin_test")
# do_setar_forecasting("favourita_sales_10000_dataset.tsf", 10, 16, "favourita_10000", depth = 1000, index = NULL, stopping_criteria = "error_imp")

# do_setar_forecasting("favourita_with_corvariates.tsf", 10, 16, "favourita_10000", depth = 1000, index = NULL, stopping_criteria = "both", categorical_covariates = c("onpromotion", "family", "perishable", "city"), series_prefix = "T"))
# do_setar_forecasting("favourita_with_corvariates.tsf", 10, 16, "favourita_10000", depth = 1000, index = NULL, stopping_criteria = "lin_test", categorical_covariates = c("onpromotion", "family", "perishable", "city"), series_prefix = "T"))
# do_setar_forecasting("favourita_with_corvariates.tsf", 10, 16, "favourita_10000", depth = 1000, index = NULL, stopping_criteria = "error_imp", categorical_covariates = c("onpromotion", "family", "perishable", "city"), series_prefix = "T"))



















# Other datasets
# do_setar_forecasting("nn5_weekly_dataset.tsf", 10, 8, "nn5_weekly", depth = 1000, significance = 0.001)
# do_setar_forecasting("nn5_weekly_dataset.tsf", 10, 8, "nn5_weekly", depth = 1000, significance = 0.05, seq_significance = TRUE)
# do_setar_forecasting("nn5_weekly_dataset.tsf", 10, 8, "nn5_weekly", depth = 1000, stopping_criteria = "error_imp", error_threshold = 0.01)
# do_setar_forecasting("nn5_weekly_dataset.tsf", 10, 8, "nn5_weekly", depth = 1000, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("nn5_weekly_dataset.tsf", 10, 8, "nn5_weekly", depth = 1000, stopping_criteria = "lin_test", fixed_lag = T)

# do_setar_forecasting("electricity_weekly_dataset.tsf", 10, 8, "electricity_weekly", depth = 1000, integer_conversion = T, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("m3_quarterly_dataset.tsf", 10, 8, "m3_quarterly", depth = 1000, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("m1_quarterly_dataset.tsf", 10, 8, "m1_quarterly", depth = 10, significance = 0.001)
# do_setar_forecasting("traffic_weekly_dataset.tsf", 10, 8, "traffic_weekly", depth = 1000, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", depth = 1000, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("m3_monthly_dataset.tsf", 15, 18, "m3_monthly", depth = 1000, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("m1_monthly_dataset.tsf", 15, 18, "m1_monthly", depth = 1000, stopping_criteria = "both", error_threshold = 0.03)
# do_setar_forecasting("hospital_dataset.tsf", 15, 12, "hospital", depth = 1000, stopping_criteria = "both", error_threshold = 0.03, integer_conversion = T)
