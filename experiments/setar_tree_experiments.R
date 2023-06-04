BASE_DIR <- "SETAR_Trees"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


# Function to execute SETAR tree

# Parameters
# input_file_name - .tsf file name
# lag - The number of past lags that should be considered during training
# forecast_horizon - The expected forecast horizon
# dataset_name - Name of the dataset
# depth - Maximum tree depth. This is 1000 by default and thus, the depth will be actually controlled by the stopping criterias unless you specificed a lower value
# key - The name of the attribute that should be used as the key when creating the tsibble from the .tsf file. If doesn't provide, a data frame will be returned instead of a tsibble
# index - The name of the time attribute that should be used as the index when creating the tsibble from the .tsf file. If doesn't provide, it will search for a valid index. When no valid index found, a data frame will be returned instead of a tsibble
# integer_conversion - Whether the final forecasts should be rounded or not
# significance - Initial significance used by the linearity test (alpha_0)
# scale - Whether the series should be normalized before training. When TRUE, mean normalization is applied to each series
# seq_significance - Whether the significance used by the linearity test is reduced in each tree level
# significance_divider - If sequence significance is used, then in each tree level, the current significance will be divided by this value
# error_threshold - The minimum error reduction percentage between parent and child nodes to make a split
# stopping_criteria - The required stopping criteria: linearity test (lin_test), error reduction percentage (error_imp), linearity test and error reduction percentage (both)
# fixed_lag - Whether the split should be done using a fixed lag. Otherwise, the optimal lag will be found for each split
# external_lag - If fixed_lag=TRUE, users can specify a lag which should be used for splitting
# categorical_covariates - A vector containing the names of external categorical covariates. The .tsf file should contain series corresponding with each categorical covariate
# numerical_covariates - A vector containing the names of external numerical covariates. The .tsf file should contain series corresponding with each numerical covariate
# series_prefix - The prefix used to identify original time series in the .tsf file. This is only required when the models are trained with external covariates
# splitter - The splitter used in the names of time series in the .tsf file to separate the series type and number. This is only required when the models are trained with external covariates
do_setar_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 1000, key = "series_name", index = "start_timestamp", integer_conversion = FALSE, significance = 0.05, scale = FALSE, seq_significance = TRUE, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", fixed_lag = FALSE, external_lag = 0, categorical_covariates = NULL, numerical_covariates = NULL, series_prefix = NULL, splitter = "_"){
  
  # Creating training and test sets
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon, categorical_covariates, numerical_covariates, series_prefix, splitter)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  cat_unique_vals <- loaded_data[[3]]
  seasonality <- loaded_data[[4]]
  
  result <- create_tree_input_matrix(training_set, lag, scale, test_set, categorical_covariates, numerical_covariates, cat_unique_vals)
  embedded_series <- result[[1]]
  final_lags <- result[[2]]
  series_means <- result[[3]]
  
  
  # Start timestamp
  all_start_time <- Sys.time()
  
  
  # Set list of defaults
  start.con <- list(nTh = 15) # Number of thresholds considered when making each split to define the optimal lag
  
  
  tree <- list() # Stores the nodes in tree in each level
  th_lags <- list() # Stores the optimal lags used during splitting
  thresholds <- list() # Stores the optimal thresholds used during splitting
  level_errors <- NULL
  
  node_data <- list(embedded_series) # Root node contains the training instances coming from all series
  
  split_info <- 1
  
  categorical_indexes <- NULL
  
  # Identify the column indexes of covariates in the test set
  if(!is.null(categorical_covariates)){
    if(is.null(numerical_covariates))
      categorical_indexes <- (lag+1):ncol(final_lags)
    else
      categorical_indexes <- (lag+1):(ncol(final_lags) - length(numerical_covariates))
  }
  
  
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
      
      
      if((nrow(node_data[[n]]) >  (2 * (ncol(embedded_series) - 1) + 2)) & split_info[n] == 1){ 
        
        if(fixed_lag){
          if(external_lag > 0)
            lg <- external_lag
          else
            lg <- 1
          
          ss_output <- find.cut.point(as.matrix(node_data[[n]][,-1]), as.matrix(node_data[[n]][,1]), node_data[[n]][,lg+1], start.con$nTh, lag, criterion = "RSS")
          cost <- ss_output[["RSS.left"]] + ss_output[["RSS.right"]]
          
          if(cost <= best_cost) { # Find th and th_lag which minimizes the squared errors
            best_cost <- cost
            th <- ss_output[["cut.point"]]
            th_lag <- lg
          }
        }else{
          for(lg in 1:(ncol(embedded_series) - 1)){ 
            print(paste0("Lag ", lg))
            
            # Finding the optimal lag and threshold that should be used for splitting
            # Optimised grid search
            ss_output <- find.cut.point(as.matrix(node_data[[n]][,-1]), as.matrix(node_data[[n]][,1]), node_data[[n]][,lg+1], start.con$nTh, lag, criterion = "RSS")
            cost <- ss_output[["RSS.left"]] + ss_output[["RSS.right"]]
            recheck <- ss_output[["need_recheck"]]
            
            if(recheck > round(start.con$nTh*0.6)){
              # If optimised grid search fails, then try with SS()
              if(!is.null(categorical_indexes) & (lg %in% categorical_indexes))
                ths <- 1
              else
                ths <- seq(min(node_data[[n]][,lg+1]), max(node_data[[n]][,lg+1]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the corresponding lag
              
              for(ids in 1:length(ths)){
                cost <- SS(ths[ids], node_data[[n]], lg)
                
                if(cost <= best_cost) { # Find th and th_lag which minimizes the squared errors
                  best_cost <- cost
                  th <- ths[ids]
                  th_lag <- lg
                }
              }
            }else{
              if(cost <= best_cost) { # Find th and th_lag which minimize the squared errors
                best_cost <- cost
                th <- ss_output[["cut.point"]]
                th_lag <- lg
              }
            }
          }
        }
        
        if(best_cost != Inf){
          splited_nodes <- create_split(node_data[[n]], th_lag, th) # Get the child nodes
          
          # Check whether making the split is worth enough
          if(stopping_criteria == "lin_test")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, ncol(embedded_series) - 1, significance)
          else if(stopping_criteria == "error_imp")
            is_significant <- check_error_improvement(node_data[[n]], splited_nodes, error_threshold)
          else if(stopping_criteria == "both")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, ncol(embedded_series) - 1, significance) & check_error_improvement(node_data[[n]], splited_nodes, error_threshold) 
          
          if(is_significant){ # Split the node into child nodes only if making that split is worth enough
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
      
      if(seq_significance) # Defining the significance for the next level of the tree
        significance <- significance/significance_divider
    }else
      break # If all nodes in a particular tree level are not further splitted, then stop
  }
  
  
  # Model training
  # Check whether the tree has any nodes. If not, train a single pooled regression model
  if(length(tree) > 0){
    leaf_nodes <- tree[[length(tree)]]
    leaf_trained_models <- list()
    num_of_leaf_instances <- NULL
    
    # Train a linear model per each leaf node
    for(ln in 1:length(leaf_nodes)){
      leaf_trained_models[[ln]] <- fit_global_model(leaf_nodes[[ln]], NULL)[["model"]] 
      num_of_leaf_instances <- c(num_of_leaf_instances, nrow(leaf_nodes[[ln]]))
    }
  }else{
    final_trained_model <- fit_global_model(embedded_series, NULL)[["model"]]
  }
  
  
  file_name <- paste0(dataset_name, "_setartree_", stopping_criteria)
  
  if(length(tree) > 0){ # Recording tree information such as the tree depth and number of nodes in the leaf level
    dir.create(file.path(BASE_DIR, "results", "tree_info", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
    write(paste("No: of nodes in leaf level:", length(leaf_nodes)), file = file.path(BASE_DIR, "results", "tree_info", paste0(file_name, ".txt")), append = FALSE)
    write(paste("Tree depth:", length(tree)), file = file.path(BASE_DIR, "results", "tree_info", paste0(file_name, ".txt")), append = TRUE)
    write.table(num_of_leaf_instances, file.path(BASE_DIR, "results", "tree_info", paste0(file_name, ".txt")), row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
  }else{
    write(paste("No: of nodes in leaf level:", 1), file = file.path(BASE_DIR, "results", "tree_info", paste0(file_name, ".txt")), append = FALSE)
    write(paste("Tree depth:", 0), file = file.path(BASE_DIR, "results", "tree_info", paste0(file_name, ".txt")), append = TRUE)
  } 
  
  # Forecasting start timestamp
  forecasting_start_time <- Sys.time()
  
  forecasts <- NULL
  
  for(h in 1:forecast_horizon){
    print(paste0("Horizon = ", h))
    horizon_predictions <- NULL
    
    if(length(tree) > 0){
      for(f in 1:nrow(final_lags)){
        inst <- as.data.frame(final_lags[f,])
        
        if(lag == 1)
          colnames(inst) <- "Lag1"
        
        leaf_index <- get_leaf_index(inst, th_lags, thresholds) # Identify the leaf node corresponding with a given test instance
        leaf_model <- leaf_trained_models[[leaf_index]]
        
        horizon_predictions <- c(horizon_predictions, predict.glm(object = leaf_model, newdata = inst)) 
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
      
      # Updating categorical covariates for the next horizon
      if(!is.null(categorical_covariates)){
        for(cat_cov in categorical_covariates){
          if(cat_unique_vals[[cat_cov]] > 2){
            new_cat_cov_final_lags <- do_one_hot_encoding(test_set[[cat_cov]][, (h+1)], cat_unique_vals[[cat_cov]], cat_cov)
            final_lags[,colnames(new_cat_cov_final_lags)] <- new_cat_cov_final_lags
          }else{
            final_lags[[cat_cov]] <- test_set[[cat_cov]][, (h+1)]
          }
          
        }
      }
      
      # Updating numerical covariates for the next horizon
      if(!is.null(numerical_covariates)){
        for(num_cov in numerical_covariates)
          final_lags[[num_cov]] <- test_set[[num_cov]][, (h+1)]
      }
      
      final_lags <- as.data.frame(final_lags)
    }
  }
  
  if(scale)
    forecasts <- forecasts * as.vector(series_means)
  
  # Training + forecasting finish timestamp
  all_end_time <- Sys.time()
  
  if(integer_conversion)
    forecasts <- round(forecasts)
  
  dir.create(file.path(BASE_DIR, "results", "forecasts", "setar_tree", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", "setar_tree", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  print(th_lags)
  print(thresholds)
  print(paste0("Executed for ", length(tree), " levels"))
  
  if(length(tree) > 1)
    print(paste0("Insignificant node count = ", length(tree[[length(tree) - 1]]) - level_significant_node_count))
  
  # Execution time
  dir.create(file.path(BASE_DIR, "results", "execution_times", "setar_tree", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  all_exec_time <- all_end_time - all_start_time
  print(all_exec_time)
  forecasting_exec_time <- all_end_time - forecasting_start_time
  write(paste("Full execution time = ", all_exec_time, attr(all_exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", "setar_tree", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
  write(paste("Forecasting time = ", forecasting_exec_time, attr(forecasting_exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", "setar_tree", paste0(file_name, ".txt"), fsep = "/"), append = TRUE)
  write(paste("One interval forecasting time = ", forecasting_exec_time/forecast_horizon, attr(forecasting_exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", "setar_tree", paste0(file_name, ".txt"), fsep = "/"), append = TRUE)
  
  # Error calculations
  calculate_errors(forecasts, test_set$series, training_set$series, seasonality, file_name)
}



# Experiments

# Chaotic Logistic
do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, stopping_criteria = "lin_test")
do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, stopping_criteria = "error_imp")
do_setar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, stopping_criteria = "both")


# Mackey-Glass
do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, stopping_criteria = "lin_test")
do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, stopping_criteria = "error_imp")
do_setar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, stopping_criteria = "both")


# Tourism Quarterly
do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", stopping_criteria = "lin_test")
do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", stopping_criteria = "error_imp")
do_setar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", stopping_criteria = "both")


# Tourism Monthly
do_setar_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", stopping_criteria = "lin_test")
do_setar_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", stopping_criteria = "error_imp")
do_setar_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", stopping_criteria = "both")


# M5
do_setar_forecasting("m5_dataset.tsf", 10, 28, "m5", stopping_criteria = "lin_test")
do_setar_forecasting("m5_dataset.tsf", 10, 28, "m5", stopping_criteria = "error_imp")
do_setar_forecasting("m5_dataset.tsf", 10, 28, "m5", stopping_criteria = "both")


# Rossmann
# Without covariates
do_setar_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = TRUE, stopping_criteria = "lin_test")
do_setar_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = TRUE, stopping_criteria = "error_imp")
do_setar_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = TRUE, stopping_criteria = "both")

# With covariates
do_setar_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", index = NULL, integer_conversion = TRUE, stopping_criteria = "lin_test", categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_setar_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", index = NULL, integer_conversion = TRUE, stopping_criteria = "error_imp", categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_setar_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", index = NULL, integer_conversion = TRUE, stopping_criteria = "both", categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")


# Kaggle Web Traffic
# Without covariates
do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = TRUE, stopping_criteria = "lin_test")
do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = TRUE, stopping_criteria = "error_imp")
do_setar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = TRUE, stopping_criteria = "both")

# With covariates
do_setar_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", index = NULL, integer_conversion = TRUE, stopping_criteria = "lin_test", categorical_covariates = "wday", series_prefix = "T")
do_setar_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", index = NULL, integer_conversion = TRUE, stopping_criteria = "both", categorical_covariates = "wday", series_prefix = "T")
do_setar_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", index = NULL, integer_conversion = TRUE, stopping_criteria = "error_imp", categorical_covariates = "wday", series_prefix = "T")


# Favourita 
# Without covariates
do_setar_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", index = NULL, stopping_criteria = "lin_test")
do_setar_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", index = NULL, stopping_criteria = "error_imp")
do_setar_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", index = NULL, stopping_criteria = "both")

# With covariates
do_setar_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", index = NULL, stopping_criteria = "lin_test", categorical_covariates = "wday", series_prefix = "T")
do_setar_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", index = NULL, stopping_criteria = "error_imp", categorical_covariates = "wday", series_prefix = "T")
do_setar_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", index = NULL, stopping_criteria = "both", categorical_covariates = "wday", series_prefix = "T")


