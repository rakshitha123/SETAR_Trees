BASE_DIR <- "SETAR_Trees"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


# Function to execute a SETAR tree
setar_tree_forecasting <- function(tree_data, test_set, cat_unique_vals, lag, forecast_horizon, final_lags, feature_indexes, depth = 1000, significance = 0.5, seq_significance = TRUE, significance_divider = 2, stopping_criteria = "both", error_threshold = 0.03, random_significance = FALSE, random_error_threshold = FALSE, num_leaves = 100000, min_data_in_leaf = 0, categorical_covariates = NULL, numerical_covariates = NULL){
  tree <- list() # Stores the nodes in tree in each level
  th_lags <- list() # Stores the lags used during splitting
  thresholds <- list() # Stores the thresholds used during splitting
  level_errors <- NULL

  # Set list of defaults:
  start.con <- list(nTh = 15) # Number of thresholds considered when making each split to define the optimal lag

  node_data <- list(tree_data) # Root node contains the training instances coming from all series

  split_info <- 1

  categorical_indexes <- NULL

  if(!is.null(categorical_covariates)){
    for(cat in categorical_covariates)
      categorical_indexes <- c(categorical_indexes, grep(cat ,colnames(tree_data)))
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


      if((nrow(node_data[[n]]) > (2* (ncol(node_data[[n]]) - 1) + 2)) & (split_info[n] == 1) & (nrow(node_data[[n]]) >= min_data_in_leaf)){

        for(lg in feature_indexes){  # Find the optimal lag and the threshold to make a split
            print(paste0("Lag ", lg))

            # Optimised grid search
            ss_output <- find.cut.point(as.matrix(node_data[[n]][,-1]), as.matrix(node_data[[n]][,1]), node_data[[n]][,(lg+1)], start.con$nTh, lag, criterion = "RSS")
            cost <- ss_output[["RSS.left"]] + ss_output[["RSS.right"]]
            recheck <- ss_output[["need_recheck"]]

            if(recheck > round(start.con$nTh*0.6)){
              # Old grid search
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

        if(best_cost != Inf){
          splited_nodes <- create_split(node_data[[n]], th_lag, th) # Get the child nodes

          if(random_significance){
            set.seed(d+n)
            significance <- sample(seq(0.00001, 0.0001, length.out = 20), 1)
            print(significance)
          }

          if(random_error_threshold){
            set.seed(d+n)
            error_threshold <- sample(seq(0.01, 0.1, length.out = 10), 1)
            print(error_threshold)
          }

          if(stopping_criteria == "lin_test")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, ncol(tree_data) - 1, significance)
          else if(stopping_criteria == "error_imp")
            is_significant <- check_error_improvement(node_data[[n]], splited_nodes, error_threshold)
          else if(stopping_criteria == "both")
            is_significant <- check_linearity(node_data[[n]], splited_nodes, ncol(tree_data) - 1, significance) & check_error_improvement(node_data[[n]], splited_nodes, error_threshold)

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

    if(level_significant_node_count > 0 & length(level_nodes) <= num_leaves){
      tree[[d]] <- level_nodes
      thresholds[[d]] <- level_thresholds
      th_lags[[d]] <- level_th_lags
      node_data <- tree[[d]]
      split_info <- level_split_info

      if(seq_significance)
          significance <- significance/significance_divider # Defining the significance for the next level of the tree
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
    final_trained_model <- fit_global_model(tree_data, NULL)[["model"]]
  }

  # Forecasting
  forecasts <- NULL

  for(h in 1:forecast_horizon){
    horizon_predictions <- NULL

    if(length(tree) > 0){
      for(f in 1:nrow(final_lags)){
        leaf_index <- get_leaf_index(final_lags[f, feature_indexes], th_lags, thresholds) # Identify the leaf node corresponding with a given test instance
        leaf_model <- leaf_trained_models[[leaf_index]]
        horizon_predictions <- c(horizon_predictions, predict.glm(object = leaf_model, newdata = as.data.frame(final_lags[f, feature_indexes])))
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

  print(th_lags)
  print(thresholds)
  print(paste0("Executed for ", length(tree), " levels"))

  forecasts
}


# Function to execute a SETAR forest

# Parameters
# input_file_name - .tsf file name
# lag - The number of past lags that should be considered during training
# forecast_horizon - The expected forecast horizon
# dataset_name - Name of the dataset
# depth - Maximum tree depth. This is 1000 by default and thus, the depth will be actually controlled by the stopping criterias unless you specified a lower value
# key - The name of the attribute that should be used as the key when creating the tsibble from the .tsf file. If doesn't provide, a data frame will be returned instead of a tsibble
# index - The name of the time attribute that should be used as the index when creating the tsibble from the .tsf file. If doesn't provide, it will search for a valid index. When no valid index found, a data frame will be returned instead of a tsibble
# integer_conversion - Whether the final forecasts should be rounded or not
# significance - Initial significance used by the linearity test (alpha_0)
# seq_significance - Whether the significance used by the linearity test is reduced in each tree level
# significance_divider - If sequence significance is used, then in each tree level, the current significance will be divided by this value
# error_threshold - The minimum error reduction percentage between parent and child nodes to make a split
# stopping_criteria - The required stopping criteria: linearity test (lin_test), error reduction percentage (error_imp), linearity test and error reduction percentage (both)
# bagging_fraction - The percentage of instances that should be used to train each tree in the forest
# bagging_freq - The number of trees in the forest
# feature fraction - The percentage of features that should be used to train each tree in the forest
# random_significance - Whether a random significance should be considered to assess each node split (When set to TRUE, the "significance" parameter will be ignored)
# random_error_threshold - Whether a random error threshold should be considered to assess each node split (When set to TRUE, the "error_threshold" parameter will be ignored)
# random_tree_significance - Whether a random singificance should be considered for splitting per each tree (Each node split within the tree will consider the same significance level. When set to TRUE, the "significance" parameter will be ignored)
# random_significance_divider - Whether a random singificance divider should be considered for splitting per each tree (When set to TRUE, the "significance_divider" parameter will be ignored)
# random_tree_error_threshold - Whether a random error threshold should be considered for splitting per each tree (Each node split within the tree will consider the same error threshold. When set to TRUE, the "error_threshold" parameter will be ignored)
# num_leaves - The maximum number of leaves that a tree can have. This is 100000 by default and thus, actually the number of leaf nodes will be controlled by the stopping criteria, unless you specify a lower value for this parameter
# min_data_in_leaf - The minimum number of instances that should be there in each leaf node. This is 0 by default
# categorical_covariates - A vector containing the names of external categorical covariates. The .tsf file should contain series corresponding with each categorical covariate
# numerical_covariates - A vector containing the names of external numerical covariates. The .tsf file should contain series corresponding with each numerical covariate
# series_prefix - The prefix used to identify original time series in the .tsf file. This is only required when the models are trained with external covariates
# splitter - The splitter used in the names of time series in the .tsf file to separate the series type and number. This is only required when the models are trained with external covariates
do_setar_forest_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 1000, key = "series_name", index = "start_timestamp", integer_conversion = FALSE, significance = 0.05, seq_significance = TRUE, significance_divider = 2, error_threshold = 0.03, stopping_criteria = "both", bagging_fraction = 0.8, bagging_freq = 10, feature_fraction = 1, random_significance = FALSE, random_error_threshold = FALSE, random_tree_significance = FALSE, random_significance_divider = FALSE, random_tree_error_threshold = FALSE, num_leaves = 100000, min_data_in_leaf = 0, categorical_covariates = NULL, numerical_covariates = NULL, series_prefix = NULL, splitter = "_"){

  # Creating training and test sets
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon, categorical_covariates, numerical_covariates, series_prefix, splitter)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  cat_unique_vals <- loaded_data[[3]]
  seasonality <- loaded_data[[4]]

  result <- create_tree_input_matrix(training_set, lag, FALSE, test_set, categorical_covariates, numerical_covariates, cat_unique_vals)
  embedded_series <- result[[1]]
  full_final_lags <- result[[2]]

  all_tree_forecasts <- list()


  # Start timestamp
  start_time <- Sys.time()

  num_indexes <- round(nrow(embedded_series) * bagging_fraction)
  num_features <- round((ncol(embedded_series)-1) * feature_fraction)


  # Get forecasts from multiple setar trees as required
  for(bag_f in 1:bagging_freq){
    set.seed(bag_f)
    tree_indexes <- sort(sample(1:nrow(embedded_series), num_indexes, replace = FALSE))

    set.seed(bag_f)
    feature_indexes <- sort(sample(1:(ncol(embedded_series)-1), num_features, replace = FALSE))

    current_tree_data <- embedded_series[tree_indexes, c(1, (feature_indexes + 1))]

    if(random_tree_significance){
      set.seed(bag_f)
      significance <- sample(seq(0.01, 0.1, length.out = 10), 1)
      print(paste0("Chosen significance: ", significance))
    }

    if(random_significance_divider){
      set.seed(bag_f)
      significance_divider <- sample(2:10, 1)
      print(paste0("Chosen significance divider: ", significance_divider))
    }

    if(random_tree_error_threshold){
      set.seed(bag_f)
      error_threshold <- sample(seq(0.001, 0.05, length.out = 50), 1)
      print(paste0("Chosen error threshold: ", error_threshold))
    }

    # Execute individual SETAR trees
    all_tree_forecasts[[bag_f]] <- setar_tree_forecasting(current_tree_data, test_set, cat_unique_vals, lag, forecast_horizon, full_final_lags, feature_indexes, depth, significance, seq_significance, significance_divider, stopping_criteria, error_threshold, random_significance, random_error_threshold, num_leaves, min_data_in_leaf, categorical_covariates, numerical_covariates)
  }

  final_forecasts <- all_tree_forecasts[[1]]

  for(bag_f in 2:bagging_freq)
    final_forecasts <- final_forecasts + all_tree_forecasts[[bag_f]]

  final_forecasts <- final_forecasts/bagging_freq # Final forecasts are the average of forecasts given by all trees


  # Finish timestamp
  end_time <- Sys.time()

  if(integer_conversion)
    final_forecasts <- round(final_forecasts)

  file_name <- paste0(dataset_name, "_setarforest_bagging_freq_", bagging_freq)

  if(random_significance)
    file_name <- paste0(file_name, "_random_significance")

  if(random_error_threshold)
    file_name <- paste0(file_name, "_random_error_threshold")

  if(random_tree_significance)
    file_name <- paste0(file_name, "_random_tree_significance")

  if(random_tree_error_threshold)
    file_name <- paste0(file_name, "_random_tree_error_threshold")

  if(random_significance_divider)
    file_name <- paste0(file_name, "_random_significance_divider")

  dir.create(file.path(BASE_DIR, "results", "forecasts", "setar_forest", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  write.table(final_forecasts, file.path(BASE_DIR, "results", "forecasts", "setar_forest", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)

  # Execution time
  dir.create(file.path(BASE_DIR, "results", "execution_times", "setar_forest", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", "setar_forest", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)

  # Error calculations
  calculate_errors(final_forecasts, test_set$series, training_set$series, seasonality, file_name)
}



# Experiments

# Chaotic Logistic
do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE)
do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)

# Experiments with different forest sizes
do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.8, bagging_freq = 5, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.8, bagging_freq = 20, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, bagging_fraction = 0.8, bagging_freq = 50, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)


# Mackey-Glass
do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE)
do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)


# Tourism Quarterly
do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE)
do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)


# Tourism Monthly
do_setar_forest_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE)
do_setar_forest_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)


# M5
do_setar_forest_forecasting("m5_dataset.tsf", 10, 28, "m5", bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE)
do_setar_forest_forecasting("m5_dataset.tsf", 10, 28, "m5", bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("m5_dataset.tsf", 10, 28, "m5", bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)


# Kaggle Web Traffic
# Without covariates
do_setar_forest_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE)
do_setar_forest_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)

# With covariates
do_setar_forest_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", index = NULL, integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, categorical_covariates = "wday", series_prefix = "T")
do_setar_forest_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", index = NULL, integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE, categorical_covariates = "wday", series_prefix = "T")
do_setar_forest_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", index = NULL, integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE, categorical_covariates = "wday", series_prefix = "T")


# Rossmann
# Without covariates
do_setar_forest_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE)
do_setar_forest_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)

# With covariates
do_setar_forest_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", index = NULL, integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_setar_forest_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", index = NULL, integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_setar_forest_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", index = NULL, integer_conversion = TRUE, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")


# Favourita
# Without covariates
do_setar_forest_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE)
do_setar_forest_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE)
do_setar_forest_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE)

# With covariates
do_setar_forest_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, categorical_covariates = "wday", series_prefix = "T")
do_setar_forest_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_error_threshold = TRUE, categorical_covariates = "wday", series_prefix = "T")
do_setar_forest_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", index = NULL, bagging_fraction = 0.8, bagging_freq = 10, random_tree_significance = TRUE, random_significance_divider = TRUE, random_tree_error_threshold = TRUE, categorical_covariates = "wday", series_prefix = "T")
