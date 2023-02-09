BASE_DIR <- "SETAR_Trees"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


# A wrapper function to execute global forecasting models

# Parameters
# input_file_name - .tsf file name
# lag - The number of past lags that should be considered during training
# forecast_horizon - The expected forecast horizon
# dataset_name - Name of the dataset
# method_name - Name of the global model. Current supportive global model names: pooled_regression, regression_tree, ffnn, catboost, lightgbm, xgboost, rf, cubist
# key - The name of the attribute that should be used as the key when creating the tsibble from the .tsf file. If doesn't provide, a data frame will be returned instead of a tsibble
# index - The name of the time attribute that should be used as the index when creating the tsibble from the .tsf file. If doesn't provide, it will search for a valid index. When no valid index found, a data frame will be returned instead of a tsibble
# integer_conversion - Whether the final forecasts should be rounded or not
# scale - Whether the series should be normalized before training. When TRUE, mean normalization is applied to each series
# categorical_covariates - A vector containing the names of external categorical covariates. The .tsf file should contain series corresponding with each categorical covariate
# numerical_covariates - A vector containing the names of external numerical covariates. The .tsf file should contain series corresponding with each numerical covariate
# series_prefix - The prefix used to identify original time series in the .tsf file. This is only required when the models are trained with external covariates
# splitter - The splitter used in the names of time series in the .tsf file to separate the series type and number. This is only required when the models are trained with external covariates
do_global_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, method_name = "pooled_regression", key = "series_name", index = "start_timestamp", integer_conversion = F, scale = FALSE, categorical_covariates = NULL, numerical_covariates = NULL, series_prefix = NULL, splitter = "_"){
  
  # Creating training and test sets
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon, categorical_covariates, numerical_covariates, series_prefix, splitter)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[4]]
  
  # Start timestamp
  start_time <- Sys.time()
  
  # Forecasting
  forecasts <- start_forecasting(training_set, lag, forecast_horizon, method_name, scale, test_set, categorical_covariates, numerical_covariates)
  
  # Finish timestamp
  end_time <- Sys.time()
  
  if(integer_conversion)
    forecasts <- round(forecasts)
  
  file_name <- paste0(dataset_name, "_", method_name)
  
  dir.create(file.path(BASE_DIR, "results", "forecasts", "global_models", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", "global_models", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote=FALSE)

  # Error calculations
  calculate_errors(forecasts, test_set$series, training_set$series, seasonality, file_name)
  
  # Execution time
  dir.create(file.path(BASE_DIR, "results", "execution_times", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
}


# Experiments

# Pooled Regression
# Without covariates
do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", method_name = "pooled_regression", index = NULL)
do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", method_name = "pooled_regression", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", method_name = "pooled_regression", integer_conversion = T)
do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "pooled_regression")
do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "pooled_regression", integer_conversion = T)
do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "pooled_regression", index = NULL)
do_global_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", method_name = "pooled_regression")
do_global_forecasting("m5_dataset.tsf", 10, 28, "m5", method_name = "pooled_regression")


# With covariates
do_global_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", method_name = "pooled_regression", integer_conversion = T, index = NULL, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_global_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", method_name = "pooled_regression", index = NULL, integer_conversion = T, categorical_covariates = "wday", series_prefix = "T")
do_global_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", method_name = "pooled_regression", index = NULL, categorical_covariates = "wday", series_prefix = "T")


# Cubist
# Without covariates
do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", method_name = "cubist", index = NULL)
do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", method_name = "cubist", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", method_name = "cubist", integer_conversion = T)
do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "cubist")
do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "cubist", integer_conversion = T)
do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "cubist", index = NULL)
do_global_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", method_name = "cubist")
do_global_forecasting("m5_dataset.tsf", 10, 28, "m5", method_name = "cubist")


# With covariates
do_global_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", method_name = "cubist", integer_conversion = T, index = NULL, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_global_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", method_name = "cubist", index = NULL, integer_conversion = T, categorical_covariates = "wday", series_prefix = "T")
do_global_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", method_name = "cubist", index = NULL, categorical_covariates = "wday", series_prefix = "T")


# CatBoost
# Without covariates
do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "catboost")
do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "catboost")
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "catboost")
do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "catboost")
do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "catboost", integer_conversion = T)
do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "catboost", index = NULL)
do_global_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", method_name = "catboost")
do_global_forecasting("m5_dataset.tsf", 10, 28, "m5", method_name = "catboost")

# With covariates
do_global_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", method_name = "catboost", integer_conversion = T, index = NULL, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_global_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", method_name = "catboost", index = NULL, integer_conversion = T, categorical_covariates = "wday", series_prefix = "T")
do_global_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", method_name = "catboost", index = NULL, categorical_covariates = "wday", series_prefix = "T")


# Regression Tree
# Without covariates
do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "regression_tree")
do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "regression_tree")
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "regression_tree")
do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "regression_tree")
do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "regression_tree", integer_conversion = T)
do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "regression_tree", index = NULL)
do_global_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", method_name = "regression_tree")
do_global_forecasting("m5_dataset.tsf", 10, 28, "m5", method_name = "regression_tree")

# With covariates
do_global_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", method_name = "regression_tree", integer_conversion = T, index = NULL, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_global_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", method_name = "regression_tree", index = NULL, integer_conversion = T, categorical_covariates = "wday", series_prefix = "T")
do_global_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", method_name = "regression_tree", index = NULL, categorical_covariates = "wday", series_prefix = "T")


# LightGBM
# Without covariates
do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "lightgbm")
do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "lightgbm")
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "lightgbm")
do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "lightgbm")
do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "lightgbm", integer_conversion = T)
do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "lightgbm", index = NULL)
do_global_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", method_name = "lightgbm")
do_global_forecasting("m5_dataset.tsf", 10, 28, "m5", method_name = "lightgbm")

# With covariates
do_global_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", method_name = "lightgbm", integer_conversion = T, index = NULL, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_global_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", method_name = "lightgbm", index = NULL, integer_conversion = T, categorical_covariates = "wday", series_prefix = "T")
do_global_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", method_name = "lightgbm", index = NULL, categorical_covariates = "wday", series_prefix = "T")


# XGBoost
# Without covariates
do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "xgboost")
do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "xgboost")
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "xgboost")
do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "xgboost")
do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "xgboost", integer_conversion = T)
do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "xgboost", index = NULL)
do_global_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", method_name = "xgboost")
do_global_forecasting("m5_dataset.tsf", 10, 28, "m5", method_name = "xgboost")

# With covariates
do_global_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", method_name = "xgboost", integer_conversion = T, index = NULL, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_global_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", method_name = "xgboost", index = NULL, integer_conversion = T, categorical_covariates = "wday", series_prefix = "T")
do_global_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", method_name = "xgboost", index = NULL, categorical_covariates = "wday", series_prefix = "T")


# Random Forest
# Without covariates
do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "rf")
do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "rf")
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "rf")
do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "rf")
do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "rf", integer_conversion = T)
do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "rf", index = NULL)
do_global_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", method_name = "rf")
do_global_forecasting("m5_dataset.tsf", 10, 28, "m5", method_name = "rf")

# With covariates
do_global_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", method_name = "rf", integer_conversion = T, index = NULL, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_global_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", method_name = "rf", index = NULL, integer_conversion = T, categorical_covariates = "wday", series_prefix = "T")
do_global_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", method_name = "rf", index = NULL, categorical_covariates = "wday", series_prefix = "T")


# FFNN
# Without covariates
do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "ffnn")
do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "ffnn")
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "ffnn")
do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "ffnn")
do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "ffnn", integer_conversion = T)
do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "ffnn", index = NULL)
do_global_forecasting("tourism_monthly_dataset.tsf", 15, 24, "tourism_monthly", method_name = "ffnn")
do_global_forecasting("m5_dataset.tsf", 10, 28, "m5", method_name = "ffnn")

# With covariates
do_global_forecasting("rossmann_data_with_corvariates.tsf", 10, 48, "rossmann_with_cov", method_name = "ffnn", integer_conversion = T, index = NULL, categorical_covariates = c("Open", "Promo", "StateHoliday", "SchoolHoliday"), numerical_covariates = "Customers", series_prefix = "T")
do_global_forecasting("kaggle_1000_with_date_corvariates.tsf", 10, 59, "kaggle_daily_with_cov", method_name = "ffnn", index = NULL, integer_conversion = T, categorical_covariates = "wday", series_prefix = "T")
do_global_forecasting("favourita_1000_with_date_corvariates.tsf", 10, 16, "favourita_with_cov", method_name = "ffnn", index = NULL, categorical_covariates = "wday", series_prefix = "T")

