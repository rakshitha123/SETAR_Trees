# BASE_DIR <- "C:/Projects/SETAR_Trees/"
BASE_DIR <- "/home/rakshitha/Trees/"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


do_global_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, method_name = "pooled_regression", key = "series_name", index = "start_timestamp", integer_conversion = F, scale = FALSE){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  # Start timestamp
  start_time <- Sys.time()
  
  forecasts <- start_forecasting(training_set, lag, forecast_horizon, method_name, scale)
  
  # Finish timestamp
  end_time <- Sys.time()
  
  if(integer_conversion)
    forecasts <- round(forecasts)
  
  file_name <- paste0(dataset_name, "_lag_", lag, "_", method_name)
  
  if(scale)
    file_name <- paste0(file_name, "_with_scaling")
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", "global_models", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote=FALSE)

  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
}


# do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL)
# do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL)
# do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T)
# do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly")
# do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", integer_conversion = T)
# do_global_forecasting("walmart_store_sales_dataset.tsf", 10, 39, "walmart", index = NULL)
# do_global_forecasting("restaurant_visitors_dataset.tsf", 10, 39, "restaurant", index = NULL, integer_conversion = T)
# do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_10000.tsf", 10, 59, "kaggle_daily_10000", integer_conversion = T)
do_global_forecasting("favourita_sales_10000_dataset.tsf", 10, 16, "favourita_10000", index = NULL)


# do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "catboost")
# do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "catboost")
# do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "catboost")
# do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "catboost")
# do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "catboost", integer_conversion = T)
# do_global_forecasting("walmart_store_sales_dataset.tsf", 10, 39, "walmart", method_name = "catboost", index = NULL)
# do_global_forecasting("restaurant_visitors_dataset.tsf", 10, 39, "restaurant", method_name = "catboost", index = NULL, integer_conversion = T)
# do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "catboost", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_10000.tsf", 10, 59, "kaggle_daily_10000", integer_conversion = T, method_name = "catboost")
do_global_forecasting("favourita_sales_10000_dataset.tsf", 10, 16, "favourita_10000", index = NULL, method_name = "catboost")



# do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "lightgbm")
# do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "lightgbm")
# do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "lightgbm")
# do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "lightgbm")
# do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "lightgbm", integer_conversion = T)
# do_global_forecasting("walmart_store_sales_dataset.tsf", 10, 39, "walmart", method_name = "lightgbm", index = NULL)
# do_global_forecasting("restaurant_visitors_dataset.tsf", 10, 39, "restaurant", method_name = "lightgbm", index = NULL, integer_conversion = T)
# do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "lightgbm", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_10000.tsf", 10, 59, "kaggle_daily_10000", integer_conversion = T, method_name = "lightgbm")
do_global_forecasting("favourita_sales_10000_dataset.tsf", 10, 16, "favourita_10000", index = NULL, method_name = "lightgbm")


# do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "xgboost")
# do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "xgboost")
# do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "xgboost")
# do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "xgboost")
# do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "xgboost", integer_conversion = T)
# do_global_forecasting("walmart_store_sales_dataset.tsf", 10, 39, "walmart", method_name = "xgboost", index = NULL)
# do_global_forecasting("restaurant_visitors_dataset.tsf", 10, 39, "restaurant", method_name = "xgboost", index = NULL, integer_conversion = T)
# do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "xgboost", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_10000.tsf", 10, 59, "kaggle_daily_10000", integer_conversion = T, method_name = "xgboost")
do_global_forecasting("favourita_sales_10000_dataset.tsf", 10, 16, "favourita_10000", index = NULL, method_name = "xgboost")



# do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "ffnn")
# do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "ffnn")
# do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "ffnn")
# do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "ffnn")
# do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "ffnn", integer_conversion = T)
# do_global_forecasting("walmart_store_sales_dataset.tsf", 10, 39, "walmart", method_name = "ffnn", index = NULL)
# do_global_forecasting("restaurant_visitors_dataset.tsf", 10, 39, "restaurant", method_name = "ffnn", index = NULL, integer_conversion = T)
# do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "ffnn", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_10000.tsf", 10, 59, "kaggle_daily_10000", integer_conversion = T, method_name = "ffnn")
do_global_forecasting("favourita_sales_10000_dataset.tsf", 10, 16, "favourita_10000", index = NULL, method_name = "ffnn")



# do_global_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", index = NULL, method_name = "rf")
# do_global_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", index = NULL, method_name = "rf")
# do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", integer_conversion = T, method_name = "rf")
# do_global_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", method_name = "rf")
# do_global_forecasting("rossmann_dataset_without_missing_values.tsf", 10, 48, "rossmann", method_name = "rf", integer_conversion = T)
# do_global_forecasting("walmart_store_sales_dataset.tsf", 10, 39, "walmart", method_name = "rf", index = NULL)
# do_global_forecasting("restaurant_visitors_dataset.tsf", 10, 39, "restaurant", method_name = "rf", index = NULL, integer_conversion = T)
# do_global_forecasting("favourita_sales_1000_dataset.tsf", 10, 16, "favourita", method_name = "rf", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_10000.tsf", 10, 59, "kaggle_daily_10000", integer_conversion = T, method_name = "rf")
do_global_forecasting("favourita_sales_10000_dataset.tsf", 10, 16, "favourita_10000", index = NULL, method_name = "rf")

