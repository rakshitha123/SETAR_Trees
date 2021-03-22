BASE_DIR <- "C:/Projects/Trees/"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


do_global_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, method_name = "pooled_regression", key = "series_name", index = "start_timestamp", interger_conversion = F){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  # Start timestamp
  start_time <- Sys.time()
  
  forecasts <- start_forecasting(training_set, lag, forecast_horizon, method_name)
  
  # Finish timestamp
  end_time <- Sys.time()
  
  if(interger_conversion)
    forecasts <- round(forecasts)
  
  file_name <- paste0(dataset_name, "_lag_", method_name)
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote=FALSE)

  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
}

do_global_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily")
do_global_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", index = NULL)
do_global_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", interger_conversion = T)
do_global_forecasting("tourism_quarterly_dataset.ts", 10, 8, "tourism_quarterly")

do_global_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", method_name = "catboost")
do_global_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", index = NULL, method_name = "catboost")
do_global_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", index = NULL, method_name = "catboost")
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", interger_conversion = T, method_name = "catboost")
do_global_forecasting("tourism_quarterly_dataset.ts", 10, 8, "tourism_quarterly", method_name = "catboost")

do_global_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", method_name = "lightgbm")
do_global_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", index = NULL, method_name = "lightgbm")
do_global_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", index = NULL, method_name = "lightgbm")
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", interger_conversion = T, method_name = "lightgbm")
do_global_forecasting("tourism_quarterly_dataset.ts", 10, 8, "tourism_quarterly", method_name = "lightgbm")

do_global_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", method_name = "xgboost")
do_global_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", index = NULL, method_name = "xgboost")
do_global_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", index = NULL, method_name = "xgboost")
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", interger_conversion = T, method_name = "xgboost")
do_global_forecasting("tourism_quarterly_dataset.ts", 10, 8, "tourism_quarterly", method_name = "xgboost")


