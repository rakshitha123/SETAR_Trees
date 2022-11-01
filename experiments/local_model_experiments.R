BASE_DIR <- "SETAR_Trees"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


# A wrapper function to execute local models

# Parameters
# input_file_name - .tsf file name
# forecast_horizon - The expected forecast horizon
# dataset_name - Name of the dataset
# method - Name of the local model. Current supportive local model names: ets, arima, ses, theta, setar and star
# key - The name of the attribute that should be used as the key when creating the tsibble from the .tsf file. If doesn't provide, a data frame will be returned instead of a tsibble
# index - The name of the time attribute that should be used as the index when creating the tsibble from the .tsf file. If doesn't provide, it will search for a valid index. When no valid index found, a data frame will be returned instead of a tsibble
# integer_conversion - Whether the final forecasts should be rounded or not
do_local_forecasting <- function(input_file_name, forecast_horizon, dataset_name, method, key = "series_name", index = "start_timestamp", integer_conversion = F){
  
  # Creating training and test sets
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]$series
  test_set <- loaded_data[[2]]$series
  seasonality <- loaded_data[[4]]
  
  forecasts <- matrix(NA, nrow = length(training_set), ncol = forecast_horizon)
  
  # Start timestamp
  start_time <- Sys.time()
  
  for(i in 1:length(training_set)){
    print(i)
    series <- forecast:::msts(training_set[[i]], seasonal.periods = seasonality)
    
    # Forecasting
    current_forecasts <- eval(parse(text = paste0("get_", method, "_forecasts(series, forecast_horizon)")))
    
    if(integer_conversion)
      current_forecasts <- round(current_forecasts)
    
    forecasts[i,] <- current_forecasts
  }
  
  end_time <- Sys.time()
 
  file_name <- paste0(dataset_name, "_", method)
  
  dir.create(file.path(BASE_DIR, "results", "forecasts", "local_models", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", "local_models", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Execution time
  dir.create(file.path(BASE_DIR, "results", "execution_times", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
  
  # Error calculations
  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)  
}


# Experiments

# Chaotic Logistic
do_local_forecasting("chaotic_logistic_dataset.tsf", 8, "chaotic_logistic", "ets", index = NULL)
do_local_forecasting("chaotic_logistic_dataset.tsf", 8, "chaotic_logistic", "arima", index = NULL)
do_local_forecasting("chaotic_logistic_dataset.tsf", 8, "chaotic_logistic", "setar", index = NULL)
do_local_forecasting("chaotic_logistic_dataset.tsf", 8, "chaotic_logistic", "star", index = NULL)

# Mackey-Glass
do_local_forecasting("mackey_glass_dataset.tsf", 8, "mackey_glass", "ets", index = NULL)
do_local_forecasting("mackey_glass_dataset.tsf", 8, "mackey_glass", "arima", index = NULL)
do_local_forecasting("mackey_glass_dataset.tsf", 8, "mackey_glass", "setar", index = NULL)
do_local_forecasting("mackey_glass_dataset.tsf", 8, "mackey_glass", "star", index = NULL)

# Kaggle Web Traffic
do_local_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 59, "kaggle_daily", "ets", integer_conversion = TRUE)
do_local_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 59, "kaggle_daily", "arima", integer_conversion = TRUE)
do_local_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 59, "kaggle_daily", "setar", integer_conversion = TRUE)
do_local_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 59, "kaggle_daily", "star", integer_conversion = TRUE)

# Tourism Quarterly
do_local_forecasting("tourism_quarterly_dataset.tsf", 8, "tourism_quarterly", "ets")
do_local_forecasting("tourism_quarterly_dataset.tsf", 8, "tourism_quarterly", "arima")
do_local_forecasting("tourism_quarterly_dataset.tsf", 8, "tourism_quarterly", "setar")
do_local_forecasting("tourism_quarterly_dataset.tsf", 8, "tourism_quarterly", "star")

# Rossmann
do_local_forecasting("rossmann_dataset_without_missing_values.tsf", 48, "rossmann", "ets", integer_conversion = TRUE)
do_local_forecasting("rossmann_dataset_without_missing_values.tsf", 48, "rossmann", "arima", integer_conversion = TRUE)
do_local_forecasting("rossmann_dataset_without_missing_values.tsf", 48, "rossmann", "setar", integer_conversion = TRUE)
do_local_forecasting("rossmann_dataset_without_missing_values.tsf", 48, "rossmann", "star", integer_conversion = TRUE)

# Favourita
do_local_forecasting("favourita_sales_1000_dataset.tsf", 16, "favourita", "ets", index = NULL)
do_local_forecasting("favourita_sales_1000_dataset.tsf", 16, "favourita", "arima", index = NULL)
do_local_forecasting("favourita_sales_1000_dataset.tsf", 16, "favourita", "setar", index = NULL)
do_local_forecasting("favourita_sales_1000_dataset.tsf", 16, "favourita", "star", index = NULL)

# Tourism Monthly
do_local_forecasting("tourism_monthly_dataset.tsf", 24, "tourism_monthly", "ets")
do_local_forecasting("tourism_monthly_dataset.tsf", 24, "tourism_monthly", "arima")
do_local_forecasting("tourism_monthly_dataset.tsf", 24, "tourism_monthly", "setar")
do_local_forecasting("tourism_monthly_dataset.tsf", 24, "tourism_monthly", "star")

# M5
do_local_forecasting("m5_dataset.tsf", 28, "m5", "ets")
do_local_forecasting("m5_dataset.tsf", 28, "m5", "arima")
do_local_forecasting("m5_dataset.tsf", 28, "m5", "setar")
do_local_forecasting("m5_dataset.tsf", 28, "m5", "star")
