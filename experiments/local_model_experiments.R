# BASE_DIR <- "C:/Projects/SETAR_Trees/"
BASE_DIR <- "/home/rakshitha/Trees/"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


do_local_forecasting <- function(input_file_name, forecast_horizon, dataset_name, method, key = "series_name", index = "start_timestamp", integer_conversion = F){
  
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
    
    current_forecasts <- eval(parse(text = paste0("get_", method, "_forecasts(series, forecast_horizon)")))
    
    if(integer_conversion)
      current_forecasts <- round(current_forecasts)
    
    forecasts[i,] <- current_forecasts
  }
  
  end_time <- Sys.time()
 
  file_name <- paste0(dataset_name, "_", method)
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", "local_models", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
  
  # Error calculations
  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)  
}


# Experiments

# Chaotic Logistic
# do_local_forecasting("chaotic_logistic_dataset.tsf", 8, "chaotic_logistic", "ets", index = NULL)
# do_local_forecasting("chaotic_logistic_dataset.tsf", 8, "chaotic_logistic", "arima", index = NULL)

# Mackey Glass
# do_local_forecasting("mackey_glass_dataset.tsf", 8, "mackey_glass", "ets", index = NULL)
# do_local_forecasting("mackey_glass_dataset.tsf", 8, "mackey_glass", "arima", index = NULL)

# Kaggle Daily
# do_local_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 59, "kaggle_daily", "ets", integer_conversion = TRUE)
# do_local_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 59, "kaggle_daily", "arima", integer_conversion = TRUE)
# do_local_forecasting("kaggle_web_traffic_dataset_10000.tsf", 59, "kaggle_daily_10000", "ets", integer_conversion = TRUE)
# do_local_forecasting("kaggle_web_traffic_dataset_10000.tsf", 59, "kaggle_daily_10000", "arima", integer_conversion = TRUE)

# Tourism Quarterly
# do_local_forecasting("tourism_quarterly_dataset.tsf", 8, "tourism_quarterly", "ets")
# do_local_forecasting("tourism_quarterly_dataset.tsf", 8, "tourism_quarterly", "arima")

# Rossmann
# do_local_forecasting("rossmann_dataset_without_missing_values.tsf", 48, "rossmann", "ets", integer_conversion = TRUE)
# do_local_forecasting("rossmann_dataset_without_missing_values.tsf", 48, "rossmann", "arima", integer_conversion = TRUE)

# Walmart Store Sales
# do_local_forecasting("walmart_store_sales_dataset.tsf", 39, "walmart", "ets", index = NULL)
# do_local_forecasting("walmart_store_sales_dataset.tsf", 39, "walmart", "arima", index = NULL)

# Restaurant Visitors
# do_local_forecasting("restaurant_visitors_dataset.tsf", 39, "restaurant", "ets", index = NULL, integer_conversion = T)
# do_local_forecasting("restaurant_visitors_dataset.tsf", 39, "restaurant", "arima", index = NULL, integer_conversion = T)

do_local_forecasting("restaurant_visitors_60_dataset.tsf", 39, "restaurant_60", "ets", index = NULL, integer_conversion = T)
do_local_forecasting("restaurant_visitors_60_dataset.tsf", 39, "restaurant_60", "arima", index = NULL, integer_conversion = T)

# Favourita Sales
# do_local_forecasting("favourita_sales_1000_dataset.tsf", 16, "favourita", "ets", index = NULL)
# do_local_forecasting("favourita_sales_1000_dataset.tsf", 16, "favourita", "arima", index = NULL)
# do_local_forecasting("favourita_sales_10000_dataset.tsf", 16, "favourita_10000", "ets", index = NULL)
# do_local_forecasting("favourita_sales_10000_dataset.tsf", 16, "favourita_10000", "arima", index = NULL)

