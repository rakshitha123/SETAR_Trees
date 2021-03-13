BASE_DIR <- "C:/Projects/Trees/"
DATASET_DIR <- "C:/Projects/TSForecasting/"

source(file.path(DATASET_DIR, "utils", "data_loader.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "global_models.R", fsep = "/"))
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


do_global_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, method_name = "pooled_regression", key = "series_name", index = "start_timestamp", interger_conversion = F){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  forecasts <- start_forecasting(training_set, lag, forecast_horizon)
  
  if(interger_conversion)
    forecasts <- round(forecasts)
  
  file_name <- paste0(dataset_name, "_lag_", method_name)
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote=FALSE)

  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
}

do_global_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily")
do_global_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", index = NULL)
do_global_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", index = NULL)
do_global_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", interger_conversion = T)
