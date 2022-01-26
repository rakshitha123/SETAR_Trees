# Implementations of a set of univariate forecasting models
#
# Each function takes 2 parameters
# time_series - a ts object representing the time series that should be used with model training
# forecast_horizon - expected forecast horizon


# Calculate ets forecasts
get_ets_forecasts <- function(time_series, forecast_horizon){
  forecast(forecast:::ets(time_series), h = forecast_horizon)$mean
}


# Calculate auto.arima forecasts
get_arima_forecasts <- function(time_series, forecast_horizon){
  tryCatch({
    fit <- forecast:::auto.arima(time_series, lambda = 0)
  }, error = function(e) {
    tryCatch({
      fit <<- forecast:::auto.arima(time_series)
    }, error = function(e){
      fit <<- forecast:::auto.arima(time_series, seasonal = FALSE)
    })
  })
  
  forecast:::forecast(fit, h = forecast_horizon)$mean
}


# Calculate simple exponential smoothing forecasts
get_ses_forecasts <- function(time_series, forecast_horizon){
  forecast(forecast:::ses(time_series, h = forecast_horizon))$mean
}


# Calculate theta forecasts
get_theta_forecasts <-function(time_series, forecast_horizon){
  forecast:::thetaf(y = time_series, h = forecast_horizon)$mean
}
