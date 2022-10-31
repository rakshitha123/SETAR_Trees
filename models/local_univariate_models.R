# Implementations of a set of univariate forecasting models
#
# ETS,ARIMA, SES and Theta models take 2 parameters
# time_series - a ts object representing the time series that should be used with model training
# forecast_horizon - expected forecast horizon
#
# SETAR and STAR models also get the above 2 parameters. Additionally, they take the embedding dimension/lag (deafult: 10) as a parameter.


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
get_theta_forecasts <- function(time_series, forecast_horizon){
  forecast:::thetaf(y = time_series, h = forecast_horizon)$mean
}


# Calculate forecasts from a 2-regime SETAR model
get_setar_forecasts <- function(time_series, forecast_horizon, lag = 10){
  fit <- NULL
  forecasts <- NULL
  
  for(l in lag:1){
    tryCatch({
      fit <- tsDyn:::setar(time_series, m = l)
      break
    }, error = function(e) {}
    )
  }
  
  if(!is.null(fit)){
    test <- tail(time_series, l)
    
    for(h in 1:forecast_horizon){
      f <- predict(fit, test)
      forecasts <- c(forecasts, f)
      test <- test[-1]
      test <- c(test, f)
    }
  }else{ # If fitting a SETAR model fails, then fit a regression model
    embedded_series <- as.data.frame(embed(time_series, lag + 1))
    colnames(embedded_series)[1] <- "y"
    colnames(embedded_series)[2:(lag+1)] <- paste("Lag", 1:lag, sep = "")
    
    fit <- glm(formula = create_formula(embedded_series), data = embedded_series)
    
    test <- as.data.frame(t(as.matrix(rev(tail(time_series, lag)))))
    colnames(test)[1:lag] <- paste("Lag", 1:lag, sep = "")
    
    for(h in 1:forecast_horizon){
      f <- predict.glm(fit, test)
      forecasts <- c(forecasts, f)
      test <- test[-lag]
      test <- cbind(f, test) 
      colnames(test)[1:lag] <- paste("Lag", 1:lag, sep="")
    }
  }
 
  forecasts
}


# Calculate forecasts from a 2-regime STAR model
get_star_forecasts <- function(time_series, forecast_horizon, lag = 10){
  
  fit <- NULL
  forecasts <- NULL
  
  for(l in lag:1){
    tryCatch({
      fit <- tsDyn:::star(time_series, m = l, noRegimes = 2)
      break
    }, error = function(e) {}
    )
  }
  
  if(!is.null(fit)){
    test <- tail(time_series, l)
    
    for(h in 1:forecast_horizon){
      f <- predict(fit, test)
      forecasts <- c(forecasts, f)
      test <- test[-1]
      test <- c(test, f)
    }
  }else{ # If fitting a STAR model fails, then fit a regression model
    embedded_series <- as.data.frame(embed(time_series, lag + 1))
    colnames(embedded_series)[1] <- "y"
    colnames(embedded_series)[2:(lag+1)] <- paste("Lag", 1:lag, sep = "")
    
    fit <- glm(formula = create_formula(embedded_series), data = embedded_series)
    
    test <- as.data.frame(t(as.matrix(rev(tail(time_series, lag)))))
    colnames(test)[1:lag] <- paste("Lag", 1:lag, sep = "")
    
    for(h in 1:forecast_horizon){
      f <- predict.glm(fit, test)
      forecasts <- c(forecasts, f)
      test <- test[-lag]
      test <- cbind(f, test) 
      colnames(test)[1:lag] <- paste("Lag", 1:lag, sep="")
    }
  }
  
  forecasts
}
