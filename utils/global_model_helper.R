# Creating embedded matrix and final lags to train the global models
create_input_matrix <- function(dataset, lag){
  embedded_series <- NULL
  final_lags <- NULL
  # series_means <- NULL
  
  for (i in 1:length(dataset)) {
    time_series <- as.numeric(unlist(dataset[i], use.names = FALSE))
    
    # mean <- mean(time_series)
    
    # if(mean != 0){
    #   series_means <- c(series_means, mean)
    #   time_series <- time_series / mean
    # }else
    #   series_means <- c(series_means, 1)
    
    # Embed the series
    embedded <- embed(time_series, lag + 1)
    
    if (!is.null(embedded_series)) {
      embedded_series <- as.matrix(embedded_series)
    }
    embedded_series <- rbind(embedded_series, embedded)
    
    # Creating the test set
    if (!is.null(final_lags)) {
      final_lags <- as.matrix(final_lags)
    }
    
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))
    final_lags <- rbind(final_lags, current_series_final_lags)
  }
  
  embedded_series <- as.data.frame(embedded_series)
  colnames(embedded_series)[1] <- "y"
  colnames(embedded_series)[2:(lag + 1)] <- paste("Lag", 1:lag, sep = "")
 
  final_lags <- as.data.frame(final_lags)
  colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep = "")
  
  list(embedded_series, final_lags)  #series_means
}


# Creating a formula to train a model
create_formula <- function(data){
  formula <- "y ~ "
  for(predictor in 2:ncol(data)){
    if(predictor != ncol(data)){
      formula <- paste0(formula, colnames(data)[predictor], " + ")
    }else{
      formula <- paste0(formula, colnames(data)[predictor])
    }
  }
  
  as.formula(paste(formula, "+ 0", sep=""))
}


# Creating training and test sets
create_train_test_sets <- function(input_file_name, key, index, forecast_horizon){
  loaded_data <- convert_ts_to_tsibble(file.path(DATASET_DIR, "ts_data", input_file_name, fsep = "/"), VALUE_COL_NAME, key, index)
  dataset <- loaded_data[[1]]
  frequency <- loaded_data[[2]]
  
  if(!is.null(frequency))
    seasonality <- SEASONALITY_MAP[[frequency]]
  else
    seasonality <- 1
  
  all_serie_names <- unique(dataset$series_name)
  
  training_set <- list()
  test_set <- matrix(0, nrow = length(all_serie_names), ncol = forecast_horizon)
  
  for(s in seq_along(all_serie_names)){
    series_data <- dataset[dataset$series_name == as.character(all_serie_names[s]), ]
    train_series_data <- series_data[1:(nrow(series_data) - forecast_horizon),][[VALUE_COL_NAME]]
    test_series_data <- series_data[(nrow(series_data) - forecast_horizon + 1):nrow(series_data),][[VALUE_COL_NAME]]
    
    training_set[[s]] <- train_series_data
    test_set[s,] <- test_series_data
  }
  
  list(training_set, data.frame(test_set), seasonality)
}


# Fitting a global regression model
fit_global_model <- function(fitting_data, test_data = NULL) {
  model <- glm(formula = create_formula(fitting_data), data = fitting_data)
  
  if(is.null(test_data))
    global_predictions  <- model$fitted.values
  else  
    global_predictions <- predict.glm(object = model, newdata = as.data.frame(test_data))
  
  list("predictions" = global_predictions, "model" = model)
}









