# Implementation of a pooled regression model

library(glmnet)
library(catboost)
library(nnet)
library(lightgbm)
library(xgboost)

set.seed(1)


# Forecasting with different lags
start_forecasting <- function(dataset, lag, forecast_horizon, method = "pooled_regression", scale = FALSE){
  # Creating embedded matrix (for model training) and test set
  result <- create_input_matrix(dataset, lag, scale)
  
  embedded_series <- result[[1]] # Embedded matrix
  final_lags <- result[[2]] # Test set
  series_means <- result[[3]] # Mean value of each series
  
  fit_model(embedded_series, lag, final_lags, forecast_horizon, series_means, method, scale)
}


# Fit and forecast from a global model
fit_model <- function(fitting_data, lag, final_lags, forecast_horizon, series_means, method = "pooled_regression", scale = FALSE) {
  # Create the formula
  formula <- "y ~ "
  for(predictor in 2:ncol(fitting_data)){
    if(predictor != ncol(fitting_data)){
      formula <- paste0(formula, colnames(fitting_data)[predictor], " + ")
    }else{
      formula <- paste0(formula, colnames(fitting_data)[predictor])
    }
  }
  
  formula <- paste(formula, "+ 0", sep="")
  formula <- as.formula(formula)
  
  # Fit the pooled regression model
  if(method == "pooled_regression")
    model <- glm(formula = formula, data = fitting_data)
  else if(method == "catboost"){
    train_pool <- catboost.load_pool(data = as.matrix(fitting_data[-1]), label = as.matrix(fitting_data[,1]))
    model <- catboost.train(train_pool)
  }else if(method == "ffnn"){
    # Define parameters grid to tune hyperparameters
    parameters_df = expand.grid(size = seq(10, 60, 10), decay = seq(0.01, 0.1, 0.01))
    lowest_error_list = list()
    
    # Hyperparameter tuning using grid search
    for (row in 1:nrow(parameters_df)){
      
      nnetcv <- nnet(formula = formula, data = fitting_data, size = parameters_df$size[row], decay = parameters_df$decay[row], linout = FALSE)
      
      lowest_error <- data.frame("error" = mean(abs(nnetcv$residuals[,1])))
      lowest_error_list[[row]] <- lowest_error
    }
    
    lowest_error_df <- do.call(rbind, lowest_error_list)
    grid_search_output <- cbind(lowest_error_df, parameters_df)
    optimised_parameters <- (grid_search_output[grid_search_output$error == min(grid_search_output$error),])[1,]
    
    print(optimised_parameters)
   
    model <- nnet(formula = formula, data = fitting_data, size = optimised_parameters$size, decay = optimised_parameters$decay, linout = FALSE)
  }else if(method == "lightgbm" | method == "rf"){
    # Define parameters grid to tune hyperparameters
    parameters_df = expand.grid(min_data_in_leaf = seq(50, 200, 50), learning_rate = seq(0.01, 0.1, 0.01))
    lowest_error_list = list()
    
    # Hyperparameter tuning using grid search
    for (row in 1:nrow(parameters_df)){
      
      if(method == "lightgbm"){
        lgcv <- lightgbm(data = as.matrix(fitting_data[-1]), 
                         label = as.matrix(fitting_data[,1]), 
                         params = list(objective = "regression",
                                       metric = "rmse",
                                       boosting_type = "gbdt",
                                       learning_rate = parameters_df$learning_rate[row],
                                       min_data_in_leaf = parameters_df$min_data_in_leaf[row],
                                       early_stopping_rounds = 30,
                                       n_estimators = 400))
      }else{
        lgcv <- lightgbm(data = as.matrix(fitting_data[-1]), 
                         label = as.matrix(fitting_data[,1]), 
                         params = list(objective = "regression",
                                       metric = "rmse",
                                       boosting_type = "rf",
                                       learning_rate = parameters_df$learning_rate[row],
                                       min_data_in_leaf = parameters_df$min_data_in_leaf[row],
                                       bagging_freq = 1,
                                       bagging_fraction = 0.5,
                                       early_stopping_rounds = 30,
                                       n_estimators = 400))
      }
        
      
      lowest_error <- data.frame("min_error" = min(unlist(lgcv$record_evals$train$rmse$eval)))
      lowest_error_list[[row]] <- lowest_error
    }
    
    lowest_error_df <- do.call(rbind, lowest_error_list)
    grid_search_output <- cbind(lowest_error_df, parameters_df)
    optimised_parameters <- (grid_search_output[grid_search_output$min_error == min(grid_search_output$min_error),])[1,]
    
    print(optimised_parameters)
    
    if(method == "lightgbm"){
      model <- lightgbm(data = as.matrix(fitting_data[-1]), 
                        label = as.matrix(fitting_data[,1]), 
                        params = list(objective = "regression",
                                      metric = "rmse",
                                      boosting_type = "gbdt",
                                      learning_rate = optimised_parameters$learning_rate,
                                      min_data_in_leaf = optimised_parameters$min_data_in_leaf,
                                      early_stopping_rounds = 30,
                                      n_estimators = 400)) 
    }else{
      model <- lightgbm(data = as.matrix(fitting_data[-1]), 
                        label = as.matrix(fitting_data[,1]), 
                        params = list(objective = "regression",
                                      metric = "rmse",
                                      boosting_type = "rf",
                                      learning_rate = optimised_parameters$learning_rate,
                                      min_data_in_leaf = optimised_parameters$min_data_in_leaf,
                                      bagging_freq = 1,
                                      bagging_fraction = 0.5,
                                      early_stopping_rounds = 30,
                                      n_estimators = 400)) 
    }
  }else if(method == "xgboost"){
    # Define parameters grid to tune hyperparameters
    parameters_df = expand.grid(max_depth = seq(3, 10, 1), eta = seq(0.1, 0.5, 0.1))
    lowest_error_list = list()
    
    # Hyperparameter tuning using grid search
    for (row in 1:nrow(parameters_df)){
      xgcv <- xgboost:::xgboost(data = as.matrix(fitting_data[-1]), 
                                label = as.matrix(fitting_data[,1]), 
                                booster = "gbtree",
                                objective = "reg:squarederror",
                                eval_metric = "rmse",
                                max_depth = parameters_df$max_depth[row],
                                eta = parameters_df$eta[row],
                                nrounds = 400,
                                early_stopping_rounds = 30,
                                print_every_n = 10)
      
      lowest_error <- data.frame("min_error" = min(xgcv$evaluation_log$train_rmse))
      lowest_error_list[[row]] <- lowest_error
    }
    
    lowest_error_df <- do.call(rbind, lowest_error_list)
    grid_search_output <- cbind(lowest_error_df, parameters_df)
    optimised_parameters <- (grid_search_output[grid_search_output$min_error == min(grid_search_output$min_error),])[1,]
    
    print(optimised_parameters)
    
    # Train a XGBoost model
    model <- xgboost:::xgboost(data = as.matrix(fitting_data[-1]), 
                               label = as.matrix(fitting_data[,1]), 
                               booster = "gbtree",
                               objective = "reg:squarederror",
                               eval_metric = "rmse",
                               max_depth = optimised_parameters$max_depth,
                               eta = optimised_parameters$eta,
                               nround = 400,
                               early_stopping_rounds = 30)
  }
  
  # Do forecasting
  forec_recursive(lag, model, final_lags, forecast_horizon, series_means, method, scale)
}


# Recursive forecasting of the series until a given horizon
forec_recursive <- function(lag, model, final_lags, forecast_horizon, series_means, method = "pooled_regression", scale = FALSE) {
  
  # This will store the predictions corresponding with each horizon
  predictions <- NULL
  
  for (i in 1:forecast_horizon){
    # Get predictions for the current horizon
    if(method == "pooled_regression")
      new_predictions <- predict.glm(object = model, newdata = as.data.frame(final_lags)) 
    else if(method == "catboost"){
      catboost_final_lags <- catboost.load_pool(final_lags)
      new_predictions <- catboost.predict(model, catboost_final_lags)
    }else if(method == "ffnn")
      new_predictions <- predict(model, as.data.frame(final_lags))
    else if(method == "lightgbm" | method == "rf")
      new_predictions <- predict(model, as.matrix(final_lags))
    else if(method == "xgboost")
      new_predictions <- predict(model, data.matrix(final_lags))
    
    # Adding the current forecasts to the final predictions matrix
    predictions <- cbind(predictions, new_predictions)
    
    # Updating the test set for the next horizon
    if(i < forecast_horizon){
      final_lags <- final_lags[-lag]
      final_lags <- cbind(new_predictions, final_lags)
      colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
      final_lags <- as.data.frame(final_lags)
    }
  }
  
  if(scale)
     predictions <- predictions * as.vector(series_means)
  
  predictions
}
