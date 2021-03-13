BASE_DIR <- "C:/Projects/Trees/"
DATASET_DIR <- "C:/Projects/TSForecasting/"

source(file.path(DATASET_DIR, "utils", "data_loader.R", fsep = "/"))
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


# For testing
# input_file_name <- "nn5_daily_dataset_without_missing_values.ts"
# lag <- 10
# key <- "series_name"
# index <- "start_timestamp"
# forecast_horizon <- 56
# dataset_name = "nn5_daily"
# integer_conversion = F
# depth = 2

# input_file_name <- "chaotic_logistic_dataset.ts"
# lag <- 10
# key <- "series_name"
# index <- NULL
# forecast_horizon <- 8
# dataset_name = "chaotic_logistic"
# integer_conversion = F
# depth = 4
# optimizer = "BFGS"
# significance = 0.05
# validation_size = 50


get_leaf_instances <- function(original_instance, gamma_vals, thresholds, th_lags){
  leaf_instances <- NULL
  instances<- list(original_instance)
  
  for(g in 1:length(gamma_vals)){
    l_gammas <- gamma_vals[[g]]
    l_thresholds <- thresholds[[g]]
    l_th_lags <- th_lags[[g]]
    
    next_level <- list()
    
    for(i in 1:length(instances)){
      len <- length(next_level)
      left_instance <- instances[[i]] * G(instances[[i]][,l_th_lags[i]], l_gammas[i], l_thresholds[i])
      next_level[[len + 1]] <- left_instance
      
      len <- length(next_level)
      right_instance <- instances[[i]] * (1 - G(instances[[i]][,l_th_lags[i]], l_gammas[i], l_thresholds[i]))
      next_level[[len + 1]] <- right_instance
    }
   
    instances <- next_level
  }
  
  instances
}


# Logistic transition function
# z: variable
# gamma: smoothing parameter
# th: threshold value
G <- function(z, gamma, th) {
  plogis(z, th, 1/gamma)
}


fit_global_model <- function(fitting_data, test_data = NULL) {
  model <- glm(formula = create_formula(fitting_data), data = fitting_data)
  
  if(is.null(test_data))
    global_predictions  <- model$fitted.values
  else  
    global_predictions <- predict.glm(object = model, newdata = as.data.frame(test_data))
  
  list("predictions" = global_predictions, "model" = model)
}


create_split <- function(data, gamma, th, th_lag){
  left_node <- data* G(data[-1][,th_lag], gamma, th)
  right_node <- data* (1 - G(data[-1][,th_lag], gamma, th))
  list("left_node" = left_node, "right_node" = right_node)
}


do_lstar_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 2, key = "series_name", index = "start_timestamp", integer_conversion = F, significance = 0.05, optimizer = "BFGS", validation_size = NULL){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  result <- create_input_matrix(training_set, lag)
  embedded_series <- result[[1]]
  final_lags <- result[[2]]
  # series_means <- result[[3]]
  
  if(is.null(validation_size)){
    initial_predictions <- fit_global_model(embedded_series)$predictions
    initial_error <- sum((embedded_series$y - initial_predictions)^2)
  }else{
    train_indexes <- 1:(nrow(embedded_series) - validation_size)
    test_indexes <- (nrow(embedded_series) - validation_size + 1):nrow(embedded_series)
    
    initial_predictions <- fit_global_model(embedded_series[train_indexes,], embedded_series[test_indexes,-1])$predictions
    initial_error <- sum((embedded_series[test_indexes, "y"] - initial_predictions)^2)
  }
  
 
  # Set list of defaults:
  start.con<-list(
    nTh = 3, 
    nGamma = 5, # number of gamma values that will be used for testing
    gamma_int = c(1,100))
  
  
  gammas <- seq(start.con$gamma_int[1], start.con$gamma_int[2], length.out=start.con$nGamma) # Divide the gamma interval 
  
  
  # Sum of squares function
  # p: vector of parameters
  SS <- function(p, train_data, optimal_lag, test_data = NULL) {
    gamma <- p[1] # Extract parms from vector p
    th <- p[2] 
   
    if(is.null(test_data)){
      pred_1 <- fit_global_model(train_data * G(train_data[-1][,optimal_lag], gamma, th))$predictions
      pred_2 <- fit_global_model(train_data * (1 - G(train_data[-1][,optimal_lag], gamma, th)))$predictions
      final_preds <- as.numeric(pred_1 + pred_2)
      residuals <- train_data$y - final_preds
    }else{
      pred_1 <- fit_global_model(train_data * G(train_data[-1][,optimal_lag], gamma, th), test_data[-1] * G(test_data[-1][,optimal_lag], gamma, th))$predictions
      pred_2 <- fit_global_model(train_data * (1 - G(train_data[-1][,optimal_lag], gamma, th)), test_data[-1] * (1 - G(test_data[-1][,optimal_lag], gamma, th)))$predictions
      final_preds <- as.numeric(pred_1 + pred_2)
      residuals <- test_data$y - final_preds
    }
   
    sum(residuals ^ 2) # sum of squares of residuals
  }
  
  
  tree <- list()
  thresholds <- list()
  th_lags <- list()
  gamma_vals <- list()
  level_errors <- NULL
  
  node_data <- list(embedded_series)
    
  for(d in 1:depth){ 
    print(paste0("Depth: ", d))
    level_gamma_vals <- NULL
    level_thresholds <- NULL
    level_th_lags <- NULL
    level_nodes <- list()
    
    for(n in 1:length(node_data)){
      best_cost <- Inf
      
      for(lg in 1:lag){
        print(lg)
        ths <- seq(min(node_data[[n]][,lg+1]), max(node_data[[n]][,lg+1]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the data
        IDS <- as.matrix(expand.grid(gammas, ths) ) 
        
        for(ids in 1:nrow(IDS)){
          if(is.null(validation_size)){
            pred_l <- fit_global_model(node_data[[n]]* G(node_data[[n]][,lg+1], IDS[ids,1], IDS[ids,2]))$predictions
            pred_r <- fit_global_model(node_data[[n]] * (1 - G(node_data[[n]][,lg+1], IDS[ids,1], IDS[ids,2])))$predictions
            current_final_preds <- as.numeric(pred_l + pred_r)
            current_residuals <- node_data[[n]]$y - current_final_preds
          }else{
            pred_l <- fit_global_model(node_data[[n]][train_indexes,]* G(node_data[[n]][train_indexes,lg+1], IDS[ids,1], IDS[ids,2]), node_data[[n]][test_indexes, -1]* G(node_data[[n]][test_indexes,lg+1], IDS[ids,1], IDS[ids,2]))$predictions
            pred_r <- fit_global_model(node_data[[n]][train_indexes,] * (1 - G(node_data[[n]][train_indexes,lg+1], IDS[ids,1], IDS[ids,2])), node_data[[n]][test_indexes, -1] * (1 - G(node_data[[n]][test_indexes,lg+1], IDS[ids,1], IDS[ids,2])))$predictions
            current_final_preds <- as.numeric(pred_l + pred_r)
            current_residuals <- node_data[[n]][test_indexes, "y"] - current_final_preds
          }
          
          
          cost <-  sum(current_residuals ^ 2) # sum of squares of residuals
          
          if(cost <= best_cost) { # find gamma and th which minimize the squared errors
            best_cost <- cost;
            gamma <- IDS[ids,1]
            th <- IDS[ids,2]
            th_lag <- lg
          }
        }
      }
      
      print(gamma)
      print(th)
      print(th_lag)
      
      p <- c(gamma, th) # pack parameters in one vector   
      
      if(is.null(validation_size))
        res <- optim(p, SS, method = optimizer, lower = c(0, -Inf), train_data = node_data[[n]], test_data = NULL, optimal_lag = th_lag)
      else
        res <- optim(p, SS, method = optimizer, lower = c(0, -Inf), train_data = node_data[[n]][train_indexes,], test_data = node_data[[n]][test_indexes,] , optimal_lag = th_lag)
      
      
      if(res$convergence!=0){
        if(res$convergence==1) 
          print(paste0("Convergence problem code 1. You might want to increase maximum number of iterations by setting control=list(maxit=1000)"))
        else 
          print(paste0("Convergence problem. Convergence code: ", res$convergence))
      }else 
        print(paste0("Optimization algorithm converged"))
      
      best_gamma <- res$par[1]
      best_th <- res$par[2]
      best_th_lag <- th_lag 
      
      level_gamma_vals <- c(level_gamma_vals, best_gamma)
      level_thresholds <- c(level_thresholds, best_th)
      level_th_lags <- c(level_th_lags, best_th_lag)
      
      splited_nodes <- create_split(node_data[[n]], best_gamma, best_th, best_th_lag)
      
      for(s in 1:length(splited_nodes)){
        len <- length(level_nodes)
        level_nodes[[len + 1]] <- splited_nodes[[s]]
      }
    }
    
    # Linearity test
    if(is.null(validation_size)){
      train_predictions <- rep(0, nrow(embedded_series))
      for(ln in 1:length(level_nodes)){
        train_predictions <- train_predictions + as.numeric(fit_global_model(level_nodes[[ln]])[["predictions"]]) 
      }
      
      ss1 <- sum((embedded_series$y - train_predictions) ^ 2)
    }else{
      train_predictions <- rep(0, validation_size)
      for(ln in 1:length(level_nodes)){
        train_predictions <- train_predictions + as.numeric(fit_global_model(level_nodes[[ln]][train_indexes,], level_nodes[[ln]][test_indexes, -1])[["predictions"]]) 
      }
      
      ss1 <- sum((embedded_series[test_indexes, "y"] - train_predictions) ^ 2)
    }
    
    
    # Compute F-statistic. For details, see https://online.stat.psu.edu/stat501/lesson/6/6.2
    if(d == 1)
      ss0 <- initial_error
    else
      ss0 <- level_errors[d-1]
    
    if(is.null(validation_size)){
      f_stat <- ((ss0 - ss1)/(lag+1))/(ss1/(nrow(embedded_series) - 2*lag - 2))
      p_value <- pf(f_stat, lag+1, nrow(embedded_series) - 2*lag - 2, lower.tail = FALSE)
    }else{
      f_stat <- ((ss0 - ss1)/(lag+1))/(ss1/(validation_size - 2*lag - 2))
      p_value <- pf(f_stat, lag+1, validation_size - 2*lag - 2, lower.tail = FALSE)
    }
    
    
    print(paste0("SS0 = ", ss0))
    print(paste0("SS1 = ", ss1))
    print(paste0("F-statistic = ", f_stat))
    print(paste0("P-value = ", p_value))
    
    if(p_value < significance){
      print(paste0("Linearity test is significant. Adding a new level to the tree..."))
      tree[[d]] <- level_nodes
      thresholds[[d]] <- level_thresholds
      th_lags[[d]] <- level_th_lags
      gamma_vals[[d]] <- level_gamma_vals
      node_data <- tree[[d]]
      level_errors[d] <- ss1
    }else{
      print(paste0("Linearity test is not significant. Stop training..."))
      break
    }
  }
  
  
  if(length(tree) > 0){
    leaf_nodes <- tree[[length(tree)]]
    leaf_trained_models <- list()
    
    
    # Train a linear model per each leaf node
    for(ln in 1:length(leaf_nodes)){
      leaf_trained_models[[ln]] <- fit_global_model(leaf_nodes[[ln]])[["model"]] 
    }
  }else{
    final_trained_model <- fit_global_model(embedded_series)[["model"]]
  }
  
  
  
  # Forecasting
  forecasts <- NULL
  
  for(h in 1:forecast_horizon){
    horizon_predictions <- NULL
    
    if(length(tree) > 0){
      for(f in 1:nrow(final_lags)){
        leaf_instances <- get_leaf_instances(final_lags[f,], gamma_vals, thresholds, th_lags)
        
        pred <- 0
        
        for(lf in 1:length(leaf_instances)){
          pred <- pred + predict.glm(object = leaf_trained_models[[lf]], newdata = as.data.frame(leaf_instances[[lf]]))
        }
        
        horizon_predictions <- c(horizon_predictions, pred) 
      }
    }else{
      horizon_predictions <- predict.glm(object = final_trained_model, newdata = as.data.frame(final_lags))
    }
    
    
    forecasts <- cbind(forecasts, horizon_predictions)
    
    # Updating the test set for the next horizon
    if(h < forecast_horizon){
      final_lags <- final_lags[-lag]
      
      # Updating lags for the next horizon
      final_lags <- cbind(horizon_predictions, final_lags)
      colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
      
      final_lags <- as.data.frame(final_lags)
    }
  }
 
  if(integer_conversion)
    forecasts <- round(forecasts)
  
  if(is.null(validation_size))
    file_name <- paste0(dataset_name, "_lag_", lag, "_depth_", depth, "_optimizer_", optimizer, "_lstar")
  else
    file_name <- paste0(dataset_name, "_lag_", lag, "_depth_", depth, "_optimizer_", optimizer, "_validation_size_", validation_size, "_lstar")
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote=FALSE)
  
  print(gamma_vals)
  print(thresholds)
  print(th_lags)
  
  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
}


# Experiments

# NN5 Daily
do_lstar_forecasting("nn5_daily_dataset_without_missing_values.ts", 70, 56, "nn5_daily", depth = 1, optimizer = "BFGS")

do_lstar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 1, optimizer = "BFGS")
do_lstar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 2, optimizer = "BFGS")
do_lstar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 4, optimizer = "BFGS")

do_lstar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 1, optimizer = "BFGS", validation_size = 50)
do_lstar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 1, optimizer = "BFGS", validation_size = 200)
do_lstar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 1, optimizer = "BFGS", validation_size = 500)
do_lstar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 1, optimizer = "BFGS", validation_size = 1000)
do_lstar_forecasting("nn5_daily_dataset_without_missing_values.ts", 10, 56, "nn5_daily", depth = 4, optimizer = "BFGS", validation_size = 1000)


# Chaotic Logistic
do_lstar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 1, index = NULL, optimizer = "Nelder-Mead")
do_lstar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 1, index = NULL, optimizer = "BFGS")
do_lstar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 2, index = NULL, optimizer = "BFGS")
do_lstar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 4, index = NULL, optimizer = "BFGS")

do_lstar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 1, index = NULL, optimizer = "BFGS", validation_size = 50)
do_lstar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 2, index = NULL, optimizer = "BFGS", validation_size = 50)
do_lstar_forecasting("chaotic_logistic_dataset.ts", 10, 8, "chaotic_logistic", depth = 4, index = NULL, optimizer = "BFGS", validation_size = 50)


# Mackey glass
do_lstar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 1, index = NULL, optimizer = "Nelder-Mead")
do_lstar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 1, index = NULL, optimizer = "BFGS")
do_lstar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 2, index = NULL, optimizer = "BFGS")
do_lstar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 4, index = NULL, optimizer = "BFGS")

do_lstar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 1, index = NULL, optimizer = "BFGS", validation_size = 50)
do_lstar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 1, index = NULL, optimizer = "BFGS", validation_size = 200)
do_lstar_forecasting("mackey_glass_dataset.ts", 10, 8, "mackey_glass", depth = 4, index = NULL, optimizer = "BFGS", validation_size = 200)


# Kaggle daily
do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", depth = 1, integer_conversion = T, optimizer = "BFGS")
do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", depth = 2, integer_conversion = T, optimizer = "BFGS")
do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", depth = 4, integer_conversion = T, optimizer = "BFGS")

do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", depth = 1, integer_conversion = T, optimizer = "BFGS", validation_size = 500)
do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.ts", 10, 59, "kaggle_daily", depth = 4, integer_conversion = T, optimizer = "BFGS", validation_size = 500)

  







