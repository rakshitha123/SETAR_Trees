BASE_DIR <- "C:/Projects/SETAR_Trees/"

source(file.path(BASE_DIR, "configs", "configs.R", fsep = "/"))


get_leaf_instances <- function(original_instance, gamma_vals, thresholds, th_lags){
  leaf_instances <- NULL
  instances<- list(original_instance)
  
  for(g in 1:length(gamma_vals)){
    l_gammas <- gamma_vals[[g]]
    l_thresholds <- thresholds[[g]]
    l_th_lags <- th_lags[[g]]
    
    next_level <- list()
    
    for(i in 1:length(instances)){
      if(l_th_lags[i] > 0){
        len <- length(next_level)
        left_instance <- instances[[i]] * G(instances[[i]][,l_th_lags[i]], l_gammas[i], l_thresholds[i])
        next_level[[len + 1]] <- left_instance
        
        len <- length(next_level)
        right_instance <- instances[[i]] * (1 - G(instances[[i]][,l_th_lags[i]], l_gammas[i], l_thresholds[i]))
        next_level[[len + 1]] <- right_instance
      }else{
        len <- length(next_level)
        next_level[[len + 1]] <- instances[[i]] 
      }
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


create_split <- function(data, gamma, th, th_lag){
  left_node <- data* G(data[-1][,th_lag], gamma, th)
  right_node <- data* (1 - G(data[-1][,th_lag], gamma, th))
  list("left_node" = left_node, "right_node" = right_node)
}


check_linearity <- function(parent_node, child_nodes, lag, significance){
  print("lin test")
  
  is_significant <- TRUE
  
  ss0 <- sum((parent_node$y - as.numeric(fit_global_model(parent_node)[["predictions"]])) ^2) 
  
  if(ss0 == 0){
    is_significant <- FALSE
  }else{
    lin_predictions <- rep(0, nrow(parent_node))
    for(ln in 1:length(child_nodes)){
      lin_predictions <- lin_predictions + as.numeric(fit_global_model(child_nodes[[ln]])[["predictions"]]) 
    }
    
    ss1 <- sum((parent_node$y - lin_predictions) ^ 2)
    
    
    # Compute F-statistic. For details, see https://online.stat.psu.edu/stat501/lesson/6/6.2
    f_stat <- ((ss0 - ss1)/(lag+1))/(ss1/(nrow(parent_node) - 2*lag - 2))
    p_value <- pf(f_stat, lag+1, nrow(parent_node) - 2*lag - 2, lower.tail = FALSE)
    
    if(p_value > significance)
      is_significant <- FALSE
    
    print(paste0("P-value = ", p_value, " Significant ", is_significant))
  }
  
  is_significant
}


do_lstar_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, depth = 2, key = "series_name", index = "start_timestamp", integer_conversion = F, significance = 0.05, optimizer = "BFGS"){
  
  loaded_data <- create_train_test_sets(input_file_name, key, index, forecast_horizon)
  training_set <- loaded_data[[1]]
  test_set <- loaded_data[[2]]
  seasonality <- loaded_data[[3]]
  
  result <- create_input_matrix(training_set, lag)
  embedded_series <- result[[1]]
  final_lags <- result[[2]]
  # series_means <- result[[3]]
  
  
  # Start timestamp
  start_time <- Sys.time()
  
  
  # Set list of defaults:
  start.con<-list(
    nTh = 3, 
    nGamma = 5, # number of gamma values that will be used for testing
    gamma_int = c(1,100))
  
  
  gammas <- seq(start.con$gamma_int[1], start.con$gamma_int[2], length.out=start.con$nGamma) # Divide the gamma interval 
  
  
  # Sum of squares function
  # p: vector of parameters
  SS <- function(p, train_data, optimal_lag) {
    gamma <- p[1] # Extract parms from vector p
    th <- p[2] 
   
    pred_1 <- fit_global_model(train_data * G(train_data[-1][,optimal_lag], gamma, th))$predictions
    pred_2 <- fit_global_model(train_data * (1 - G(train_data[-1][,optimal_lag], gamma, th)))$predictions
    final_preds <- as.numeric(pred_1 + pred_2)
    residuals <- train_data$y - final_preds
   
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
    level_significant_node_count <- 0
    
    for(n in 1:length(node_data)){
      best_cost <- Inf
      
      for(lg in 1:lag){
        print(lg)
        ths <- seq(min(node_data[[n]][,lg+1]), max(node_data[[n]][,lg+1]), length.out = start.con$nTh) # Threshold interval is the minimum and maximum values in the corresponding lag
        IDS <- as.matrix(expand.grid(gammas, ths) ) 
        
        for(ids in 1:nrow(IDS)){
          pred_l <- fit_global_model(node_data[[n]]* G(node_data[[n]][,lg+1], IDS[ids,1], IDS[ids,2]))$predictions
          pred_r <- fit_global_model(node_data[[n]] * (1 - G(node_data[[n]][,lg+1], IDS[ids,1], IDS[ids,2])))$predictions
          current_final_preds <- as.numeric(pred_l + pred_r)
          current_residuals <- node_data[[n]]$y - current_final_preds
          
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
      res <- optim(p, SS, method = optimizer, lower = c(0, -Inf), train_data = node_data[[n]], optimal_lag = th_lag)
      
      
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
      
      splited_nodes <- create_split(node_data[[n]], best_gamma, best_th, best_th_lag)
      
      is_significant <- check_linearity(node_data[[n]], splited_nodes, lag, significance)
      
      if(is_significant){
        level_gamma_vals <- c(level_gamma_vals, best_gamma)
        level_thresholds <- c(level_thresholds, best_th)
        level_th_lags <- c(level_th_lags, best_th_lag)
        level_significant_node_count <- level_significant_node_count + 1
        
        for(s in 1:length(splited_nodes)){
          len <- length(level_nodes)
          level_nodes[[len + 1]] <- splited_nodes[[s]]
        }
      }else{
        level_gamma_vals <- c(level_gamma_vals, 0)
        level_thresholds <- c(level_thresholds, 0)
        level_th_lags <- c(level_th_lags, 0)
        
        len <- length(level_nodes)
        level_nodes[[len + 1]] <- node_data[[n]]
      }
    }
    
    
    if(level_significant_node_count > 0){
      tree[[d]] <- level_nodes
      thresholds[[d]] <- level_thresholds
      th_lags[[d]] <- level_th_lags
      gamma_vals[[d]] <- level_gamma_vals
      node_data <- tree[[d]]
    }else
      break
  }
  
  
  # Check whether the tree has any nodes. If not, train a single pooled regression model
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
 
  
  # Finish timestamp
  end_time <- Sys.time()
  
  
  if(integer_conversion)
    forecasts <- round(forecasts)
  
  file_name <- paste0(dataset_name, "_lag_", lag, "_depth_", depth, "_optimizer_", optimizer, "_lstar")
  
  write.table(forecasts, file.path(BASE_DIR, "results", "forecasts", "star", paste0(file_name, "_forecasts.txt"), fsep = "/"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  print(gamma_vals)
  print(thresholds)
  print(th_lags)
  print(paste0("Executed for ", length(tree), " levels"))
  
  if(length(tree) > 1)
    print(paste0("Insignificant node count = ", length(tree[[length(tree) - 1]]) - level_significant_node_count))
  
  # Execution time
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
  
  
  calculate_errors(forecasts, test_set, training_set, seasonality, file_name)
}



# Experiments

# Chaotic Logistic
# do_lstar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1, index = NULL, optimizer = "Nelder-Mead")
# do_lstar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 1, index = NULL)
# do_lstar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 2, index = NULL)
# do_lstar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 4, index = NULL)
# do_lstar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 6, index = NULL, significance = 0.001)
# do_lstar_forecasting("chaotic_logistic_dataset.tsf", 10, 8, "chaotic_logistic", depth = 10, index = NULL, significance = 0.001)


# Mackey glass
# do_lstar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 1, index = NULL, optimizer = "Nelder-Mead")
# do_lstar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 1, index = NULL)
# do_lstar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 2, index = NULL)
# do_lstar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 4, index = NULL)
# do_lstar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 6, index = NULL, significance = 0.001)
# do_lstar_forecasting("mackey_glass_dataset.tsf", 10, 8, "mackey_glass", depth = 10, index = NULL, significance = 0.001)


# Tourism Quarterly
# do_lstar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 6, significance = 0.001)
# do_lstar_forecasting("tourism_quarterly_dataset.tsf", 10, 8, "tourism_quarterly", depth = 10, significance = 0.001)


# Kaggle daily
# do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 74, 59, "kaggle_daily", depth = 1, integer_conversion = T)

# do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 1, integer_conversion = T)
# do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 2, integer_conversion = T)
# do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 4, integer_conversion = T)
# do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 6, integer_conversion = T, significance = 0.001)
# do_lstar_forecasting("kaggle_web_traffic_dataset_1000_without_missing_values.tsf", 10, 59, "kaggle_daily", depth = 10, integer_conversion = T, significance = 0.001)


# NN5 Daily
# do_lstar_forecasting("nn5_daily_dataset_without_missing_values.tsf", 70, 56, "nn5_daily", depth = 1)
# 
# do_lstar_forecasting("nn5_daily_dataset_without_missing_values.tsf", 10, 56, "nn5_daily", depth = 1)
# do_lstar_forecasting("nn5_daily_dataset_without_missing_values.tsf", 10, 56, "nn5_daily", depth = 2)
# do_lstar_forecasting("nn5_daily_dataset_without_missing_values.tsf", 10, 56, "nn5_daily", depth = 4)
# do_lstar_forecasting("nn5_daily_dataset_without_missing_values.tsf", 10, 56, "nn5_daily", depth = 6, significance = 0.001)








