# Creating embedded matrix and final lags to train the global models
create_input_matrix <- function(training_set, lag, scale = FALSE, test_set = NULL, categorical_covariates = NULL, numerical_covariates = NULL){
  embedded_series <- NULL
  final_lags <- NULL
  series_means <- NULL
  
  dataset <- training_set$series
  
  for (i in 1:length(dataset)) {
    print(i)
    time_series <- as.numeric(unlist(dataset[i], use.names = FALSE))
    
    mean <- mean(time_series)
    
    if(mean != 0){
      series_means <- c(series_means, mean)
      
      if(scale)
        time_series <- time_series / mean
    }else
      series_means <- c(series_means, 1)
    
    # Embed the series
    embedded <- embed(time_series, lag + 1)
    
    # Add categorical covariates
    if(!is.null(categorical_covariates)){
      for(cat_cov in categorical_covariates){
        cat_cov_series <- training_set[[cat_cov]][[i]]
        cat_cov_series <- cat_cov_series[(lag + 1): length(cat_cov_series)]
        embedded <- cbind(embedded, cat_cov_series)
      }
    }
    
    # Add numerical covariates
    if(!is.null(numerical_covariates)){
      for(num_cov in numerical_covariates){
        num_cov_series <- training_set[[num_cov]][[i]]
        num_cov_series <- num_cov_series[(lag + 1): length(num_cov_series)]
        embedded <- cbind(embedded, num_cov_series)
      }
    }
    
    if (!is.null(embedded_series)) 
      embedded_series <- as.matrix(embedded_series)
    
    embedded_series <- rbind(embedded_series, embedded)
    
    # Creating the test set
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))
    
    # Add categorical covariates
    if(!is.null(categorical_covariates)){
      for(cat_cov in categorical_covariates){
        cat_cov_final_lags <- test_set[[cat_cov]][i, 1]
        current_series_final_lags <- cbind(current_series_final_lags, cat_cov_final_lags)
      }
    }
    
    # Add categorical covariates
    if(!is.null(numerical_covariates)){
      for(num_cov in numerical_covariates){
        num_cov_final_lags <- test_set[[num_cov]][i, 1]
        current_series_final_lags <- cbind(current_series_final_lags, num_cov_final_lags)
      }
    }
    
    if (!is.null(final_lags)) {
      final_lags <- as.matrix(final_lags)
    }
    
    final_lags <- rbind(final_lags, current_series_final_lags)
  }
  
  embedded_series <- as.data.frame(embedded_series)
  colnames(embedded_series)[1] <- "y"
  colnames(embedded_series)[2:(lag + 1)] <- paste("Lag", 1:lag, sep = "")
  
  final_lags <- as.data.frame(final_lags)
  colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep = "")
  
  if(!is.null(categorical_covariates)){
    colnames(embedded_series)[(lag + 2):(lag + length(categorical_covariates) + 1)] <- categorical_covariates
    colnames(final_lags)[(lag + 1):(lag + length(categorical_covariates))] <- categorical_covariates
    embedded_series[(lag + 2):(lag + length(categorical_covariates) + 1)] <- lapply(embedded_series[(lag + 2):(lag + length(categorical_covariates) + 1)], function(x) as.factor(x))
    final_lags[(lag + 1):(lag + length(categorical_covariates))] <- lapply(final_lags[(lag + 1):(lag + length(categorical_covariates))], function(x) as.factor(x))
  }
  
  if(!is.null(numerical_covariates)){
    colnames(embedded_series)[(ncol(embedded_series) - length(numerical_covariates) + 1):ncol(embedded_series)] <- numerical_covariates
    colnames(final_lags)[(ncol(final_lags) - length(numerical_covariates) + 1):ncol(final_lags)] <- numerical_covariates
  }
 
  list(embedded_series, final_lags, series_means) 
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
create_train_test_sets <- function(input_file_name, key, index, forecast_horizon, categorical_covariates = NULL, numerical_covariates = NULL, series_prefix = NULL, splitter = "_"){
  loaded_data <- convert_tsf_to_tsibble(file.path(DATASET_DIR, "tsf_data", input_file_name, fsep = "/"), VALUE_COL_NAME, key, index)
  dataset <- loaded_data[[1]]
  frequency <- loaded_data[[2]]
  
  if(!is.null(frequency))
    seasonality <- SEASONALITY_MAP[[frequency]]
  else
    seasonality <- 1
  
  if(!is.null(categorical_covariates) | !is.null(numerical_covariates)){
    dataset$type <- stringr:::str_split_fixed(dataset[[key]], splitter, 2)[,1]
    
    all_types <- unique(dataset$type)
      
    if(!all(c(categorical_covariates, numerical_covariates) %in% all_types))
      stop("One or more covariate series are not defined in the .tsf file")
    
    if(!(series_prefix %in% all_types))
      stop("Either series_prefix is wrong or the series that require forecasts are missing in the .tsf file")
  }
    
  
  training_set <- list()
  test_set <- list()
  
  if(is.null(categorical_covariates) & is.null(numerical_covariates)){
    output <- split_data(dataset, forecast_horizon) 
    training_set[["series"]] <- output[[1]]
    test_set[["series"]] <- output[[2]]
  }else{
    for(type in c(series_prefix, categorical_covariates, numerical_covariates)){
      process_data <- dataset[dataset$type == type,]
      output <- split_data(process_data, forecast_horizon) 
      if(type == series_prefix){
        training_set[["series"]] <- output[[1]]
        test_set[["series"]] <- output[[2]]
      }else{
        training_set[[type]] <- output[[1]]
        test_set[[type]] <- output[[2]]
      }
    }
  }
  
  
  list(training_set, test_set, seasonality)
}


split_data <- function(input, forecast_horizon){
  all_series_names <- unique(input$series_name)
  
  series_training_set <- list()
  series_test_set <- matrix(0, nrow = length(all_series_names), ncol = forecast_horizon)
  
  for(s in seq_along(all_series_names)){
    series_data <- input[input$series_name == as.character(all_series_names[s]), ]
    train_series_data <- series_data[1:(nrow(series_data) - forecast_horizon),][[VALUE_COL_NAME]]
    test_series_data <- series_data[(nrow(series_data) - forecast_horizon + 1):nrow(series_data),][[VALUE_COL_NAME]]
    
    series_training_set[[s]] <- train_series_data
    series_test_set[s,] <- test_series_data
  } 
  
  list(series_training_set, data.frame(series_test_set))
}


# Fitting a global regression model
fit_global_model <- function(fitting_data, test_data = NULL, categorical_covariates = NULL) {
  if(!is.null(categorical_covariates)){
    # cat_node_indexes <- which(colnames(fitting_data) %in% categorical_covariates)
    # 
    # for(cn_ind in cat_node_indexes){
    #   if(nlevels(fitting_data[,cn_ind]) == 1)
    #     fitting_data[,cn_ind] <- as.numeric(fitting_data[,cn_ind])
    # }
    
    
    
    for(cat in categorical_covariates){
      if(cat %in% colnames(fitting_data)){
        if(nlevels(fitting_data[[cat]]) == 1)
          fitting_data[[cat]] <- as.numeric(fitting_data[[cat]])
      }
    }
  }
  
  model <- glm(formula = create_formula(fitting_data), data = fitting_data)
  
  if(is.null(test_data))
    global_predictions  <- model$fitted.values
  else  
    global_predictions <- predict.glm(object = model, newdata = as.data.frame(test_data))
  
  list("predictions" = global_predictions, "model" = model)
}


check_leaf_data_matching <- function(leaf_data, instance, categorical_covariates){

  changed_leaf <- FALSE
  
  for(cat in categorical_covariates){
    if(cat %in% colnames(leaf_data)){
      if(nlevels(leaf_data[[cat]]) == 1)
        instance[[cat]] <- as.numeric(instance[[cat]])
      
      if(!(instance[[cat]] %in% as.numeric(levels(leaf_data[[cat]])))){
        leaf_data[[cat]] <- as.numeric(leaf_data[[cat]])
        instance[[cat]] <- as.numeric(instance[[cat]])
        changed_leaf <- TRUE
      }
    }
  }
  
  list("leaf_data" = leaf_data, "instance" = instance, "changed_leaf"  = changed_leaf)
  
}








