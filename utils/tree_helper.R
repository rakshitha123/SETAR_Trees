# SETAR tree helper functions

create_split <- function(data, conditional_lag, threshold, categorical_covariates = NULL){
  is_changed <- FALSE
  unique_vals <- NULL
  
  if(!is.null(categorical_covariates)){
    cat_indexes <- (which(colnames(data) %in% categorical_covariates))
    
    if((conditional_lag + 1) %in% cat_indexes){
      unique_vals <- unique(as.numeric(levels(data[,conditional_lag + 1])))
      data[,conditional_lag + 1] <- as.numeric(as.character(data[,conditional_lag + 1]))
      is_changed <- TRUE
    }
  }
  
  left_node <- data[data[,(conditional_lag + 1)] < threshold,]
  right_node <- data[data[,(conditional_lag + 1)] >= threshold,]
  
  if(is_changed){
    left_node[,(conditional_lag + 1)] <- factor(left_node[,(conditional_lag + 1)], levels = unique_vals)
    right_node[,(conditional_lag + 1)] <- factor(right_node[,(conditional_lag + 1)], levels = unique_vals)
  }
  
  list("left_node" = left_node, "right_node" = right_node)
}


tree_traverse <- function(instance, split, threshold, categorical_covariates = NULL){
  direction <- "left"
  
  if(!is.null(categorical_covariates)){
    cat_indexes <- which(colnames(instance) %in% categorical_covariates)
    
    if(split %in% cat_indexes)
      instance[,split] <- as.numeric(as.character(instance[,split]))
  }
  
  if(instance[,split] >= threshold)  #[[paste0("Lag", split)]]
    direction <- "right"
  
  direction
}


get_leaf_index <- function(instance, splits, thresholds, categorical_covariates= NULL){
  current_split <- 1
  divide_factor <- 2
  
  for(sp in 1:length(splits)){
    
    if(length(which(splits[[sp]] == 0)) > 0){
      zeros <- which(splits[[sp]] == 0)
      
      change_count <- 0
      
      for(d in 1:length(zeros)){
        if(zeros[d] < current_split){
          change_count <- change_count + 1
        }
      }
      
      next_possible_splits <- tail(1: (current_split*divide_factor), divide_factor)
      next_possible_splits <- next_possible_splits - change_count
      
      if(splits[[sp]][current_split] == 0){
        current_split <- next_possible_splits[1]
      }else{
        direction <- tree_traverse(instance, splits[[sp]][current_split], thresholds[[sp]][current_split], categorical_covariates)
        
        if(direction == "left")
          current_split <- next_possible_splits[1]
        else
          current_split <- next_possible_splits[2]
      }
    }else{
      direction <- tree_traverse(instance, splits[[sp]][current_split], thresholds[[sp]][current_split], categorical_covariates)
      next_possible_splits <- tail(1: (current_split*divide_factor), divide_factor)
      
      if(direction == "left")
        current_split <- next_possible_splits[1]
      else
        current_split <- next_possible_splits[2]
    }
  }
  current_split
}


# Sum of squares function
SS <- function(p, train_data, current_lg, categorical_covariates = NULL) {
  splitted_nodes <- create_split(train_data, current_lg, p, categorical_covariates)
  
  left <- splitted_nodes$left_node
  right <-  splitted_nodes$right_node 
  
  if(nrow(left) > 0 & nrow(right) > 0){
    residuals_l <- left$y - fit_global_model(left, NULL, categorical_covariates)$predictions
    residuals_r <- right$y - fit_global_model(right, NULL, categorical_covariates)$predictions
    current_residuals <- c(residuals_l, residuals_r)
    cost <-  sum(current_residuals ^ 2)
  }else{
    cost <- Inf
  }
  
  cost
}


check_linearity <- function(parent_node, child_nodes, lag, significance, categorical_covariates = NULL){
  print("lin test")
  
  is_significant <- TRUE
  
  ss0 <- sum((parent_node$y - as.numeric(fit_global_model(parent_node, NULL, categorical_covariates)[["predictions"]])) ^2) 
  
  if(ss0 == 0){
    is_significant <- FALSE
  }else{
    train_residuals <- NULL
    for(ln in 1:length(child_nodes)){
      train_residuals <- c(train_residuals, (child_nodes[[ln]]$y - as.numeric(fit_global_model(child_nodes[[ln]], NULL, categorical_covariates)[["predictions"]]))) 
    }
    
    ss1 <- sum(train_residuals ^ 2)
    
    
    # Compute F-statistic. For details, see https://online.stat.psu.edu/stat501/lesson/6/6.2
    f_stat <- ((ss0 - ss1)/(lag+1))/(ss1/(nrow(parent_node) - 2*lag - 2))
    p_value <- pf(f_stat, lag+1, nrow(parent_node) - 2*lag - 2, lower.tail = FALSE)
    
    if(p_value > significance)
      is_significant <- FALSE
    
    print(paste0("P-value = ", p_value, " Significant ", is_significant))
  }
  
  is_significant
}


check_error_improvement <- function(parent_node, child_nodes, error_threshold, categorical_covariates = NULL){
  print("error improvement")
  
  is_improved <- TRUE
  
  ss0 <- sum((parent_node$y - as.numeric(fit_global_model(parent_node, NULL, categorical_covariates)[["predictions"]])) ^2) 
  
  if(ss0 == 0){
    is_improved <- FALSE
  }else{
    train_residuals <- NULL
    for(ln in 1:length(child_nodes)){
      train_residuals <- c(train_residuals, (child_nodes[[ln]]$y - as.numeric(fit_global_model(child_nodes[[ln]], NULL, categorical_covariates)[["predictions"]]))) 
    }
    
    ss1 <- sum(train_residuals ^ 2)
    
    improvement <- (ss0-ss1)/ss0
    
    if(improvement < error_threshold)
      is_improved <- FALSE
    
    print(paste0("Error improvement = ", improvement, " Enough improvement ", is_improved))
  }
  
  is_improved
}