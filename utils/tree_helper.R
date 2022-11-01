# SETAR tree helper functions


# A function to split a parent node into child nodes
create_split <- function(data, conditional_lag, threshold){
  
  left_node <- data[data[,(conditional_lag + 1)] < threshold,]
  right_node <- data[data[,(conditional_lag + 1)] >= threshold,]
  
  list("left_node" = left_node, "right_node" = right_node)
}


# A function to traverse through the tree based on the used thresholds and lags during splitting
tree_traverse <- function(instance, split, threshold){
  direction <- "left"
  
  if(instance[,split] >= threshold)  
    direction <- "right"
  
  direction
}


# A function to identify the leaf node corresponding with a given test instance
get_leaf_index <- function(instance, splits, thresholds){
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
        direction <- tree_traverse(instance, splits[[sp]][current_split], thresholds[[sp]][current_split])
        
        if(direction == "left")
          current_split <- next_possible_splits[1]
        else
          current_split <- next_possible_splits[2]
      }
    }else{
      direction <- tree_traverse(instance, splits[[sp]][current_split], thresholds[[sp]][current_split])
      next_possible_splits <- tail(1: (current_split*divide_factor), divide_factor)
      
      if(direction == "left")
        current_split <- next_possible_splits[1]
      else
        current_split <- next_possible_splits[2]
    }
  }
  current_split
}


# A function to calculate Sum of Squared Errors (SSE)
SS <- function(p, train_data, current_lg) {
  splitted_nodes <- create_split(train_data, current_lg, p)
  
  left <- splitted_nodes$left_node
  right <-  splitted_nodes$right_node 
  
  if((nrow(left) > 0) & (nrow(right) > 0)){ 
    residuals_l <- left$y - fit_global_model(left, NULL)$predictions
    residuals_r <- right$y - fit_global_model(right, NULL)$predictions
    current_residuals <- c(residuals_l, residuals_r)
    cost <-  sum(current_residuals ^ 2)
  }else{
    cost <- Inf
  }
  
  cost
}


# A function to check whether there exists a remaining non-linearity in the parent node instances
check_linearity <- function(parent_node, child_nodes, lag, significance){
  print("lin test")
  
  is_significant <- TRUE
  
  ss0 <- sum((parent_node$y - as.numeric(fit_global_model(parent_node, NULL)[["predictions"]])) ^2) 
  
  if(ss0 == 0){
    is_significant <- FALSE
  }else{
    train_residuals <- NULL
    for(ln in 1:length(child_nodes)){
      train_residuals <- c(train_residuals, (child_nodes[[ln]]$y - as.numeric(fit_global_model(child_nodes[[ln]], NULL)[["predictions"]]))) 
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


# A function to check whether a considerable error reduction (depends on error threshold) can be gained by splitting a parent node into child nodes
check_error_improvement <- function(parent_node, child_nodes, error_threshold){
  print("error improvement")
  
  is_improved <- TRUE
  
  ss0 <- sum((parent_node$y - as.numeric(fit_global_model(parent_node, NULL)[["predictions"]])) ^2) 
  
  if(ss0 == 0){
    is_improved <- FALSE
  }else{
    train_residuals <- NULL
    for(ln in 1:length(child_nodes)){
      train_residuals <- c(train_residuals, (child_nodes[[ln]]$y - as.numeric(fit_global_model(child_nodes[[ln]], NULL)[["predictions"]]))) 
    }
    
    ss1 <- sum(train_residuals ^ 2)
    
    improvement <- (ss0-ss1)/ss0
    
    if(improvement < error_threshold)
      is_improved <- FALSE
    
    print(paste0("Error improvement = ", improvement, " Enough improvement ", is_improved))
  }
  
  is_improved
}