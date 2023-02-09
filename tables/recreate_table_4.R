BASE_DIR <- "SETAR_Trees"
RESULTS_DIR <- file.path(BASE_DIR, "results", "errors")


dataset_name <- "chaotic_logistic"
error_measures <- c("smape", "mase")
freq <- c(5, 10, 20, 50)

for(m in error_measures){
  assign(paste0("mean_", m), NULL) 
  assign(paste0("median_", m), NULL) 
}

for(e in error_measures){
  for(f in freq)
  if(file.exists(file.path(RESULTS_DIR, paste0(dataset_name, "_setarforest_bagging_freq_", f, "_random_tree_significance_random_tree_error_threshold_random_significance_divider_", e, "_errors.txt")))){
    errors <- read.csv(file.path(RESULTS_DIR, paste0(dataset_name, "_setarforest_bagging_freq_", f, "_random_tree_significance_random_tree_error_threshold_random_significance_divider_", e, "_errors.txt")), header = FALSE)$V1
    assign(paste0("mean_", e), c(eval(parse(text=paste0("mean_", e))), mean(errors, na.rm = TRUE))) 
    assign(paste0("median_", e), c(eval(parse(text=paste0("median_", e))), median(errors, na.rm = TRUE))) 
  }else{ 
    assign(paste0("mean_", e), c(eval(parse(text=paste0("mean_", e))), NA)) 
    assign(paste0("median_", e), c(eval(parse(text=paste0("median_", e))), NA)) 
  }
}

mean_smape <- as.numeric(format(round(mean_smape*100, 2), nsmall = 2))
median_smape <- as.numeric(format(round(median_smape*100, 2), nsmall = 2))
mean_mase <- as.numeric(format(round(mean_mase, 3), nsmall = 3))
median_mase <- as.numeric(format(round(median_mase, 3), nsmall = 3))

errors <- data.frame("No_of_Trees" = freq,
                     "Mean_msMAPE" = mean_smape, 
                     "Median_msMAPE" = median_smape, 
                     "Mean_MASE" = mean_mase,  
                     "Median_MASE" = median_mase)


print("Table 4 Errors")
errors




