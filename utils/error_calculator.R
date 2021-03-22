library(smooth)

# Function to calculate series wise smape values
#
# Parameters
# forecasts - a matrix containing forecasts for a set of series
#             no: of rows should be equal to number of series and no: of columns should be equal to the forecast horizon 
# test_set - a matrix with the same dimensions as 'forecasts' containing the actual values corresponding with them
calculate_smape <- function(forecasts, test_set){
  epsilon <- 0.1
  sum <- NULL
  comparator <- data.frame(matrix((0.5 + epsilon), nrow = nrow(test_set), ncol = ncol(test_set)))
  sum <- pmax(comparator, (abs(forecasts) + abs(test_set) + epsilon))
  smape <- 2 * abs(forecasts - test_set) / (sum)
  smape_per_series <- rowMeans(smape, na.rm = TRUE)
  smape_per_series
}


# Function to calculate series wise mase values
#
# Parameters
# forecasts - a matrix containing forecasts for a set of series
#             no: of rows should be equal to number of series and no: of columns should be equal to the forecast horizon 
# test_set - a matrix with the same dimensions as 'forecasts' containing the actual values corresponding with them
# training_set - a matrix containing the training series
# seasonality - frequency of the dataset, e.g. 12 for monthly
calculate_mase <- function(forecasts, test_set, training_set, seasonality){
  mase_per_series = NULL
  
  for(k in 1 :nrow(forecasts)){
    mase_per_series[k] = MASE(as.numeric(test_set[k,]), as.numeric(forecasts[k,]), mean(abs(diff(as.numeric(unlist(training_set[k])), lag = max(seasonality), differences = 1))))
    
    if(is.na(mase_per_series[k]))
        mase_per_series[k] = MASE(as.numeric(test_set[k,]), as.numeric(forecasts[k,]), mean(abs(diff(as.numeric(unlist(training_set[k])), lag = min(seasonality), differences = 1))))
    
    if(is.na(mase_per_series[k]))
        mase_per_series[k] = MASE(as.numeric(test_set[k,]), as.numeric(forecasts[k,]), mean(abs(diff(as.numeric(unlist(training_set[k])), lag = 1, differences = 1))))        
  }
  
  mase_per_series <- mase_per_series[!is.infinite(mase_per_series) & !is.na(mase_per_series)]
  mase_per_series
}


calculate_errors <- function(forecasts, test_set, training_set, seasonality, file_name){
  #calculating smape
  smape_per_series <- calculate_smape(forecasts, test_set)
  
  #calculating mase
  mase_vector <- calculate_mase(forecasts, test_set, training_set, seasonality)
 
  write.table(smape_per_series, file.path(BASE_DIR, "results", "errors", paste0(file_name, "_smape_errors.txt")), row.names = F, col.names = F, quote = F)
  write.table(mase_vector, file.path(BASE_DIR, "results", "errors", paste0(file_name, "_mase_errors.txt")), row.names = F, col.names = F, quote = F)
  
  mean_smape <- paste0("Mean SMAPE: ", mean(smape_per_series, na.rm = TRUE))
  median_smape <- paste0("Median SMAPE: ", median(smape_per_series, na.rm = TRUE))
  mean_mase <- paste0("Mean MASE: ", mean(mase_vector, na.rm = TRUE))
  median_mase <- paste0("Median MASE: ", median(mase_vector, na.rm = TRUE))
  
  print(mean_smape)
  print(median_smape)
  print(mean_mase)
  print(median_mase)
  
  write(c(mean_smape, median_smape, mean_mase, median_mase, "\n"), file = file.path(BASE_DIR, "results", "errors", paste0(file_name, ".txt")), append = FALSE)
}


aggregate_sub_dataset_errors <- function(error_file_names){
  all_errors <- NULL
  
  for(i in 1:length(error_file_names)){
    errors <- read.csv(file.path(BASE_DIR, "results", "errors", paste0(error_file_names[i], ".txt")), header = F)
    all_errors <- c(all_errors, errors$V1)
  }
  
  print(paste0("Mean: ", mean(all_errors)))
  print(paste0("Median: ", median(all_errors)))
}
