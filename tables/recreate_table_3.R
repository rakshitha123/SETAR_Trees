BASE_DIR <- "SETAR_Trees"
RESULTS_DIR <- file.path(BASE_DIR, "results", "errors")


dataset_names <- c("rossmann", 
                   "kaggle_daily", 
                   "favourita", 
                   "m5", 
                   "tourism_monthly", 
                   "tourism_quarterly", 
                   "chaotic_logistic", 
                   "mackey_glass", 
                   "rossmann_with_cov", 
                   "kaggle_daily_with_cov", 
                   "favourita_with_cov")

models <- c("ets", 
            "arima", 
            "setar", 
            "star", 
            "pooled_regression", 
            "cubist",
            "ffnn", 
            "regression_tree", 
            "catboost", 
            "lightgbm", 
            "xgboost", 
            "rf", 
            "setartree_both", 
            "setarforest_bagging_freq_10_random_tree_significance_random_tree_error_threshold_random_significance_divider")

model_names <- c("ETS", 
                 "ARIMA", 
                 "SETAR", 
                 "STAR", 
                 "PR", 
                 "Cubist", 
                 "FFNN", 
                 "Regression Tree", 
                 "CatBoost", 
                 "LightGBM", 
                 "XGBoost", 
                 "RF", 
                 "Tree.Lin.Test.Error.Red", 
                 "Forest.Significance.Error.Red")

error_measures <- c("smape", "mase")


for(name in dataset_names){
  for(m in error_measures){
    assign(paste0(name, "_mean_", m), NULL) 
    assign(paste0(name, "_median_", m), NULL) 
  }
}

for(i in 1:length(dataset_names)){
  for(j in 1:length(models)){
    for(k in 1:length(error_measures)){
      if(file.exists(file.path(RESULTS_DIR, paste0(dataset_names[i], "_", models[j], "_", error_measures[k], "_errors.txt")))){
        errors <- read.csv(file.path(RESULTS_DIR, paste0(dataset_names[i], "_", models[j], "_", error_measures[k], "_errors.txt")), header = FALSE)$V1
        assign(paste0(dataset_names[i], "_mean_", error_measures[k]), c(eval(parse(text=paste0(dataset_names[i], "_mean_", error_measures[k]))), mean(errors, na.rm = TRUE))) 
        assign(paste0(dataset_names[i], "_median_", error_measures[k]), c(eval(parse(text=paste0(dataset_names[i], "_median_", error_measures[k]))), median(errors, na.rm = TRUE))) 
      }else{ 
        assign(paste0(dataset_names[i], "_mean_", error_measures[k]), c(eval(parse(text=paste0(dataset_names[i], "_mean_", error_measures[k]))), NA)) 
        assign(paste0(dataset_names[i], "_median_", error_measures[k]), c(eval(parse(text=paste0(dataset_names[i], "_median_", error_measures[k]))), NA)) 
      }
    }
  }
}

for(name in dataset_names){
  assign(paste0(name, "_mean_smape"), as.numeric(format(round(eval(parse(text=paste0(name, "_mean_smape")))*100, 2), nsmall = 2))) 
  assign(paste0(name, "_median_smape"), as.numeric(format(round(eval(parse(text=paste0(name, "_median_smape")))*100, 2), nsmall = 2))) 
  
  assign(paste0(name, "_mean_mase"), as.numeric(format(round(eval(parse(text=paste0(name, "_mean_mase"))), 3), nsmall = 3))) 
  assign(paste0(name, "_median_mase"), as.numeric(format(round(eval(parse(text=paste0(name, "_median_mase"))), 3), nsmall = 3))) 
}

mean_smape_errors <- data.frame("Model" = model_names,
                               "Rossmann" = rossmann_mean_smape, 
                               "Kaggle" = kaggle_daily_mean_smape, 
                               "Favorita" = favourita_mean_smape, 
                               "M5" = m5_mean_smape, 
                               "Tour_M" = tourism_monthly_mean_smape, 
                               "Tour_Q" = tourism_quarterly_mean_smape, 
                               "Chaotic" = chaotic_logistic_mean_smape, 
                               "Mackey_Glass" = mackey_glass_mean_smape, 
                               "Rossmann_Cov" = rossmann_with_cov_mean_smape, 
                               "Kaggle_Cov" = kaggle_daily_with_cov_mean_smape, 
                               "Favorita_Cov" = favourita_with_cov_mean_smape)

median_smape_errors <- data.frame("Model" = model_names,
                                "Rossmann" = rossmann_median_smape, 
                                "Kaggle" = kaggle_daily_median_smape, 
                                "Favorita" = favourita_median_smape, 
                                "M5" = m5_median_smape, 
                                "Tour_M" = tourism_monthly_median_smape, 
                                "Tour_Q" = tourism_quarterly_median_smape, 
                                "Chaotic" = chaotic_logistic_median_smape, 
                                "Mackey_Glass" = mackey_glass_median_smape, 
                                "Rossmann_Cov" = rossmann_with_cov_median_smape, 
                                "Kaggle_Cov" = kaggle_daily_with_cov_median_smape, 
                                "Favorita_Cov" = favourita_with_cov_median_smape)

mean_mase_errors <- data.frame("Model" = model_names,
                                "Rossmann" = rossmann_mean_mase, 
                                "Kaggle" = kaggle_daily_mean_mase, 
                                "Favorita" = favourita_mean_mase, 
                                "M5" = m5_mean_mase, 
                                "Tour_M" = tourism_monthly_mean_mase, 
                                "Tour_Q" = tourism_quarterly_mean_mase, 
                                "Chaotic" = chaotic_logistic_mean_mase, 
                                "Mackey_Glass" = mackey_glass_mean_mase, 
                                "Rossmann_Cov" = rossmann_with_cov_mean_mase, 
                                "Kaggle_Cov" = kaggle_daily_with_cov_mean_mase, 
                                "Favorita_Cov" = favourita_with_cov_mean_mase)

median_mase_errors <- data.frame("Model" = model_names,
                                  "Rossmann" = rossmann_median_mase, 
                                  "Kaggle" = kaggle_daily_median_mase, 
                                  "Favorita" = favourita_median_mase, 
                                  "M5" = m5_median_mase, 
                                  "Tour_M" = tourism_monthly_median_mase, 
                                  "Tour_Q" = tourism_quarterly_median_mase, 
                                  "Chaotic" = chaotic_logistic_median_mase, 
                                  "Mackey_Glass" = mackey_glass_median_mase, 
                                  "Rossmann_Cov" = rossmann_with_cov_median_mase, 
                                  "Kaggle_Cov" = kaggle_daily_with_cov_median_mase, 
                                  "Favorita_Cov" = favourita_with_cov_median_mase)



print("Table 3 Mean msMAPE Errors")
mean_smape_errors

print("Table 3 Median msMAPE Errors")
median_smape_errors

print("Table 3 Mean MASE Errors")
mean_mase_errors

print("Table 3 Median MASE Errors")
median_mase_errors
