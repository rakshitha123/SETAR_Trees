BASE_DIR <- "C:/Projects/SETAR_Trees/"
DATASET_DIR <- "C:/Projects/TSForecasting/"

# BASE_DIR <- "/home/rakshitha/Trees/"
# DATASET_DIR <- "/home/rakshitha/TSForecasting/"

source(file.path(DATASET_DIR, "utils", "data_loader.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "global_model_helper.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "tree_helper.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "global_models.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "local_univariate_models.R", fsep = "/"))

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
