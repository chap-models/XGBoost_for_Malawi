options(warn=1)

source("utils.R")

library(dplyr)
library(xgboost)

#library(fable)
#library(tsibble)
#library(lubridate)
#library(distributional) #to extract info from dist objects 

predict_chap <- function(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn) {
  future_per_location <- get_df_per_location(future_climatedata_fn)
  historic_per_location <- get_df_per_location(historic_data_fn)
  models <- readRDS(model_fn)  # Assumes the model was saved using saveRDS
  first_location <- TRUE

  predictors <- c("rainfall", "mean_temperature") # adjust to your variables
  response <- "disease_cases"
  n_samples <- 100

  for (location in names(future_per_location)){
    df <- future_per_location[[location]]
    historic_df <- historic_per_location[[location]]
    historic_df <- historic_df[!is.na(historic_df[[response]]), ]

    test_matrix <- as.matrix(df[, predictors])
    h_steps <- nrow(df)

    # Bootstrap: resample historic data, retrain, predict — repeat n_samples times
    boot_preds <- matrix(NA, nrow = h_steps, ncol = n_samples)
    set.seed(42)
    for (i in 1:n_samples) {
      boot_idx <- sample(nrow(historic_df), replace = TRUE)
      boot_df <- historic_df[boot_idx, ]
      train_matrix <- as.matrix(boot_df[, predictors])
      label <- boot_df[[response]]
      boot_model <- xgboost(data = train_matrix, label = label,
                            nrounds = 100, objective = "reg:squarederror", verbose = 0)
      boot_preds[, i] <- predict(boot_model, test_matrix)
    }

    sample_df <- df
    colnames(boot_preds) <- paste0("sample_", 0:(n_samples - 1))
    sample_df <- cbind(sample_df, boot_preds)

    if (first_location){
      full_df <- sample_df
      first_location <- FALSE
    } else {
      full_df <- rbind(full_df, sample_df)
    }
  }
  write.csv(full_df, predictions_fn, row.names = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 4) {
  model_fn <- args[1]
  historic_data_fn <- args[2]
  future_climatedata_fn <- args[3]
  predictions_fn <- args[4]
  
  predict_chap(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn)
}






