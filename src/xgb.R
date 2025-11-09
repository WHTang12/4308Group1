# Code is very similar to the PCR one, so if you are evaluating the code, then read them in succession
# Only difference is that in here, we use (time-series aware) cross validation to tune hyperparameters
# If strictly only want to get results, just run line 200 to load in results data

# Import packages
#install.packages("xgboost")
library(readr)
library(dplyr)
library(ggplot2)
library(xgboost)

# Keep row of required transformations aside first
df <- read_csv("../data/cleaned_fred2.csv")

### --- Define our target variables: 1-month inflation rate and 3-month cumulative inflation rate --- ###
df1 <- df %>%
  mutate(
    # Monthly inflation rate
    inflationRate_1m = 100 * (log_coreCPI - lag(log_coreCPI, 1)),
    # Next month's inflation
    inflationRate_1m_future = lead(inflationRate_1m, 1),
    # 3-month cumulative inflation
    inflationRate_3m = 100 * (log_coreCPI - lag(log_coreCPI, 3)),
    # 3-month-ahead cumulative inflation
    inflationRate_3m_future = lead(inflationRate_3m, 3)
  ) %>%
  select(-CPILFESL, -log_coreCPI, -log_lagcoreCPI, -inflationRate)

### --- Create 4 lags of all predictors, excluding Y --- ###
# First identify the predictors 
predictor_cols <- setdiff(names(df1), 
                          c("sasdate", "year", "month", "inflationRate_1m_future","inflationRate_3m_future"))

# Then create lags
xgb_data <- df1
for (col in predictor_cols) {
  for (lag in 1:6) { # Only using 6 lags of each variable, can use more but don't want to explode number of variables
    # basically creating name + lag of the variable
    lag_name <- paste0(col, "_lag", lag)
    xgb_data[[lag_name]] <- lag(df1[[col]], lag)
  }
}

### --- Prepare data --- ###
xgb_data <- xgb_data %>% 
  filter(year >= 1985)

X <- xgb_data %>%
  select(-all_of(c("sasdate", "year", "month", 
                   "inflationRate_1m_future", "inflationRate_3m_future"))) %>%
  as.matrix()
  
# Target variables
inflation1m <- xgb_data$inflationRate_1m_future
inflation3m <- xgb_data$inflationRate_3m_future

## --- XGBoost Rolling Window Function --- ###
rolling_xgb <- function(y, X, nprev = 120, win_len = 360) {
  n <- nrow(X)
  
  # Store actual & predicted matrix
  preds  <- rep(NA_real_, nprev)
  actual <- rep(NA_real_, nprev)

  
  # Rolling window
  for (i in 1:nprev) {
    # Total Training data indices
    te <- n - nprev + i - 1 # training window end
    ts <- max(1, te - win_len + 1) # training window start
    
    # Training data
    val_len <- 12
    X_tr <- X[ts:(te-val_len), , drop = F] # X for training
    y_tr <- y[ts:(te-val_len)] # y for training
    
    # Validation data (here we use 12 latest observation. Can use more ig but want to use as much latest data in initial model as much as possible)
    X_val <- X[(te-val_len+1), , drop = F]
    y_val <- y[(te-val_len+1)]
    
    # This is like a lazy way of dealing with missing NA's in the last few observations, we just skip the iteration
    if(is.na(y_val) || is.na(y[te+1]) || any(is.na(y_tr))) {
      cat("Skipping iteration", i, "\n")
      next
    }
    
    # Test data (next un-seen observation)
    newx <- X[te+1, , drop = F]
    
    # create DMatrix for XGBoost
    dtrain <- xgb.DMatrix(data = X_tr, label = y_tr)
    dval <- xgb.DMatrix(data = X_val, label = y_val)
    dtest <- xgb.DMatrix(data = newx)
    
    ### --- Hyperparameter tuning --- ###
    # Define grid
    grid <- expand.grid(
      max_depth = c(2, 3, 4, 5), # tree complexity. I didn't include 1 bcos i dont like stumps...
      eta = c(0.01, 0.05, 0.1) # learning rate
    )
    
    best_score <- Inf
    best_params <- NULL
    
    # Grid search cross-validation loop
    for (j in 1:nrow(grid)) { # for each combination of parameters
      params <- list(
        max_depth = grid$max_depth[j],
        eta = grid$eta[j],
        objective = "reg:squarederror",
        eval_metric = "rmse" # use rmse as scoring metric
      )
      
      # Train model
      set.seed(4308)
      xgb_model <- xgb.train(
        params = params,
        data = dtrain,
        nrounds = 100, # number of iterations
        watchlist = list(eval = dval),
        early_stopping_rounds = 10, # if validation error doesn't improve after 5 rounds, then just stop training
        verbose = 0 # mute the process output
      )
      
      # Update/Store best parameters to be used later
      if (xgb_model$best_score < best_score) {
        best_score <- xgb_model$best_score
        best_params <- list(
          max_depth = grid$max_depth[j],
          eta = grid$eta[j],
          nrounds = xgb_model$best_iteration
        )
      }
    }
    
    ### --- RETRAIN TRAINING MODEL WITH TUNED PARAMETERS --- ###
    X_tr_full <- X[ts:te, , drop = F]
    y_tr_full <- y[ts:te]
    dtrain_full <- xgb.DMatrix(data = X_tr_full, label = y_tr_full)
    
    final_params <- list(
      max_depth = best_params$max_depth,
      eta = best_params$eta,
      objective = "reg:squarederror"
    )
    
    xgb_final <- xgb.train(
      params = final_params,
      data = dtrain_full,
      nrounds = best_params$nrounds,
      verbose = 0
    )
    
    if(is.na(y_val) || is.na(y[te+1]) || any(is.na(y_tr))) {
      cat("Iteration", i, ": NA found in y_val =", y_val, "y_test =", y[te+1], "y_tr NAs =", sum(is.na(y_tr)), "\n")
      next
    }
    
    # Make OOS prediction
    preds[i] <- predict(xgb_final, dtest)
    actual[i] <- y[te + 1]
    
    # Progress tracking
    cat("Completed", i, "of", nprev, "iterations\n")
  }
  
  RMSE <- sqrt(mean((actual - preds)^2, na.rm = TRUE))
  MAE  <- mean(abs(actual - preds), na.rm = TRUE)

  list(pred = preds, actual = actual, RMSE = RMSE, MAE = MAE)
}

## --- Run for both targets ---
win_len <- 360 # 30-year rolling window
nprev <- 127 # OOS: Jan 2015 (predicting feb 2025) -> July 2025 (pred aug)

# 1-month ahead forecast
xgb_1m <- rolling_xgb(inflation1m, X, nprev = nprev, win_len = win_len) # TAKES APPROX 5MINS
cat("XGB (1m) RMSE:", xgb_1m$RMSE, " MAE:", xgb_1m$MAE, "\n") 

# 3-month ahead cumulative inflation forecast
xgb_3m <- rolling_xgb(inflation3m, X, nprev = nprev, win_len = win_len) # TAKES APPROX 5MINS
cat("XGB (3m) RMSE:", xgb_3m$RMSE, " MAE:", xgb_3m$MAE, "\n")

# Save data, bcos each function call takes like 3-5mins...
results <- list(
  xgb_1m = xgb_1m,
  xgb_3m = xgb_3m,
  data_info = list(
    n_forecasts_1m = sum(!is.na(xgb_1m$pred)),
    n_forecasts_3m = sum(!is.na(xgb_3m$pred)),
    date_range = range(xgb_data$sasdate)
  )
)

#saveRDS(results, "../modelresults/xgb_complete_results.rds")

# load in the saved results
#loaded_results <- readRDS("../modelresults/xgb_complete_results.rds")
xgb_1m <- loaded_results$xgb_1m
xgb_3m <- loaded_results$xgb_3m

cat("XGB (1m) RMSE:", xgb_1m$RMSE, " MAE:", xgb_1m$MAE, "\n") 
cat("XGB (3m) RMSE:", xgb_3m$RMSE, " MAE:", xgb_3m$MAE, "\n")

# predicted vs actual plot
plot_forecasts <- function(results, target_name) {
  df <- data.frame(
    Actual = results$actual,
    Predicted = results$pred,
    Date = tail(xgb_data$sasdate, length(results$actual))
  )
  ggplot(df, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual")) +
    geom_line(aes(y = Predicted, color = "Predicted")) +
    labs(title = paste("XGBoost", target_name), y = "Inflation Rate") +
    theme_minimal()
}

plot_forecasts(xgb_1m, "1-Month Inflation Forecast")
plot_forecasts(xgb_3m, "3-Month Inflation Forecast")

