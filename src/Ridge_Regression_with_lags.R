rm(list = ls())
library(glmnet)
library(dplyr)
library(lubridate)

############################################################
## 1. Load and prepare data
############################################################
data <- read.csv("../data/cleaned_fred2.csv", stringsAsFactors = FALSE)
#data$sasdate <- as.Date(data$sasdate, format = "%d/%m/%Y")

# Create inflation variables
data <- data %>%
  mutate(
    # Monthly inflation (1-month change)
    inflationRate_1m = 100 * (log_coreCPI - lag(log_coreCPI, 1)),
    inflationRate_1m_future = lead(inflationRate_1m, 1),
    
    # 3-month cumulative inflation
    inflationRate_3m = 100 * (log_coreCPI - lag(log_coreCPI, 3)),
    inflationRate_3m_future = lead(inflationRate_3m, 3)
  )

# Remove missing rows
#data <- na.omit(data)

############################################################
## 2. Common settings across both forecast horizons
############################################################
window_size <- 360   # 30-year rolling window
lag_max <- 6         # number of inflation lags
dates_full <- data$sasdate
T <- nrow(data)

# Forecast period
test_data <- data %>%
  filter(sasdate >= as.Date("2015-01-01") & sasdate <= as.Date("2025-07-01"))
n_test <- nrow(test_data)

##############################################################
## 3. Define predictors (exclude date and inflation/CPI vars)
##############################################################
X_full <- data %>%
  select(-sasdate, -year, -month,
         -inflationRate_1m, -inflationRate_1m_future,
         -inflationRate_3m, -inflationRate_3m_future,
         -CPILFESL, -log_coreCPI, log_lagcoreCPI, inflationRate) %>%
  as.matrix()

############################################################
## 4. 1-MONTH-AHEAD MODEL (train & predict inflationRate_1m)
############################################################
cat("\n=== Running 1-month-ahead Ridge Forecast (inflationRate_1m) ===\n")

y_full <- data$inflationRate_1m
true_values <- test_data$inflationRate_1m_future
ridge_forecasts_1m <- rep(NA, n_test)

for (i in seq_along(test_data$sasdate)) {  # for loop for each month from Jan 2015 to Jul 2025
  forecast_date <- test_data$sasdate[i]
  idx_forecast <- which(dates_full == forecast_date)  # finds row number of date that matches forecast_date
  
  # Define rolling training window
  idx_train <- (idx_forecast - 2 - window_size + 1):(idx_forecast - 2)  # finds range of row numbers for relevant predictor data
  # e.g. to predict Jan 2015, train data from Dec 1984 to Nov 2014 to predict inflation from Jan 1985 to Dec 2014 respectively
  # check if indexes lie within the range of (1, T) where T is total no of obs in dataset
  if (min(idx_train - lag_max) < 1 || idx_forecast > T) next
  
  # Training target and predictors
  y_train <- y_full[idx_train + 1]  # model trained to use predictors in row idx_train to predict inflation in row idx_train + 1
  # train model to predict using last lag_max lags also
  inflation_lags_train <- t(sapply(idx_train, function(j) y_full[(j - lag_max):(j - 1)]))
  # combined predictors used in training
  X_train <- cbind(X_full[idx_train, , drop = FALSE], inflation_lags_train)
  
  # Forecast input (lags for the latest training period)
  last_train_idx <- idx_train[length(idx_train)]  # row index number of last training data in window
  # pick out last lag_max lags to use as predictors
  inflation_lags_forecast <- matrix(y_full[(last_train_idx - lag_max + 1):last_train_idx], nrow = 1)
  # combined predictors used in final prediction
  X_forecast <- cbind(matrix(X_full[last_train_idx, ], nrow = 1), inflation_lags_forecast)
  
  # Ridge fit and BIC selection
  lambda_grid <- 10^seq(4, -3, length = 100)
  ridge_fit <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_grid, standardize = TRUE)
  n <- length(y_train)
  mse_vals <- colSums((y_train - predict(ridge_fit, X_train))^2) / n
  ridge_df <- ridge_fit$df
  bic_vals <- log(mse_vals) + log(n) * ridge_df / n
  lambda_bic <- lambda_grid[which.min(bic_vals)]
  
  # Fit final model and forecast
  final_fit <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_bic, standardize = TRUE)
  ridge_forecasts_1m[i] <- predict(final_fit, X_forecast)
  
  if (i %% 12 == 0) cat("Forecasted up to:", as.character(forecast_date), "\n")  # print flags to follow progress
}

# Evaluate
rmse_1m <- sqrt(mean((ridge_forecasts_1m - true_values)^2, na.rm = TRUE))
mae_1m <- mean(abs(ridge_forecasts_1m - true_values), na.rm = TRUE)
cat("\nRMSE (1-month-ahead):", rmse_1m, "\n")
cat("MAE (1-month-ahead):", mae_1m, "\n")


# Plot
# plot(test_data$sasdate, true_values, type = "l", col = "black", lwd = 2,
#      ylab = "Inflation Rate", xlab = "Date",
#      main = "Ridge Forecast (1-month-ahead)")
# lines(test_data$sasdate, ridge_forecasts_1m, col = "blue", lwd = 2)
# legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lwd = 2)

# Export 1-month-ahead forecast results
ridge_results_1m <- data.frame(
  date = test_data$sasdate,
  actual = true_values,
  forecast = ridge_forecasts_1m
)
#write.csv(ridge_results_1m, "../modelresults/ridge_forecast_1month_ahead.csv", row.names = FALSE)
cat("Saved: ridge_forecast_1month_ahead.csv\n")

############################################################
## 5. 3-MONTH-AHEAD MODEL (train & predict inflationRate_3m)
############################################################
cat("\n=== Running 3-month-ahead Ridge Forecast (inflationRate_3m) ===\n")

y_full <- data$inflationRate_3m
true_values <- test_data$inflationRate_3m_future
ridge_forecasts_3m <- rep(NA, n_test)

for (i in seq_along(test_data$sasdate)) {  # for loop for each month from Jan 2015 to Jul 2025
  forecast_date <- test_data$sasdate[i]
  idx_forecast <- which(dates_full == forecast_date)  # finds row number of date that matches forecast_date
  
  # Define rolling training window
  idx_train <- (idx_forecast - 4 - window_size + 1):(idx_forecast - 4)  # finds range of row numbers for relevant predictor data
  # e.g. to predict Jan 2015, train data from Oct 1984 to Sep 2014 to predict inflation from Jan 1985 to Dec 2014 respectively
  # check if indexes lie within the range of (1, T) where T is total no of obs in dataset
  if (min(idx_train - lag_max) < 1 || (idx_forecast + 3) > T) next
  
  # Training target and predictors
  y_train <- y_full[idx_train + 3]  # model trained to use predictors in row idx_train to predict inflation in row idx_train + 3
  # train model to predict using last lag_max lags also
  inflation_lags_train <- t(sapply(idx_train, function(j) y_full[(j - lag_max):(j - 1)]))
  # combined predictors used in training
  X_train <- cbind(X_full[idx_train, , drop = FALSE], inflation_lags_train)
  
  # Forecast input
  last_train_idx <- idx_train[length(idx_train)]  # row index number of last training data in window
  # pick out last lag_max lags to use as predictors
  inflation_lags_forecast <- matrix(y_full[(last_train_idx - lag_max + 1):last_train_idx], nrow = 1)
  # combined predictors used in prediction
  X_forecast <- cbind(matrix(X_full[last_train_idx, ], nrow = 1), inflation_lags_forecast)
  
  # Ridge fit and BIC selection
  lambda_grid <- 10^seq(4, -3, length = 100)
  ridge_fit <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_grid, standardize = TRUE)
  n <- length(y_train)
  mse_vals <- colSums((y_train - predict(ridge_fit, X_train))^2) / n
  ridge_df <- ridge_fit$df
  bic_vals <- log(mse_vals) + log(n) * ridge_df / n
  lambda_bic <- lambda_grid[which.min(bic_vals)]
  
  # Fit final model and forecast
  final_fit <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_bic, standardize = TRUE)
  ridge_forecasts_3m[i] <- predict(final_fit, X_forecast)
  
  if (i %% 12 == 0) cat("Forecasted up to:", as.character(forecast_date), "\n")  # print flags to follow progress
}

# Evaluate
rmse_3m <- sqrt(mean((ridge_forecasts_3m - true_values)^2, na.rm = TRUE))
mae_3m <- mean(abs(ridge_forecasts_3m - true_values), na.rm = TRUE)
cat("\nRMSE (3-month-ahead):", rmse_3m, "\n")
cat("MAE (3-month-ahead):", mae_3m, "\n")


# Plot
# plot(test_data$sasdate, true_values, type = "l", col = "black", lwd = 2,
#      ylab = "3-month cumulative inflation", xlab = "Date",
#      main = "Ridge Forecast (3-month-ahead)")
# lines(test_data$sasdate, ridge_forecasts_3m, col = "red", lwd = 2)
# legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lwd = 2)

# Export 3-month-ahead forecast results
ridge_results_3m <- data.frame(
  date = test_data$sasdate,
  actual = true_values,
  forecast = ridge_forecasts_3m
)
#write.csv(ridge_results_3m, "../modelresults/ridge_forecast_3month_ahead.csv", row.names = FALSE)
cat("Saved: ridge_forecast_3month_ahead.csv\n")