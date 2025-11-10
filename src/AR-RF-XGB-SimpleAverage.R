ar_results <- readRDS("../modelresults/ar_model_results.rds")
xgb_results <- readRDS("../modelresults/xgb_complete_results.rds")
load("../modelresults/rf_results.RData")

# Extract forecasts
ar_1m <- ar_results$ar_1m$pred
xgb_1m <- xgb_results$xgb_1m$pred
rf_1m <- as.numeric(rf_results$rf_1m$pred)

ar_3m <- ar_results$ar_3m$pred
xgb_3m <- xgb_results$xgb_3m$pred
rf_3m <- as.numeric(rf_results$rf_3m$pred)

ar_6m <- ar_results$ar_6m$pred
xgb_6m <- xgb_results$xgb_6m$pred
rf_6m <- as.numeric(rf_results$rf_6m$pred)

ar_12m <- ar_results$ar_12m$pred
xgb_12m <- xgb_results$xgb_12m$pred
rf_12m <- as.numeric(rf_results$rf_12m$pred)

# Simple average for 1-month horizon
simple_avg_1m <- (xgb_1m[1:126] + rf_1m[1:126] + ar_1m[1:126]) / 3

# Simple average for 3-month horizon
simple_avg_3m <- (xgb_3m[1:124] + rf_3m[1:124] + ar_3m[1:124]) / 3

# Simple average for 6-month horizon  
simple_avg_6m <- (xgb_6m[1:121] + rf_6m[1:121] + ar_6m[1:121]) / 3

# Simple average for 12-month horizon
simple_avg_12m <- (xgb_12m[1:115] + rf_12m[1:115] + ar_12m[1:115]) / 3

# actual values
actual_1m <- xgb_results$xgb_1m$actual[1:126]
actual_3m <- xgb_results$xgb_3m$actual[1:124]
actual_6m <- xgb_results$xgb_6m$actual[1:121]
actual_12m <- xgb_results$xgb_12m$actual[1:115]

# Calculate RMSE
rmse_simple_1m <- sqrt(mean((actual_1m - simple_avg_1m)^2, na.rm = TRUE))
rmse_simple_3m <- sqrt(mean((actual_3m - simple_avg_3m)^2, na.rm = TRUE))
rmse_simple_6m <- sqrt(mean((actual_6m - simple_avg_6m)^2, na.rm = TRUE))
rmse_simple_12m <- sqrt(mean((actual_12m - simple_avg_12m)^2, na.rm = TRUE))

mae_simple_1m <- mean(abs(actual_1m - simple_avg_1m), na.rm = TRUE)
mae_simple_3m <- mean(abs(actual_3m - simple_avg_3m), na.rm = TRUE)
mae_simple_6m <- mean(abs(actual_6m - simple_avg_6m), na.rm = TRUE)
mae_simple_12m <- mean(abs(actual_12m - simple_avg_12m), na.rm = TRUE)

cat("SIMPLE AVG (1m) RMSE:", rmse_simple_1m, " MAE:", mae_simple_1m, "\n")
cat("SIMPLE AVG (3m) RMSE:", rmse_simple_3m, " MAE:", mae_simple_3m, "\n")
cat("SIMPLE AVG (6m) RMSE:", rmse_simple_6m, " MAE:", mae_simple_6m, "\n")
cat("SIMPLE AVG (12m) RMSE:", rmse_simple_12m, " MAE:", mae_simple_12m, "\n")

### --- RF-XGB ONLY --- ###
# 1-month horizon
rfxgb_avg_1m <- (xgb_1m[1:126] + rf_1m[1:126]) / 2

# 3-month horizon
rfxgb_avg_3m <- (xgb_3m[1:124] + rf_3m[1:124]) / 2

# 6-month horizon  
rfxgb_avg_6m <- (xgb_6m[1:121] + rf_6m[1:121]) / 2

# 12-month horizon
rfxgb_avg_12m <- (xgb_12m[1:115] + rf_12m[1:115]) / 2

# Calculate RMSE and MAE
rmse_rfxgb_1m <- sqrt(mean((actual_1m - rfxgb_avg_1m)^2, na.rm = TRUE))
rmse_rfxgb_3m <- sqrt(mean((actual_3m - rfxgb_avg_3m)^2, na.rm = TRUE))
rmse_rfxgb_6m <- sqrt(mean((actual_6m - rfxgb_avg_6m)^2, na.rm = TRUE))
rmse_rfxgb_12m <- sqrt(mean((actual_12m - rfxgb_avg_12m)^2, na.rm = TRUE))

mae_rfxgb_1m <- mean(abs(actual_1m - rfxgb_avg_1m), na.rm = TRUE)
mae_rfxgb_3m <- mean(abs(actual_3m - rfxgb_avg_3m), na.rm = TRUE)
mae_rfxgb_6m <- mean(abs(actual_6m - rfxgb_avg_6m), na.rm = TRUE)
mae_rfxgb_12m <- mean(abs(actual_12m - rfxgb_avg_12m), na.rm = TRUE)

cat("RF-XGB AVG (1m) RMSE:", rmse_rfxgb_1m, " MAE:", mae_rfxgb_1m, "\n")
cat("RF-XGB AVG (3m) RMSE:", rmse_rfxgb_3m, " MAE:", mae_rfxgb_3m, "\n")
cat("RF-XGB AVG (6m) RMSE:", rmse_rfxgb_6m, " MAE:", mae_rfxgb_6m, "\n")
cat("RF-XGB AVG (12m) RMSE:", rmse_rfxgb_12m, " MAE:", mae_rfxgb_12m, "\n")

# Save
# Save simple average predictions
simple_avg_results <- list(
  simple_avg_1m = simple_avg_1m,
  simple_avg_3m = simple_avg_3m,
  simple_avg_6m = simple_avg_6m,
  simple_avg_12m = simple_avg_12m
)

# Save RF-XGB average predictions  
rfxgb_avg_results <- list(
  rfxgb_avg_1m = rfxgb_avg_1m,
  rfxgb_avg_3m = rfxgb_avg_3m,
  rfxgb_avg_6m = rfxgb_avg_6m,
  rfxgb_avg_12m = rfxgb_avg_12m
)

# Save to RDS files
saveRDS(simple_avg_results, "../modelresults/simple_avg_predictions.rds")
saveRDS(rfxgb_avg_results, "../modelresults/rfxgb_avg_predictions.rds")
