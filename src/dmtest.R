#install.packages("forecast")
library(forecast)

# Load all results from our models
pcr_results <- readRDS("../modelresults/pcr_complete_results.rds")
targeted_results <- readRDS("../modelresults/targeted_pcr_results.rds")
xgb_results <- readRDS("../modelresults/xgb_complete_results.rds")
load("../modelresults/rf_results.RData")
ridge_results1m <- read.csv("../modelresults/ridge_forecast_1month_ahead.csv")
ridge_results3m <- read.csv("../modelresults/ridge_forecast_3month_ahead.csv")

# Extract forecasts
static_1m <- pcr_results$pcr_static_1m$pred
dynamic_1m <- pcr_results$pcr_dynamic_1m$pred
targeted_1m <- targeted_results$targeted_1m$pred
xgb_1m <- xgb_results$xgb_1m$pred
rf_1m <- as.numeric(rf_results$rf_1m$pred)
ridge_1m <- ridge_results1m$forecast

static_3m <- pcr_results$pcr_static_3m$pred
dynamic_3m <- pcr_results$pcr_dynamic_3m$pred
targeted_3m <- targeted_results$targeted_3m$pred
xgb_3m <- xgb_results$xgb_3m$pred
rf_3m <- as.numeric(rf_results$rf_3m$pred)
ridge_3m <- ridge_results3m$forecast

# These should all be equal btw, to ensure we all used the same OOS periods
pcr_results$pcr_static_3m$actual
targeted_results$targeted_3m$actual
xgb_results$xgb_3m$actual
as.numeric(rf_results$rf_3m$actual)
ridge_results3m$actual

# Extract actual values
actual_1m <- pcr_results$pcr_static_1m$actual
actual_3m <- pcr_results$pcr_static_3m$actual

# Compute forecast errors
errors_pcr_static_1m <- actual_1m - static_1m
errors_pcr_dynamic_1m <- actual_1m - dynamic_1m
errors_pcr_targeted_1m <- actual_1m - targeted_1m
errors_xgb_1m <- actual_1m - xgb_1m
errors_rf_1m <- actual_1m - rf_1m
errors_ridge_1m <- actual_1m - ridge_1m

errors_pcr_static_3m <- actual_3m - static_3m
errors_pcr_dynamic_3m <- actual_3m - dynamic_3m
errors_pcr_targeted_3m <- actual_3m - targeted_3m
errors_xgb_3m <- actual_3m - xgb_3m
errors_rf_3m <- actual_3m - rf_3m
errors_ridge_3m <- actual_3m - ridge_3m

# DM Test 
