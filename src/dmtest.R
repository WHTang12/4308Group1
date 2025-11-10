#install.packages("forecast")
library(forecast)

# Load all results from our models
ar_results <- readRDS("../modelresults/ar_model_results.rds")
pcr_results <- readRDS("../modelresults/pcr_complete_results.rds")
targeted_results <- readRDS("../modelresults/targeted_pcr_results.rds")
xgb_results <- readRDS("../modelresults/xgb_complete_results.rds")
load("../modelresults/rf_results.RData")
lasso_results <- readRDS("../modelresults/lasso_model_results.rds")
ridge_results1m <- read.csv("../modelresults/ridge_forecast_1month_ahead.csv")
ridge_results3m <- read.csv("../modelresults/ridge_forecast_3month_ahead.csv")
ridge_results6m <- read.csv("../modelresults/ridge_forecast_6month_ahead.csv")
ridge_results12m <- read.csv("../modelresults/ridge_forecast_12month_ahead.csv")
arrfxgb_avg_results <- readRDS("../modelresults/simple_avg_predictions.rds")
rfxgb_avg_results <- readRDS("../modelresults/rfxgb_avg_predictions.rds")

# Extract 1m forecasts
ar_1m <- ar_results$ar_1m$pred
lasso_1m <- lasso_results$lasso1m$pred
static_1m <- pcr_results$pcr_static_1m$pred
dynamic_1m <- pcr_results$pcr_dynamic_1m$pred
targeted_1m <- targeted_results$targeted_1m$pred
xgb_1m <- xgb_results$xgb_1m$pred
rf_1m <- as.numeric(rf_results$rf_1m$pred)
ridge_1m <- ridge_results1m$forecast
arrfxgb_1m <- arrfxgb_avg_results$simple_avg_1m
rfxgb_1m <- rfxgb_avg_results$rfxgb_avg_1m

# Extract 3m forecasts
ar_3m <- ar_results$ar_3m$pred
lasso_3m <- lasso_results$lasso3m$pred
static_3m <- pcr_results$pcr_static_3m$pred
dynamic_3m <- pcr_results$pcr_dynamic_3m$pred
targeted_3m <- targeted_results$targeted_3m$pred
xgb_3m <- xgb_results$xgb_3m$pred
rf_3m <- as.numeric(rf_results$rf_3m$pred)
ridge_3m <- ridge_results3m$forecast
arrfxgb_3m <- arrfxgb_avg_results$simple_avg_3m
rfxgb_3m <- rfxgb_avg_results$rfxgb_avg_3m

# Extract 6m forecasts
ar_6m <- ar_results$ar_6m$pred
lasso_6m <- lasso_results$lasso6m$pred
static_6m <- pcr_results$pcr_static_6m$pred
dynamic_6m <- pcr_results$pcr_dynamic_6m$pred
targeted_6m <- targeted_results$targeted_6m$pred
xgb_6m <- xgb_results$xgb_6m$pred
rf_6m <- as.numeric(rf_results$rf_6m$pred)
ridge_6m <- ridge_results6m$forecast
arrfxgb_6m <- arrfxgb_avg_results$simple_avg_6m
rfxgb_6m <- rfxgb_avg_results$rfxgb_avg_6m

# Extract 12m forecasts
ar_12m <- ar_results$ar_12m$pred
lasso_12m <- lasso_results$lasso12m$pred
static_12m <- pcr_results$pcr_static_12m$pred
dynamic_12m <- pcr_results$pcr_dynamic_12m$pred
targeted_12m <- targeted_results$targeted_12m$pred
xgb_12m <- xgb_results$xgb_12m$pred
rf_12m <- as.numeric(rf_results$rf_12m$pred)
ridge_12m <- ridge_results12m$forecast
arrfxgb_12m <- arrfxgb_avg_results$simple_avg_12m
rfxgb_12m <- rfxgb_avg_results$rfxgb_avg_12m

# These should all be equal btw, to ensure we all used the same OOS periods (i've already checked 1m, 6m, 12m beforehand)
ar_results$ar_3m$actual
lasso_results$lasso3m$actual
pcr_results$pcr_static_3m$actual
targeted_results$targeted_3m$actual
xgb_results$xgb_3m$actual
as.numeric(rf_results$rf_3m$actual)
ridge_results3m$actual

# Extract actual values (basically i just choose any model's actual values, since it's same for all)
actual_1m <- pcr_results$pcr_static_1m$actual
actual_3m <- pcr_results$pcr_static_3m$actual
actual_6m <- pcr_results$pcr_static_6m$actual
actual_12m <- pcr_results$pcr_static_12m$actual

# Compute forecast errors
# 1m
errors_ar_1m <- actual_1m[1:126] - ar_1m
errors_lasso_1m <- actual_1m - lasso_1m
errors_pcr_static_1m <- actual_1m - static_1m
errors_pcr_dynamic_1m <- actual_1m - dynamic_1m
errors_pcr_targeted_1m <- actual_1m - targeted_1m
errors_xgb_1m <- actual_1m - xgb_1m
errors_rf_1m <- actual_1m - rf_1m
errors_ridge_1m <- actual_1m - ridge_1m
errors_arrfxgb_1m <- actual_1m[1:126] - arrfxgb_1m
errors_rfxgb_1m <- actual_1m[1:126] - rfxgb_1m

# 3m
errors_ar_3m <- actual_3m[1:124] - ar_3m
errors_lasso_3m <- actual_3m - lasso_3m
errors_pcr_static_3m <- actual_3m - static_3m
errors_pcr_dynamic_3m <- actual_3m - dynamic_3m
errors_pcr_targeted_3m <- actual_3m - targeted_3m
errors_xgb_3m <- actual_3m - xgb_3m
errors_rf_3m <- actual_3m - rf_3m
errors_ridge_3m <- actual_3m - ridge_3m
errors_arrfxgb_3m <- actual_3m[1:124] - arrfxgb_3m
errors_rfxgb_3m <- actual_3m[1:124] - rfxgb_3m

# 6m
errors_ar_6m <- actual_6m[1:121] - ar_6m
errors_lasso_6m <- actual_6m - lasso_6m
errors_pcr_static_6m <- actual_6m - static_6m
errors_pcr_dynamic_6m <- actual_6m - dynamic_6m
errors_pcr_targeted_6m <- actual_6m - targeted_6m
errors_xgb_6m <- actual_6m - xgb_6m
errors_rf_6m <- actual_6m - rf_6m
errors_ridge_6m <- actual_6m - ridge_6m
errors_arrfxgb_6m <- actual_6m[1:121] - arrfxgb_6m
errors_rfxgb_6m <- actual_6m[1:121] - rfxgb_6m

# 12m
errors_ar_12m <- actual_12m[1:115] - ar_12m
errors_lasso_12m <- actual_12m - lasso_12m
errors_pcr_static_12m <- actual_12m - static_12m
errors_pcr_dynamic_12m <- actual_12m - dynamic_12m
errors_pcr_targeted_12m <- actual_12m - targeted_12m
errors_xgb_12m <- actual_12m - xgb_12m
errors_rf_12m <- actual_12m - rf_12m
errors_ridge_12m <- actual_12m - ridge_12m
errors_arrfxgb_12m <- actual_12m[1:115] - arrfxgb_12m
errors_rfxgb_12m <- actual_12m[1:115] - rfxgb_12m


run_dm <- function(errors_list, horizon, model_names) {
  n_models <- length(errors_list)
  
  # Results matrix, one for pvalue one for statistic
  p_values <- matrix(NA, nrow = n_models, ncol = n_models)
  statistics <- matrix(NA, nrow = n_models, ncol = n_models)
  rownames(p_values) <- model_names 
  colnames(p_values) <- model_names 
  rownames(statistics) <- model_names
  colnames(statistics) <- model_names
  
  # fill matrix using pairwise comparisons of model i and j, i != j
  for (i in 1:(n_models-1)) {
    for (j in (i+1):n_models) {
      test_result <- dm.test(errors_list[[i]], errors_list[[j]], 
                             h = horizon, alternative = "two.sided")
      p_values[i, j] <- p_values[j, i] <- test_result$p.value
      statistics[i, j] <- statistics[j, i] <- test_result$statistic
    }
  }
  
  return(list(p_values = p_values, statistics = statistics))
}

models_1m <- list(
  errors_ar_1m[1:126],
  errors_ridge_1m[1:126],
  errors_lasso_1m[1:126],
  errors_pcr_targeted_1m[1:126],
  errors_rf_1m[1:126],
  errors_xgb_1m[1:126],
  errors_rfxgb_1m[1:126],
  errors_arrfxgb_1m[1:126]
)

models_3m <- list(
  errors_ar_3m[1:124],
  errors_ridge_3m[1:124],
  errors_lasso_3m[1:124],
  errors_pcr_targeted_3m[1:124],
  errors_rf_3m[1:124],
  errors_xgb_3m[1:124],
  errors_rfxgb_3m[1:124],
  errors_arrfxgb_3m[1:124]
)

models_6m <- list(
  errors_ar_6m[1:121],
  errors_ridge_6m[1:121],
  errors_lasso_6m[1:121],
  errors_pcr_targeted_6m[1:121],
  errors_rf_6m[1:121],
  errors_xgb_6m[1:121],
  errors_rfxgb_6m[1:121],
  errors_arrfxgb_6m[1:121]
)

models_12m <- list(
  errors_ar_12m[1:115],
  errors_ridge_12m[1:115],
  errors_lasso_12m[1:115],
  errors_pcr_targeted_12m[1:115],
  errors_rf_12m[1:115],
  errors_xgb_12m[1:115],
  errors_rfxgb_12m[1:115],
  errors_arrfxgb_12m[1:115]
)

model_names <- c("AR", "Ridge", "LASSO", "Targeted PCR", "Random Forest", "XGBoost", "RF-XGB", "AR-RF-XGB")

# Run all pairwise tests
results_1m <- run_dm(models_1m, 1, model_names)
results_3m <- run_dm(models_3m, 3, model_names)
results_6m <- run_dm(models_6m, 6, model_names)
results_12m <- run_dm(models_12m, 12, model_names)

print(results_1m)
print(results_3m)
print(results_6m)
print(results_12m)
