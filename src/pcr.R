# Import packages
library(readr)
library(dplyr)
library(ggplot2)
library(tseries)

# read in data
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
                          c("sasdate", "year", "month", "inflationRate_1m_future", "inflationRate_3m_future"))

# Then create lags
pcr_data <- df1
for (col in predictor_cols) {
  for (lag in 1:6) { # Only using 6 lags of each variable, can use more but don't want to explode number of variables
    # basically creating name + lag of the variable
    lag_name <- paste0(col, "_lag", lag)
    pcr_data[[lag_name]] <- lag(df1[[col]], lag)
  }
}

### --- Prepare data --- ###
pcr_data <- pcr_data %>% 
  filter(year >= 1985)

X <- pcr_data %>%
  select(-all_of(c("sasdate", "year", "month", 
                   "inflationRate_1m_future", "inflationRate_3m_future"))) %>%
  as.matrix()

inflation1m <- pcr_data$inflationRate_1m_future
inflation3m <- pcr_data$inflationRate_3m_future

### --- DETERMINING PCR OPTIMAL NUMBER OF COMPONENTS --- ###

# --- APPROACH 1: Static Approach: meaning using training sample to determine optimal number of PCs once and for all --- ###
# Pros: less computationally expensive, dont have to keep refitting and computing bic
# Cons: no adaptation, bcos fixed across time.

### --- APPROACH 2: Dynamically determine optimal k, by computing BIC on train data each iteration --- ###
# Pros: adapts to recent trends, fantastic
# Cons: takes long

# First, we create helper function to help determine optimal k
# Standardization is done in rolling window/prior to this. I should have instead included it here but its wtv
choose_k_bic <- function(X, y, kmax) {
  # X represents STANDARDIZED VARIABLES
  # y is just target variable
  # kmax is if we want to set an upper limit for number of principle components. 
  # -- For now, we just put it as 20, since we have 120 oos forecasts, so dw to use too many
  n <- nrow(X)
  bic_values <- numeric(kmax)
  
  pca <- prcomp(X, center = F, scale = F)
  
  for (k in 1:kmax) {
    Z <- pca$x[, 1:k, drop = F]
    fit <- lm(y ~ Z)
    bic_values[k] <- BIC(fit)
  }
  
  which.min(bic_values)
}

### --- Static Approach --- ###
# First check for missing data, which is only S&P div yield in the latest data, so not na issue for now
na_counts <- colSums(is.na(X))
head(sort(na_counts, decreasing = TRUE), 10)

train_mask <- (pcr_data$year <= 2014)
X_tr <- X[train_mask, , drop = FALSE]
y_tr <- inflation1m[train_mask]

# Standardize predictors for PCA
mu_tr <- colMeans(X_tr) # mean
sd_tr <- apply(X_tr, 2, sd) # sd

which(sd_tr == 0) # check whether any sd = 0 to prevent division error
which(is.na(sd_tr)) # check whether any na's

X_tr_s <- scale(X_tr, center = mu_tr, scale = sd_tr) # then do standardization

# A brief check on whether standardization was applied correctly, obv there's more detailed checks, but good enough ig
means_check <- round(colMeans(X_tr_s), 4)
sds_check   <- round(apply(X_tr_s, 2, sd), 4)
print(unique(means_check)) 
print(unique(sds_check))

# Use BIC to optimal (static) k for 1-month model
y_tr_1m <- inflation1m[train_mask]
k_fixed_1m <- choose_k_bic(X_tr_s, y_tr_1m, kmax = 20)
cat("Chosen k for 1m model via BIC:", k_fixed_1m, "\n")

# Use BIC to optimal (static) k for 3-month model
y_tr_3m <- inflation3m[train_mask]
k_fixed_3m <- choose_k_bic(X_tr_s, y_tr_3m, kmax = 20)
cat("Chosen k for 3m model via BIC:", k_fixed_3m, "\n")


## --- Rolling PCR --- ###
roll_pcr <- function(y, X, nprev = 120, win_len = 360, kmax, fixed_k = NULL) {
  n <- nrow(X)
  
  # Store actual & predicted matrix & number of principal components used in each iteration
  preds  <- rep(NA_real_, nprev)
  actual <- rep(NA_real_, nprev)
  k_used <- rep(NA_integer_, nprev)
  
  # Rolling window
  for (i in 1:nprev) {
    te <- n - nprev + i - 1 # training window end
    ts <- max(1, te - win_len + 1) # training window start
    
    Xtr  <- X[ts:te, , drop = FALSE] # X for training
    ytr  <- y[ts:te] # y for training
    newx <- X[te + 1, , drop = FALSE] # X for testing later
    
    # Standardize within the training window
    mu <- colMeans(Xtr)
    sdv <- apply(Xtr, 2, sd)
    Xtr_s  <- scale(Xtr,  center = mu, scale = sdv)
    newx_s <- scale(newx, center = mu, scale = sdv)
    
    # Determine k - either dynamic or static
    if (is.null(fixed_k)) { 
      k <- choose_k_bic(Xtr_s, ytr, kmax) # determine optimal k using in-sample BIC each iteration
    } else {
      k <- fixed_k # use fixed k
    }
    k_used[i] <- k # store k used
    
    # Perform pca
    pca <- prcomp(Xtr_s, center = F, scale. = F)
    Ztr  <- pca$x[, 1:k, drop = F]
    znew <- newx_s %*% pca$rotation[, 1:k, drop = F]
    
    # Training model
    fit <- lm(ytr ~ Ztr)
    
    # Make new prediction
    preds[i] <- c(1, znew) %*% coef(fit)
    
    # Actual value
    actual[i] <- y[te + 1]
    
    ### --- ALIGNMENT CHECK TO ENSURE MODEL TRAINS AND COMPARES PREDICTION AGAINST THE RIGHT THING --- ###
    if (i == nprev - 1) {
      print(newx[, 1:5, drop = FALSE]) # this is like the predictor at (nprev - 1)-th forecast iteration
    }
    if (i == nprev) {
      print(tail(Xtr[, 1:5, drop = FALSE], 5)) # this is the (last observation's) predictors at (nprev)-th training iteration, which shud be equal to ^
      print(tail(ytr, 5)) # this is the training y
      print(tail(actual, 5)) # this is the oos y 
    }
    
    # Progress tracking
    cat("Completed", i, "of", nprev, "iterations\n")
  }
  
  RMSE <- sqrt(mean((actual - preds)^2, na.rm = TRUE))
  MAE  <- mean(abs(actual - preds), na.rm = TRUE)
  k_min <- min(k_used)   # Lowest k used
  k_max <- max(k_used)   # Highest k used
  list(pred = preds, actual = actual, RMSE = RMSE, MAE = MAE, k_min = k_min, k_max = k_max, k_final = k)
}

## --- Run for both targets ---
win_len <- 360 # 30-year rolling window
nprev   <- 127 # 2015 -> 2025 july as OOS (127 months)

# Static approach
res_1mf <- roll_pcr(inflation1m, X, nprev = nprev, win_len = win_len, kmax = 20, fixed_k = k_fixed_1m)
res_3mf <- roll_pcr(inflation3m, X, nprev = nprev, win_len = win_len, kmax = 20, fixed_k = k_fixed_3m)

cat("PCR STATIC K (1m) RMSE:", res_1mf$RMSE, " MAE:", res_1mf$MAE, " | Min PC:", res_1mf$k_min, "Max PC:", res_1mf$k_max, "Final PC:", res_1mf$k_final, "\n")
cat("PCR STATIC K (3m) RMSE:", res_3mf$RMSE, " MAE:", res_3mf$MAE, " | Min PC:", res_3mf$k_min, "Max PC:", res_3mf$k_max, "Final PC:", res_3mf$k_final, "\n")

# Dynamic approach
res_1m <- roll_pcr(inflation1m, X, nprev = nprev, win_len = win_len, kmax = 20, fixed_k = NULL)
res_3m <- roll_pcr(inflation3m, X, nprev = nprev, win_len = win_len, kmax = 20, fixed_k = NULL)

cat("PCR DYNAMIC K (1m) RMSE:", res_1m$RMSE, " MAE:", res_1m$MAE, " | Min PC:", res_1m$k_min, "Max PC:", res_1m$k_max, "Final PC:", res_1m$k_final, "\n")
cat("PCR DYNAMIC K (3m) RMSE:", res_3m$RMSE, " MAE:", res_3m$MAE, " | Min PC:", res_3m$k_min, "Max PC:", res_3m$k_max, "Final PC:", res_3m$k_final, "\n")

### --- Save results --- ###
results_pcr <- list(
  pcr_static_1m = res_1mf,
  pcr_static_3m = res_3mf,
  pcr_dynamic_1m = res_1m,
  pcr_dynamic_3m = res_3m,
  data_info = list(
    n_forecasts_1m = sum(!is.na(res_1m$pred)),
    n_forecasts_3m = sum(!is.na(res_3m$pred)),
    date_range = range(pcr_data$sasdate),
    static_k_1m = k_fixed_1m,
    static_k_3m = k_fixed_3m
  )
)

# Save PCR results
saveRDS(results_pcr, "../modelresults/pcr_complete_results.rds")

# Load PCR results
loaded_pcr <- readRDS("../modelresults/pcr_complete_results.rds")

# Access individual results
pcr_static_1m <- loaded_pcr$pcr_static_1m
pcr_dynamic_1m <- loaded_pcr$pcr_dynamic_1m
pcr_static_3m <- loaded_pcr$pcr_static_3m
pcr_dynamic_3m <- loaded_pcr$pcr_dynamic_3m

# PCR plotting function (same format as XGBoost)
plot_pcr_forecasts <- function(results, target_name, model_type = "PCR") {
  df <- data.frame(
    Actual = results$actual,
    Predicted = results$pred,
    Date = tail(pcr_data$sasdate, length(results$actual))
  )
  ggplot(df, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual")) +
    geom_line(aes(y = Predicted, color = "Predicted")) +
    labs(title = paste(model_type, target_name), y = "Inflation Rate") +
    theme_minimal()
}

# Create plots for all PCR models
plot_pcr_forecasts(pcr_static_1m, "1-Month Inflation Forecast (Static K)", "PCR")
plot_pcr_forecasts(pcr_dynamic_1m, "1-Month Inflation Forecast (Dynamic K)", "PCR")
plot_pcr_forecasts(pcr_static_3m, "3-Month Inflation Forecast (Static K)", "PCR")
plot_pcr_forecasts(pcr_dynamic_3m, "3-Month Inflation Forecast (Dynamic K)", "PCR")

# Print summary of all PCR results
cat("=== PCR RESULTS SUMMARY ===\n")
cat("Static K (1m)  - RMSE:", round(pcr_static_1m$RMSE, 4), "MAE:", round(pcr_static_1m$MAE, 4), 
    "K range:", pcr_static_1m$k_min, "-", pcr_static_1m$k_max, "\n")
cat("Dynamic K (1m) - RMSE:", round(pcr_dynamic_1m$RMSE, 4), "MAE:", round(pcr_dynamic_1m$MAE, 4),
    "K range:", pcr_dynamic_1m$k_min, "-", pcr_dynamic_1m$k_max, "\n")
cat("Static K (3m)  - RMSE:", round(pcr_static_3m$RMSE, 4), "MAE:", round(pcr_static_3m$MAE, 4),
    "K range:", pcr_static_3m$k_min, "-", pcr_static_3m$k_max, "\n")
cat("Dynamic K (3m) - RMSE:", round(pcr_dynamic_3m$RMSE, 4), "MAE:", round(pcr_dynamic_3m$MAE, 4),
    "K range:", pcr_dynamic_3m$k_min, "-", pcr_dynamic_3m$k_max, "\n")

