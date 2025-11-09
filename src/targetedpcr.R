# Over here, we do targeted PCR, based on Bai & Ng (2018)
# Idea is that we first determine which variables we are gonna consider in "making" our principal components

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
  for (lag in 1:4) { # Only using 4 lags of each variable, can use more but don't want to explode number of variables
    # basically creating name + lag of the variable
    lag_name <- paste0(col, "_lag", lag)
    pcr_data[[lag_name]] <- lag(df1[[col]], lag)
  }
}

### --- Prepare data --- ###
pcr_data <- pcr_data %>% 
  filter(year >= 1985)

X <- pcr_data %>%
  select(-all_of(c("sasdate", "year", "month", "inflationRate_1m_future", "inflationRate_3m_future"))) %>%
  as.matrix()

inflation1m <- pcr_data$inflationRate_1m_future
inflation3m <- pcr_data$inflationRate_3m_future

# First, we create helper function to help determine optimal k (detailed explanation in pcr.R)
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

## --- Rolling PCR --- ###
roll_targeted_pcr <- function(y, X, nprev = 120, win_len = 360, kmax, t_threshold = 1.96) {
  n <- nrow(X)
  
  # Store actual & predicted matrix & number of principal components used in each iteration
  preds  <- rep(NA_real_, nprev)
  actual <- rep(NA_real_, nprev)
  k_used <- rep(NA_integer_, nprev)
  n_predictors_used <- rep(NA_integer_, nprev)
  
  # Rolling window
  for (i in 1:nprev) {
    te <- n - nprev + i - 1 # training window end
    ts <- max(1, te - win_len + 1) # training window start
    
    Xtr  <- X[ts:te, , drop = FALSE] # X for training
    ytr  <- y[ts:te] # y for training
    newx <- X[te + 1, , drop = FALSE] # X for testing later
    
    ### --- First, determine which variables will even go into our principle component considerations --- ###
    # here, we will regress Y on each predictor and obtain t-stat, and only pick those variables where t-statistic > 1.96
    t_stats <- numeric(ncol(Xtr))
    selected_predictors <- integer(0)
    
    for (j in 1:ncol(Xtr)) { # for each predictor
      fit <- lm(ytr ~ Xtr[, j]) # we fit a univariate regression
      t_stats[j] <- summary(fit)$coefficients[2, 3] # obtain and store t-statistic
      
      if (abs(t_stats[j]) > t_threshold) { # if |t-stat| > critical value, we keep
        selected_predictors <- c(selected_predictors, j) 
      }
    }
    
    Xtr_selected <- Xtr[, selected_predictors, drop = F] # our set of selected predictors
    newx_selected <- newx[, selected_predictors, drop = F] 
    n_predictors_used[i] <- length(selected_predictors) # we also track how many predictors we use, just for troubleshooting to see if use too few/many
    
    ### --- At this point, we just follow pcr.R, so nothing new --- ###
    # Standardize within the training window
    mu <- colMeans(Xtr_selected)
    sdv <- apply(Xtr_selected, 2, sd)
    Xtr_s  <- scale(Xtr_selected,  center = mu, scale = sdv)
    newx_s <- scale(newx_selected, center = mu, scale = sdv)
    
    k <- choose_k_bic(Xtr_s, ytr, kmax) # determine optimal k using in-sample BIC each iteration
    k_used[i] <- k 
    
    # Perform pca
    pca <- prcomp(Xtr_s, center = F, scale. = F)
    Ztr  <- pca$x[, 1:k, drop = F]
    znew <- newx_s %*% pca$rotation[, 1:k, drop = F]
    
    # Training model
    model <- lm(ytr ~ Ztr)
    
    # Make new prediction
    preds[i] <- c(1, znew) %*% coef(model)
    
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
  avg_predictors_used <- mean(n_predictors_used)
  list(pred = preds, actual = actual, RMSE = RMSE, MAE = MAE, k_min = k_min, k_max = k_max, k_final = k, avg_predictors_used = avg_predictors_used)
}

## --- Run for both targets ---
win_len <- 360 # 30-year rolling window
nprev   <- 127 # OOS period: Jan 2015 to July 2025

# Run
targeted_1m <- roll_targeted_pcr(inflation1m, X, nprev = nprev, win_len = win_len, kmax = 20, t_threshold = 1.96)
targeted_3m <- roll_targeted_pcr(inflation3m, X, nprev = nprev, win_len = win_len, kmax = 20, t_threshold = 1.96)

### --- Save results --- ###
results_targeted_pcr <- list(
  targeted_1m = targeted_1m,
  targeted_3m = targeted_3m,
  data_info = list(
    n_forecasts_1m = sum(!is.na(targeted_1m$pred)),
    n_forecasts_3m = sum(!is.na(targeted_3m$pred)),
    date_range = range(pcr_data$sasdate),
    avg_predictors_1m = targeted_1m$avg_predictors_used,
    avg_predictors_3m = targeted_3m$avg_predictors_used
  )
)

# Save targeted PCR results
#saveRDS(results_targeted_pcr, "../modelresults/targeted_pcr_results.rds")

# Load PCR results
loaded_targeted_pcr <- readRDS("../modelresults/targeted_pcr_results.rds")

# Access results
targeted_1m <- loaded_targeted_pcr$targeted_1m
targeted_3m <- loaded_targeted_pcr$targeted_3m

cat("TARGETED PCR (1m) RMSE:", targeted_1m$RMSE, " MAE:", targeted_1m$MAE, " | Min PC:", targeted_1m$k_min, "Max PC:", targeted_1m$k_max, "Final PC:", targeted_1m$k_final, "\n", "Average number of raw predictors used:", targeted_1m$avg_predictors_used, "\n")
cat("TARGETED PCR (3m) RMSE:", targeted_3m$RMSE, " MAE:", targeted_3m$MAE, " | Min PC:", targeted_3m$k_min, "Max PC:", targeted_3m$k_max, "Final PC:", targeted_3m$k_final, "\n", "Average number of raw predictors used:", targeted_1m$avg_predictors_used, "\n")

### --- Plot Predicted vs Actual --- ###
# Helper function, same as the one in pcr.R
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

# plot
plot_pcr_forecasts(targeted_1m, "1-Month Inflation Forecast (Targeted PCR)", "Targeted PCR")
plot_pcr_forecasts(targeted_3m, "3-Month Inflation Forecast (Targeted PCR)", "Targeted PCR")
