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
  )

### --- Define training set: 1985 to 2014 --- ###
train_df <- df1 %>%
  filter(year >= 1985 & year <= 2014)

### --- Model selection using BIC --- ###
ar_bic_select <- function(y_target, x_infl, p_max = 12) {
  
  df_full <- data.frame(y = y_target, x = x_infl)
  df_full <- na.omit(df_full)
  
  n <- nrow(df_full)
  best_bic <- Inf
  best_p <- NA
  results <- data.frame(p = 1:p_max, BIC = NA_real_)
  
  for (p in 1:p_max) {
    # Create lagged predictors of monthly inflation (both 1 and 3month models use monthly inflation)
    x_lags <- embed(df_full$x, p + 1)
    X <- x_lags[, 1:p, drop = FALSE]       
    y <- df_full$y[(p+1):n]      
    
    # --- Brief alignment check (because doing inflation between t and t+3 headache...) ---
    if (p == 3) {
      cat("\n--- Alignment check for p =", p, "---\n")
      cat("First 4 y_target values:\n")
      print(head(y, 4))
      cat("First 6 rows of X (lagged predictors):\n")
      print(head(X, 6))
    }
    
    dfX <- as.data.frame(X)
    colnames(dfX) <- paste0("lag", 1:p)
    
    # Fit model and store BIC
    fit <- lm(y ~ ., data = dfX)
    bic_val <- BIC(fit)
    results$BIC[results$p == p] <- bic_val
    
    if (bic_val < best_bic) {
      best_bic <- bic_val
      best_p <- p
      best_fit <- fit
    }
  }
  
  list(best_p = best_p,
       best_bic = best_bic,
       fit = best_fit,
       bic_table = results)
}

sel_1m <- ar_bic_select(train_df$inflationRate_1m_future,
                        train_df$inflationRate_1m,
                        p_max = 8)

sel_3m <- ar_bic_select(train_df$inflationRate_3m_future,
                        train_df$inflationRate_1m,
                        p_max = 8)
print(sel_1m$best_p) # 6
print(sel_3m$best_p) # 8

### --- Model Evaluation using rolling window --- ###
roll_ar_forecast <- function(y_target, x_infl, p, nprev) {
  df_full <- data.frame(y = y_target, x = x_infl)
  df_full <- na.omit(df_full)
  n <- nrow(df_full)
  
  # create result vectors 
  preds <- rep(NA, nprev)
  actual <- rep(NA, nprev)
  
  for (i in 1:nprev) {
    # Training window
    train_end <- n - nprev + i - 1
    train_start <- max(1, train_end - 359)
    
    # rolling window data
    y_train <- df_full$y[train_start:train_end]
    x_train <- df_full$x[train_start:train_end]
    
    # build lagged matrix
    if (length(x_train) > p) {
      x_lags <- embed(x_train, p + 1)
      
      # aligning the target and predictor matrices
      X <- x_lags[, 1:p, drop = FALSE]
      y <- y_train[(p + 1):length(y_train)]
      
      ### --- Brief alignment check (because doing inflation between t and t+3 headache...) --- ###
      if (i == 120) { # i've checked 2-4, too don't worry.
        print("### --- Variables used for the last observation of the train set for the last iteration --- ###")
        print(X[nrow(X),])
        print("### --- Target used for the last observation of the train set for the last iteration  --- ###")
        print(y[length(y)])
      }
      
      ### --- Fit model --- ###
      fit <- lm(y ~ ., data = as.data.frame(X))
    
      ### --- Next, to perform out-of-sample prediction --- ###
      # First is to obtain the regressor values
      last_lags <- rev(x_train[(length(x_train)-p+1):length(x_train)]) # basically obtain the 2nd latest set of regressors
      x_next    <- df_full$x[train_end + 1] # then obtain latest lag
      last_lags <- c(x_next, last_lags[1:(p-1)]) # then join the latest lag with the 2nd latest latest set of regressors (dropping the oldest one ofc)
      newx <- c(1, last_lags) # include intercept
      
      # Then perform the prediction
      preds[i] <- as.numeric(newx %*% coef(fit))
      actual[i] <- df_full$y[train_end + 1]  # True out-of-sample value
      
      ### --- This is to check that it uses the right set of values as regressors and real value to compare against --- ###
      if (i == 120) { 
        print("Variable Values used for final prediction")
        print(last_lags)
        print("Actual Target for final prediction")
        print(actual[1])
      }
    }
  }

  # compute evaluation metrics
  rmse <- sqrt(mean((actual - preds)^2, na.rm = TRUE))
  mae  <- mean(abs(actual - preds), na.rm = TRUE)

  list(pred = preds, actual = actual, RMSE = rmse, MAE = mae)
}

# Define the full post-1985 data
df_eval <- df1 %>% filter(year >= 1985)

# 1-month-ahead model (AR(6))
result_1m <- roll_ar_forecast(df_eval$inflationRate_1m_future,
                              df_eval$inflationRate_1m,
                              p = 6,
                              nprev = 126)
result_1m$RMSE
result_1m$MAE

cat("Length of preds:", length(result_1m$pred), "\n")
cat("Length of actual:", length(result_1m$actual), "\n")


# 3-month-ahead model (AR(8))
result_3m <- roll_ar_forecast(df_eval$inflationRate_3m_future,
                              df_eval$inflationRate_1m,
                              p = 8,
                              nprev = 124)
result_3m$RMSE
result_3m$MAE

cat("1-month AR(6): RMSE =", result_1m$RMSE, " MAE =", result_1m$MAE, "\n")
cat("3-month-ahead AR(8): RMSE =", result_3m$RMSE, " MAE =", result_3m$MAE, "\n")

plot(result_1m$actual, type = "l", col = "black",
     main = "AR(6) Rolling Forecast (1-month-ahead)", ylab = "Inflation", xlab = "Time")
lines(result_1m$pred, col = "blue")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black","blue"), lty = 1)

plot(result_3m$actual, type = "l", col = "black",
     main = "AR(8) Rolling Forecast (3-month-ahead)", ylab = "Inflation", xlab = "Time")
lines(result_3m$pred, col = "blue")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black","blue"), lty = 1)

### --- Save results for dm test and plot later --- ###
results_ar_models <- list(
  ar_1m = result_1m,
  ar_3m = result_3m
)

#saveRDS(results_ar_models, file = "../modelresults/ar_model_results.rds")
