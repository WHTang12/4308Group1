########################################################
### This file only plots the predicted vs actual for ###
###   1. Every model                                 ###
###   2. Best and worst performing models            ###
########################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

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
ar_1m <- ar_results$ar_1m$pred[1:126]
lasso_1m <- lasso_results$lasso1m$pred[1:126]
targeted_1m <- targeted_results$targeted_1m$pred[1:126]
xgb_1m <- xgb_results$xgb_1m$pred[1:126]
rf_1m <- as.numeric(rf_results$rf_1m$pred)[1:126]
ridge_1m <- ridge_results1m$forecast[1:126]
arrfxgb_1m <- arrfxgb_avg_results$simple_avg_1m[1:126]
rfxgb_1m <- rfxgb_avg_results$rfxgb_avg_1m[1:126]

# Extract 3m forecasts
ar_3m <- ar_results$ar_3m$pred[1:124]
lasso_3m <- lasso_results$lasso3m$pred[1:124]
targeted_3m <- targeted_results$targeted_3m$pred[1:124]
xgb_3m <- xgb_results$xgb_3m$pred[1:124]
rf_3m <- as.numeric(rf_results$rf_3m$pred)[1:124]
ridge_3m <- ridge_results3m$forecast[1:124]
arrfxgb_3m <- arrfxgb_avg_results$simple_avg_3m[1:124]
rfxgb_3m <- rfxgb_avg_results$rfxgb_avg_3m[1:124]

# Extract 6m forecasts
ar_6m <- ar_results$ar_6m$pred[1:121]
lasso_6m <- lasso_results$lasso6m$pred[1:121]
targeted_6m <- targeted_results$targeted_6m$pred[1:121]
xgb_6m <- xgb_results$xgb_6m$pred[1:121]
rf_6m <- as.numeric(rf_results$rf_6m$pred)[1:121]
ridge_6m <- ridge_results6m$forecast[1:121]
arrfxgb_6m <- arrfxgb_avg_results$simple_avg_6m[1:121]
rfxgb_6m <- rfxgb_avg_results$rfxgb_avg_6m[1:121]

# Extract 12m forecasts
ar_12m <- ar_results$ar_12m$pred[1:115]
lasso_12m <- lasso_results$lasso12m$pred[1:115]
targeted_12m <- targeted_results$targeted_12m$pred[1:115]
xgb_12m <- xgb_results$xgb_12m$pred[1:115]
rf_12m <- as.numeric(rf_results$rf_12m$pred)[1:115]
ridge_12m <- ridge_results12m$forecast[1:115]
arrfxgb_12m <- arrfxgb_avg_results$simple_avg_12m[1:115]
rfxgb_12m <- rfxgb_avg_results$rfxgb_avg_12m[1:115]

# Extract actual values (basically i just choose any model's actual values, since it's same for all)
actual_1m <- pcr_results$pcr_static_1m$actual[1:126]
actual_3m <- pcr_results$pcr_static_3m$actual[1:124]
actual_6m <- pcr_results$pcr_static_6m$actual[1:121]
actual_12m <- pcr_results$pcr_static_12m$actual[1:115]

# Dates
d1  <- as.Date(ridge_results1m$date)[1:126]
d3  <- as.Date(ridge_results3m$date)[1:124]
d6  <- as.Date(ridge_results6m$date)[1:121]
d12 <- as.Date(ridge_results12m$date)[1:115]

# Helper function
make_plot <- function(df, title_lab) {
  df_long <- df |>
    pivot_longer(-date, names_to = "Series", values_to = "Predicted")
  
  # Add actual values to each model facet
  df_long <- merge(df_long, df |> select(date, Actual), by = "date")
  df_long$Series <- factor(df_long$Series,
                           levels = c("Actual", "AR", "Ridge", "LASSO",
                                      "Targeted PCR", "RF", "XGB",
                                      "RF+XGB Avg", "AR+RF+XGB Avg")
  )
  
  ggplot(df_long, aes(x = date)) +
    # Actual line (dark gray for all facets)
    geom_line(aes(y = Actual), color = "gray60", linewidth = 1) +
    # Model predictions (colored)
    geom_line(aes(y = Predicted, color = Series), linewidth = 1) +
    facet_wrap(~Series, ncol = 3, scales = "fixed") +
    labs(title = title_lab,
         x = "Date", y = "Inflation rate (%)") +
    scale_color_manual(values = c(
      "AR" = "orange", "LASSO" = "#1f77b4",
      "Targeted PCR" = "#2ca02c", "XGB" = "#d62728",
      "RF" = "#9467bd", "Ridge" = "#8c564b",
      "AR+RF+XGB Avg" = "#e377c2", "RF+XGB Avg" = "#17becf"
    )) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "none",
      strip.text = element_text(face = "bold")
    )
}

df3m <- tibble(
  date = d3,
  Actual = actual_3m,
  AR = ar_3m,
  LASSO = lasso_3m,
  `Targeted PCR` = targeted_3m,
  XGB = xgb_3m,
  RF = rf_3m,
  Ridge = ridge_3m,
  `AR+RF+XGB Avg` = arrfxgb_3m,
  `RF+XGB Avg` = rfxgb_3m
)

df6m <- tibble(
  date = d6,
  Actual = actual_6m,
  AR = ar_6m,
  LASSO = lasso_6m,
  `Targeted PCR` = targeted_6m,
  XGB = xgb_6m,
  RF = rf_6m,
  Ridge = ridge_6m,
  `AR+RF+XGB Avg` = arrfxgb_6m,
  `RF+XGB Avg` = rfxgb_6m
)

df12m <- tibble(
  date = d12,
  Actual = actual_12m,
  AR = ar_12m,
  LASSO = lasso_12m,
  `Targeted PCR` = targeted_12m,
  XGB = xgb_12m,
  RF = rf_12m,
  Ridge = ridge_12m,
  `AR+RF+XGB Avg` = arrfxgb_12m,
  `RF+XGB Avg` = rfxgb_12m
)

p1 <- make_plot(df1m,  "1-month ahead")
p3 <- make_plot(df3m,  "3-month ahead")
p6 <- make_plot(df6m,  "6-month ahead")
p12<- make_plot(df12m, "12-month ahead")

# ggsave("../figures/plot_1m.png",  p1,  width = 10, height = 7, dpi = 300)
# ggsave("../figures/plot_3m.png",  p3,  width = 10, height = 7, dpi = 300)
# ggsave("../figures/plot_6m.png",  p6,  width = 10, height = 7, dpi = 300)
# ggsave("../figures/plot_12m.png", p12, width = 10, height = 7, dpi = 300)


### --- Summary plot of predicted vs actual for selected graphs --- ###
df_1m <- data.frame(
    date = d1,
    Actual = as.numeric(actual_1m),
    RF = as.numeric(rf_1m),
    AR = as.numeric(ar_1m),
    Ridge = as.numeric(ridge_1m)
)
  
df_3m <- data.frame(
    date = d3,
    Actual = as.numeric(actual_3m),
    RF = as.numeric(rf_3m),
    AR = as.numeric(ar_3m),
    LASSO = as.numeric(lasso_3m)
)
  
df_6m <- data.frame(
    date = d6,
    Actual = as.numeric(actual_6m),
    RF = as.numeric(rf_6m),
    PCR = as.numeric(targeted_6m),
    LASSO = as.numeric(lasso_6m)
)
  
df_12m <- data.frame(
    date = d12,
    Actual = as.numeric(actual_12m),
    xgb = as.numeric(xgb_12m),
    PCR = as.numeric(targeted_12m),
    LASSO = as.numeric(lasso_12m)
)

legend_top_right <- theme(
  legend.position = c(0.98, 0.02),
  legend.justification = c(1, 0),
  legend.title = element_blank(),
  legend.background = element_rect(fill = "white", color = "grey80"),
  legend.key = element_blank(),
  legend.text = element_text(size = 16)
)

col_map <- c(
  "Actual" = "gray60",
  "RF"     = "#1e7d1e", 
  "Ridge"  = "#e15759",
  "LASSO"  = "#e15759",
  "PCR"    = "#e15759",
  "XGBoost"    = "#1e7d1e"
)

# ============ 1m ============ #
p_1m <- ggplot(df_1m, aes(x = date)) +
  geom_line(aes(y = Ridge, color = "Ridge"), linewidth = 1) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = RF, color = "RF"), linewidth = 1) +
  labs(title = "1-month ahead", x = "Date", y = "Core Inflation rate (%)") +
  scale_color_manual(values = col_map,
                     breaks = c("Actual", "RF", "Ridge")) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits = c(min(df_1m$date), max(df_1m$date))
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(size = 20),
        axis.text  = element_text(size = 16)) +
  legend_top_right

# ============ 3m ============ #
p_3m <- ggplot(df_3m, aes(x = date)) +
  geom_line(aes(y = LASSO, color = "LASSO"), linewidth = 1) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = RF, color = "RF"), linewidth = 1) +
  labs(title = "3-month ahead", x = "Date", y = "Core Inflation rate (%)") +
  scale_color_manual(values = col_map,
                     breaks = c("Actual", "RF", "LASSO")) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits = c(min(df_3m$date), max(df_3m$date))
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(size = 20),
        axis.text  = element_text(size = 16)) +
  legend_top_right

# ============ 6m ============ #
p_6m <- ggplot(df_6m, aes(x = date)) +
  geom_line(aes(y = PCR, color = "PCR"), linewidth = 1) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = RF, color = "RF"), linewidth = 1) +
  labs(title = "6-month ahead", x = "Date", y = "Core Inflation rate (%)") +
  scale_color_manual(values = col_map,
                     breaks = c("Actual", "RF", "PCR")) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits = c(min(df_3m$date), max(df_3m$date))
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(size = 20),
        axis.text  = element_text(size = 16)) +
  legend_top_right

# ============ 12m ============ #
p_12m <- ggplot(df_12m, aes(x = date)) +
  geom_line(aes(y = PCR, color = "PCR"),    linewidth = 1) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = xgb, color = "XGBoost"),     linewidth = 1) +
  labs(title = "12-month ahead", x = "Date", y = "Core Inflation rate (%)") +
  scale_color_manual(values = col_map,
                     breaks = c("Actual", "XGBoost", "PCR")) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits = c(min(df_3m$date), max(df_3m$date))
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", size = 18),
        axis.title = element_text(size = 20),
        axis.text  = element_text(size = 16)) +
  legend_top_right

# ============ arrange 2×2, share legend at bottom ============ #
(p_1m | p_3m) / (p_6m | p_12m)

best_worst_pva <- (p_1m | p_3m) / (p_6m | p_12m)

#ggsave(
#  filename = "../figures/best_worst_predictedvsactual.png",  # or .pdf, .tiff, etc.
#  plot = best_worst_pva,
#  width = 14,      # adjust width/height for your layout
#  height = 10,
#  dpi = 300        # print-quality resolution
#)
