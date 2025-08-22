# ---- Clear environment ----
rm(list = ls())

# ---- Libraries ----
library(tidyverse)
library(forecast)

# ---- Paths ----
data_path <- "/Users/caliboi/Desktop/Resumes/Github/Project 2/airline_revenue.csv"
base_dir  <- dirname(data_path)                       # same folder as the CSV
out_dir   <- file.path(base_dir, "dashboards")        # dashboards subfolder
dir.create(out_dir, showWarnings = FALSE)             # create if missing

# ---- Load data ----
df <- read.csv(data_path)
cat("✅ Data loaded. Rows:", nrow(df), "\n")

# ---- Convert to time series ----
df$quarter <- as.Date(df$quarter)
ts_data <- df %>%
  group_by(quarter) %>%
  summarise(revenue = sum(revenue_million), .groups = "drop") %>%
  arrange(quarter)

cat("✅ Time series data prepared. Quarters:", nrow(ts_data), "\n")

# ---- Time series object ----
ts_series <- ts(ts_data$revenue, frequency = 4)  # quarterly data

# ---- Forecast ----
model <- auto.arima(ts_series)
fc <- forecast(model, h = 4)
cat("✅ Forecast complete. Next 4 quarters:\n")
print(fc)

# ---- Forecast plot ----
plot(fc, main = "Airline Revenue Forecast (Next 4 Quarters)",
     ylab = "Revenue ($M)", xlab = "Quarter")

png(file.path(out_dir, "airline_revenue_forecast.png"), width = 800, height = 500, bg = "white")
plot(fc, main = "Airline Revenue Forecast (Next 4 Quarters)",
     ylab = "Revenue ($M)", xlab = "Quarter")
dev.off()
cat("📊 Saved forecast plot →", file.path(out_dir, "airline_revenue_forecast.png"), "\n")

# ---- Historical trend plot ----
p <- ggplot(df, aes(x = as.Date(quarter), y = revenue_million, color = carrier)) +
  geom_line(size = 1) +
  labs(title = "Quarterly Revenue by Airline Partner",
       x = "Quarter", y = "Revenue ($M)", color = "Carrier") +
  theme_minimal()

print(p)
ggsave(filename = file.path(out_dir, "airline_revenue_trend.png"), plot = p,
       width = 8, height = 5, dpi = 300, bg = "white")
cat("📊 Saved trend plot →", file.path(out_dir, "airline_revenue_trend.png"), "\n")

# ---- Combined actual + forecast plot (with confidence intervals) ----
future_quarters <- seq(max(ts_data$quarter) + 90, by = "quarter", length.out = 4)

fc_df <- data.frame(
  quarter = future_quarters,
  revenue = as.numeric(fc$mean),
  lower   = as.numeric(fc$lower[,2]),   # 95% lower CI
  upper   = as.numeric(fc$upper[,2]),   # 95% upper CI
  type = "Forecast"
)

hist_df <- ts_data %>%
  mutate(type = "Historical") %>%
  rename(revenue = revenue)

combined_df <- bind_rows(hist_df, fc_df)

p2 <- ggplot() +
  geom_line(data = hist_df, aes(x = quarter, y = revenue, color = "Historical"), size = 1) +
  geom_ribbon(data = fc_df, aes(x = quarter, ymin = lower, ymax = upper),
              fill = "grey70", alpha = 0.4) +
  geom_line(data = fc_df, aes(x = quarter, y = revenue, color = "Forecast"), size = 1) +
  geom_point(data = fc_df, aes(x = quarter, y = revenue, color = "Forecast"), size = 2) +
  labs(title = "Airline Revenue: Historical + Forecast",
       x = "Quarter", y = "Revenue ($M)", color = "Legend") +
  theme_minimal()

print(p2)
ggsave(filename = file.path(out_dir, "airline_revenue_combined.png"), plot = p2,
       width = 8, height = 5, dpi = 300, bg = "white")
cat("📊 Saved combined plot →", file.path(out_dir, "airline_revenue_combined.png"), "\n")

# ---- Save forecast results as CSV ----
forecast_csv <- data.frame(
  Quarter = format(future_quarters, "%Y-Q%q"),
  Forecast = round(fc$mean, 2),
  Lower95 = round(fc$lower[,2], 2),
  Upper95 = round(fc$upper[,2], 2)
)
write.csv(forecast_csv, file.path(out_dir, "forecast_results.csv"), row.names = FALSE)
cat("📊 Saved forecast results →", file.path(out_dir, "forecast_results.csv"), "\n")

# ---- Summary Report ----
acc <- accuracy(fc)

summary_txt <- paste0(
  "Airline Revenue Forecast Summary\n",
  "================================\n",
  "Rows in dataset: ", nrow(df), "\n",
  "Quarters prepared: ", nrow(ts_data), "\n",
  "ARIMA Model: ", capture.output(model)[1], "\n\n",
  "Forecast Accuracy (in-sample):\n",
  "  RMSE: ", round(acc[1,'RMSE'], 2), "\n",
  "  MAE : ", round(acc[1,'MAE'], 2), "\n",
  "  MAPE: ", round(acc[1,'MAPE'], 2), "%\n\n",
  "Next 4 Quarter Forecasts:\n",
  paste0(forecast_csv$Quarter, ": ", forecast_csv$Forecast,
         " (95% CI: ", forecast_csv$Lower95, " - ", forecast_csv$Upper95, ")\n",
         collapse = "")
)

writeLines(summary_txt, file.path(out_dir, "forecast_summary.txt"))
cat("📄 Saved summary report →", file.path(out_dir, "forecast_summary.txt"), "\n")

cat("✅ All plots and reports saved in:", out_dir, "\n")
