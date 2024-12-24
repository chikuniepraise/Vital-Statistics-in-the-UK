# Load libraries
library(zoo)
library(ggplot2)
library(forecast)
library(tseries)
library(readxl)

# Load data
file_path <- "C:/Users/user/Desktop/Assignment Project/Task 3/Vital statistics in the UK.xlsx"
birth_data <- read_excel(file_path, sheet = "Birth", skip = 5)

# Select and rename relevant columns
birth_data <- birth_data[, c("Year", "Number of live births: United Kingdom", "Number of live births: England and Wales")]
colnames(birth_data) <- c("Year", "UK_Births", "EandW_Births")

# Convert data columns to numeric
birth_data$UK_Births <- as.numeric(birth_data$UK_Births)
birth_data$EandW_Births <- as.numeric(birth_data$EandW_Births)

# Remove rows with NA values
birth_data <- birth_data[complete.cases(birth_data), ]

# Convert columns to time series format (annual frequency)
uk_births_ts <- ts(birth_data$UK_Births, start = min(birth_data$Year), frequency = 1)
eandw_births_ts <- ts(birth_data$EandW_Births, start = min(birth_data$Year), frequency = 1)

# Plot 1: Time Series Visualization
autoplot(uk_births_ts, series="UK Births") +
  autolayer(eandw_births_ts, series="England & Wales Births") +
  ggtitle("Number of Live Births in UK and England & Wales") +
  xlab("Year") + ylab("Number of Births") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green"))

# Plot 2: Moving Average for Trend
uk_births_ma <- rollmean(uk_births_ts, k = 3, align = "center", fill = NA)
eandw_births_ma <- rollmean(eandw_births_ts, k = 3, align = "center", fill = NA)

plot(uk_births_ts, main = "UK Births with Moving Average Trend", ylab = "Births", xlab = "Year")
lines(uk_births_ma, col = "blue", lwd = 2)

plot(eandw_births_ts, main = "England & Wales Births with Moving Average Trend", ylab = "Births", xlab = "Year")
lines(eandw_births_ma, col = "green", lwd = 2)

# Stationarity Test (Augmented Dickey-Fuller Test)
adf_test_uk <- adf.test(uk_births_ts)
adf_test_eandw <- adf.test(eandw_births_ts)

# Print ADF test results to console for reference
print("ADF Test for UK Births:")
print(adf_test_uk)
print("ADF Test for England & Wales Births:")
print(adf_test_eandw)

# Model 1: ARIMA Model Forecast
uk_arima <- auto.arima(uk_births_ts)
uk_arima_forecast <- forecast(uk_arima, h = 5)
autoplot(uk_arima_forecast) + ggtitle("ARIMA Forecast for UK Births")

eandw_arima <- auto.arima(eandw_births_ts)
eandw_arima_forecast <- forecast(eandw_arima, h = 5)
autoplot(eandw_arima_forecast) + ggtitle("ARIMA Forecast for England & Wales Births")

# Model 2: ETS Model Forecast
uk_ets <- ets(uk_births_ts)
uk_ets_forecast <- forecast(uk_ets, h = 5)
autoplot(uk_ets_forecast) + ggtitle("ETS Forecast for UK Births")

eandw_ets <- ets(eandw_births_ts)
eandw_ets_forecast <- forecast(eandw_ets, h = 5)
autoplot(eandw_ets_forecast) + ggtitle("ETS Forecast for England & Wales Births")

# Model Comparison: Print Accuracy Metrics
uk_arima_acc <- accuracy(uk_arima_forecast)
uk_ets_acc <- accuracy(uk_ets_forecast)
eandw_arima_acc <- accuracy(eandw_arima_forecast)
eandw_ets_acc <- accuracy(eandw_ets_forecast)

print("UK ARIMA Model Accuracy:")
print(uk_arima_acc)
print("UK ETS Model Accuracy:")
print(uk_ets_acc)
print("England & Wales ARIMA Model Accuracy:")
print(eandw_arima_acc)
print("England & Wales ETS Model Accuracy:")
print(eandw_ets_acc)
