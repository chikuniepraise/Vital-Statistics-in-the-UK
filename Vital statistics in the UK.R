
# libraries
library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(zoo)


# The data
file_path <- "C:/Users/user/Desktop/Assignment Project/Task 3/Vital statistics in the UK.xlsx"
birth_data <- read_excel(file_path, sheet = "Birth", skip = 5)

# Relevant columns and renaming of the columns
birth_data <- birth_data[, c("Year", 
                             "Number of live births: United Kingdom", 
                             "Number of live births: England and Wales")]
colnames(birth_data) <- c("Year", "UK_Births", "EandW_Births")

# Converting data to time series format
uk_births_ts <- ts(birth_data$UK_Births, start = min(birth_data$Year), frequency = 1)
eandw_births_ts <- ts(birth_data$EandW_Births, start = min(birth_data$Year), frequency = 1)


#Visualizing the time series

# Checking for NA values in the birth columns and converting the column to numeric
birth_data$UK_Births <- as.numeric(birth_data$UK_Births)
birth_data$EandW_Births <- as.numeric(birth_data$EandW_Births)

# Checking for and removing any rows with NA values
birth_data <- birth_data[complete.cases(birth_data), ]

# Converting data to time series format after cleaning
uk_births_ts <- ts(birth_data$UK_Births, start = min(birth_data$Year), frequency = 1)
eandw_births_ts <- ts(birth_data$EandW_Births, start = min(birth_data$Year), frequency = 1)

# Plotting the graph
autoplot(uk_births_ts, series="UK Births") +
  autolayer(eandw_births_ts, series="England & Wales Births") +
  ggtitle("Number of Live Births in UK and England & Wales") +
  xlab("Year") + ylab("Number of Births") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green"))




# Decomposing the time series

# Setting up the plotting area to increase size and adjust title font
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2) + 0.1, cex.main = 1.2, cex.lab = 1.1, cex.axis = 1.1)

# Plotting UK births with moving average trend line
uk_births_ma <- rollmean(uk_births_ts, k = 3, align = "center", fill = NA)
plot(uk_births_ts, main = "UK Births with Moving Average Trend", ylab = "Births", xlab = "Year", 
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
lines(uk_births_ma, col = "blue", lwd = 2)

# Plotting England & Wales births with moving average trend line
eandw_births_ma <- rollmean(eandw_births_ts, k = 3, align = "center", fill = NA)
plot(eandw_births_ts, main = "England & Wales Births with Moving Average Trend", ylab = "Births", 
     xlab = "Year", cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2)
lines(eandw_births_ma, col = "green", lwd = 2)



# Fit Time Series Models
# ARIMA MODEL

# Fit ARIMA model to UK births
uk_arima <- auto.arima(uk_births_ts)
summary(uk_arima)

# Fit ARIMA model to England & Wales births
eandw_arima <- auto.arima(eandw_births_ts)
summary(eandw_arima)




# Exponential Smoothing Model
# Fit ETS model to UK births
uk_ets <- ets(uk_births_ts)
summary(uk_ets)

# Fit ETS model to England & Wales births
eandw_ets <- ets(eandw_births_ts)
summary(eandw_ets)


# Forecasting and Evaluating Models
  
  
# Forecast with ARIMA model for UK
uk_arima_forecast <- forecast(uk_arima, h = 5)
autoplot(uk_arima_forecast) + ggtitle("ARIMA Forecast for UK Births")

# Forecast with ETS model for UK
uk_ets_forecast <- forecast(uk_ets, h = 5)
autoplot(uk_ets_forecast) + ggtitle("ETS Forecast for UK Births")

# Forecast with ARIMA model for England & Wales
eandw_arima_forecast <- forecast(eandw_arima, h = 5)
autoplot(eandw_arima_forecast) + ggtitle("ARIMA Forecast for England & Wales Births")

# Forecast with ETS model for England & Wales
eandw_ets_forecast <- forecast(eandw_ets, h = 5)
autoplot(eandw_ets_forecast) + ggtitle("ETS Forecast for England & Wales Births")


# Comparing models' accuracy
accuracy(uk_arima)
accuracy(uk_ets)
accuracy(eandw_arima)
accuracy(eandw_ets)




