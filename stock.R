# Load required libraries
library(tseries)
library(stats)
library(forecast)

# Read the dataset from the CSV file
data <- read.csv("/Users/amolgaikwad/Downloads/NIFTYBANK.csv")
str(data)
class(data)
summary(data)

colnames(data)

# Assuming the column name for closing prices is "Close"
if ("Close" %in% colnames(data)) {
  # Convert the "Close" column to a time series
  data_ts <- ts(data$Close)
  
  # Summary statistics of the time series
  summary(data_ts)
  
  # Plot the time series
  plot(data_ts)
  abline(reg = lm(data_ts ~ time(data_ts)), col = "blue") # line for the mean of time series
} else {
  print("The column 'Close' was not found in the dataset.")
}

SAP=diff(data) # to make the time series stationary in mean
plot(SAP)  

# Assuming your dataset is stored in a variable called 'data'
# Perform the ADF test
adf_test <- adf.test(data)

# Check the p-value of the test
p_value <- adf_test$p.value
if (p_value < 0.05) {
  print("The time series is stationary.")
} else {
  print("The time series is not stationary.")
}

# Plot the ACF and PACF
acf_data <- acf(data, lag.max = 20)  # Adjust lag.max as per your requirement
pacf_data <- pacf(data, lag.max = 20)

# Identify the significant lags in ACF and PACF plots
significant_lags_acf <- which(abs(acf_data$acf) > 2 / sqrt(length(data)))
significant_lags_pacf <- which(abs(pacf_data$acf) > 2 / sqrt(length(data)))

# Determine the order of AR and MA based on the significant lags
order_ar <- max(significant_lags_pacf)
order_ma <- max(significant_lags_acf)

# Determine the best ARIMA model
arima_model <- auto.arima(data)

# Forecast h=10 step ahead
forecasted_values <- forecast(arima_model, h = 10)

# Plot the forecasted values along with the original time series
plot(forecasted_values, main = "Forecasted Values vs. Original Time Series")
lines(data, col = "blue")
legend("topleft", legend = c("Original", "Forecast"), col = c("blue", "red"), lty = c(1, 1))

