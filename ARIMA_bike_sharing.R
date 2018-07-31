
rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(tree))
  suppressPackageStartupMessages(library(MASS))
  suppressPackageStartupMessages(library(mice))
  suppressPackageStartupMessages(require(xgboost))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(Matrix))
  suppressPackageStartupMessages(require(lubridate))
  suppressPackageStartupMessages(require(forecast))
  suppressPackageStartupMessages(require(tseries))
}

load_lb()

library(tidyverse)
library(anomalize)



# Importing the data file

day_df <- fread("E:\\Study\\R Projects\\Common files\\day.csv")
glimpse(day_df)
# 731 X 16

# Examine the data
day_df %>% 
  mutate(date = date(dteday)) -> day_df
day_df$dteday <- NULL

sapply(day_df, function(x) sum(is.na(x)))

# plot the series

ggplot(day_df, aes(date, cnt, color = "red")) +
  geom_line(show.legend = FALSE) +
  labs(x = "month", y = "checkout count (daily)")

# ts object for outlier detection in 'cnt'

cnt_ts <- ts(day_df[,"cnt"])
day_df$cnt_clean <- tsclean(cnt_ts)

day_df %>% 
  as.tibble() %>% 
  time_decompose(cnt) %>% 
  anomalize(remainder) %>% 
  time_recompose() %>% 
  plot_anomalies(time_recomposed = TRUE, ncol = 3)

ggplot(day_df) +
  geom_line(aes(date, cnt_clean, color = "red"), size = 2, show.legend = FALSE) +
  geom_line(aes(x = date, y = cnt, color = "blue"), show.legend = FALSE) +
  labs(x = "month", y = "checkout count (daily): cleaned")

# A few outliers seem to be removed in new column. A drop to 0 count and then
# rebouncing to high count looks like a flaw/outlier

# smoothening data series using 'moving average'

day_df$cnt_ma7 <- ma(day_df$cnt_clean, order = 7)
day_df$cnt_ma30 <- ma(day_df$cnt_clean, order = 30)

ggplot(day_df)+
  geom_line(aes(date, cnt_clean, color = "count")) +
  geom_line(aes(date, cnt_ma7, color = "weekly")) +
  geom_line(aes(date, cnt_ma30, color = "monthly"))+
  labs(x = "month", y = "count")

# Decompose the time series data

cnt_ma_7 <- ts(na.omit(day_df$cnt_ma7), frequency = 30)      # no.of obs per period: 30 obs per month
decomp <- stl(cnt_ma_7, s.window = "periodic")
deseason <- seasadj(decomp)                  # subtracting the seasonal component from original series
plot(decomp)


# stationarity

# augmented Dcikey-fuller (ADF) test: 
# H0- the series is non-stationary

adf.test(cnt_ma_7, alternative = "stationary")
# non-stationary series

# Autocorrelations and choosing model order

Acf(cnt_ma_7)
Pacf(cnt_ma_7)

# differencing

count_d1 <- diff(deseason, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
# stationary series

Acf(count_d1)
Pacf(count_d1)
# 1,2,7 lags


# Model fitting

fit <- auto.arima(deseason, seasonal = FALSE)

# evaluate

tsdisplay(residuals(fit), lag.max = 45)
# lags at 7

fit1 <- arima(deseason, order = c(1,1,7))
tsdisplay(residuals(fit1), lag.max = 45)


forst <- forecast(fit1, h=30)
plot(forst)

fit_seaconal <- auto.arima(deseason, seasonal = TRUE)
fit_seaconal

seas_fcast <- forecast(fit_seaconal, h = 30)
plot(seas_fcast)


############################# Efficient Market Hypothesis ###########################
google <- fpp2::goog
glimpse(google)

g_diff <- diff(google)

autoplot(google)
autoplot(g_diff)

# Check the ACF 
ggAcf(g_diff)
## A couple of spikes are crossing the threshold lines. Check for "Lj" test.

# Ljung-Box test
Box.test(g_diff, lag = 10, type = "Lj")
## p-value is more than 0.05, so we fail to reject the null hypothesis the daily changes are not significantly 
## different than white noise


