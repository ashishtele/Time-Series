rm(list = ls())

# Loading the 
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
  require(forecast)
}

load_lb()

# Install the required packages

if(!require(forecast)) {install.packages("forecast")}
  require(forecast)

# Load the data

df <- read_excel("E:\\Study\\R Projects\\Common files\\climate_change.xls",
                 col_names = c("Rain","Year","Month","Country","x1","x2"),
                 col_types = "numeric")

glimpse(df)
library(dplyr)

## Last two columns are not required. They are NAs

df %>% 
  dplyr::select(Rain,Year,Month) %>% 
  drop_na() %>% 
  mutate(Year = as.factor(Year),
         Month = as.factor(Month))-> df1
df1

class(df1)                    # data.frame structure

# Converting to time series object

df.ts <- ts(data = df1[,"Rain"],
            start = c(1901,1),
            frequency = 12)
df.ts

# Plot the time-series data

autoplot(df.ts) +
  geom_smooth()

# We can not see any pattern over the years.
# Data is huge. lets use data for smaller time frame

df %>% 
  dplyr::select(Rain,Year,Month) %>% 
  drop_na() %>% 
  filter(Year > 1990) %>% 
  mutate(Year = as.factor(Year),
         Month = as.factor(Month))-> df2
df2

# Converting to time series object

df.ts <- ts(data = df2[,"Rain"],
            start = c(1991,1),
            frequency = 12)
df.ts

# Confirming the frequency
frequency(df.ts)

# Plot the time-series data

autoplot(df.ts) +
  geom_smooth() +
  geom_hline(aes(yintercept = mean(df.ts)), color = "red") +
  labs(title = "Yearly rainfall trend in India (1991-2015)",
       x = "year",
       y = NULL)
## The years (2000 - 2005) have seen lower rainfall compared to average rainfall from 1991-2015
## The average rainfall is more than average from 2008 onwards.

summary(df.ts)

df2 %>% 
  group_by(Year) %>% 
  summarise(mean_fall = mean(Rain)) %>% 
  ggplot(aes(x = Year, y = mean_fall, fill = mean_fall < mean(mean_fall))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_hline(aes(yintercept = mean(mean_fall)))+
  labs(title = "Yearly rainfall trend in India (1991-2015)",
       x = "year",
       y = NULL)
## The trend is clear when we plot the data on bar chart.
## 2009 and 2014 have seen below average rainfall.


library(plotly)
# Seasonal plot - Horizontal

ggseasonplot(df.ts,
             year.labels = FALSE,
             continuous = TRUE,
             main = "Monthly trend over the years")

## Main rainfall period: Jun to Sep
## Though not very clear from the graph, recent years (lighter lines)
## show better rainfall comapred to early years (darker lines)

# seasonal plot - Polar

ggseasonplot(df.ts,
             year.labels = FALSE,
             continuous = TRUE,
             polar = TRUE,
             main = "Monthly trend over the years")
## It seems Oct (post monsoon month) received higher average rainfall when
## compared to May (pre monsoon month)

ggsubseriesplot(df.ts) +
  geom_hline(aes(yintercept = mean(df.ts), color = mean(df.ts)))+
  labs(title = "Seasonal Pattern", y = NULL)

# The hypothesis of Oct receiving higher rainfall compared to May can be 
# visualized using above graph.


## Autocorrelation plots

gglagplot(df.ts)

# we can not see a good pattern out of these graphs, but lag 12 is showing 
# good correlation which is obvious with the frequency = 12.

## ACF plot: test if an individual lag autocorrelation is different than zero

ggAcf(df.ts)

# it is a condensed plot of the autocorrelation for 24 lags.
# greatest correlation at 12 and 24
# ACF is peaking around the seasonal lags (12 X n)
# trend effect is not visible as trends induce positive correlations
# in early lags and strong  trends will result in the more recent
# observations being of closer value to one another.

acf(df.ts, plot = FALSE)
# for 1: 0.890
# for 2: 0.867

## Ljung-Box test: test whether any of a group of autocorrelations 
# of a time series are different from zero (overall randomness)

Box.test(df.ts, lag = 24, type = "Lj")

## reject null hypothesis that if it purely a white noise.
# H0: purely white noise
# H1: not purely white noise, some time series information exists in data

  
df_ts <- ts(df2$Rain, frequency = 12)
df_ts

fit <- stl(df_ts, s.window = "periodic", robust = TRUE)
autoplot(fit)

# Naive method
fit_adj <- seasadj(fit)
autoplot(naive(fit_adj))+
  labs(title = "Naive method (seasonally adjusted")

# seasonal naive 
fcast <- forecast(fit, method = "naive")
autoplot(fcast)
summary(fcast)   #RMSE: 33.45



fit_hw <- hw(df_ts, seasonal = "additive")
summary(fit_hw)     # 22.93
autoplot(fit_hw)

fit_hw <- hw(df_ts, seasonal = "multiplicative")
summary(fit_hw)      # 22.87
autoplot(fit_hw)

fit_hw_damp <- hw(df_ts, seasonal = "multiplicative", damped = TRUE)
summary(fit_hw_damp)      # 22.66
autoplot(fit_hw_damp)

fit_ets <- ets(df_ts, damped = FALSE)
summary(fit_ets)         # 24.15544
autoplot(fit_ets)
autoplot(forecast(fit_ets, h = 24))



## stationarity check

adf.test(df_ts, alternative = "stationary")
# p-value is less than significance level of 0.05 (trend)
# no differencing
Box.test(df_ts, lag = 1, type = "Lj")
# not a white noise


ggAcf(df_ts)  
ggPacf(df_ts)


## make seasonal series stationary
ns <- nsdiffs(df_ts)   # seasonal differece required
if(ns > 0) {
  xstar <- diff(df_ts,lag=frequency(df_ts),differences=ns)
} else {
  xstar <- df_ts
}
nd <- ndiffs(xstar)     # simple differencing required
if(nd > 0) {
  xstar <- diff(xstar,differences=nd)
}
autoplot(xstar)

Box.test(xstar, lag = 1, type = "Lj")
ggAcf(xstar)
ggPacf(xstar)


fit_auto <- auto.arima(df_ts, seasonal = TRUE)
fit_auto
autoplot(forecast(fit_auto, h= 24))
accuracy(fit_auto)   # RMSE: 27.68 and AIC: 2754.33


fit_auto_cmplt <- auto.arima(df_ts, seasonal = TRUE, approximation = FALSE)
fit_auto_cmplt       # AIC: 2724.73 # (0,0,0)(2,1,0)
autoplot(forecast(fit_auto_cmplt, h = 24))

tsdisplay(diff(df_ts, 12))

fit_cmplt <- Arima(df_ts, order = c(0,0,0), seasonal = list(order = c(0,1,1), period=12))
fit_cmplt           # AIC: 2672.61
tsdisplay(residuals(fit_cmplt))

Box.test(fit_cmplt$residuals, lag = 12, type = c("Ljung"), fitdf = 1)
# white noise

getrmse <- function(x,h,...)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train,...)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"])
}

getrmse(df_ts,h=24,order=c(0,0,0),seasonal=c(2,1,0),lambda=0)
getrmse(df_ts,h=24,order=c(0,0,0),seasonal=c(0,1,1),lambda=0)
getrmse(df_ts,h=24,order=c(0,0,0),seasonal=c(1,1,1),lambda=0)
getrmse(df_ts,h=24,order=c(0,0,0),seasonal=c(2,1,1),lambda=0)
getrmse(df_ts,h=24,order=c(0,0,0),seasonal=c(3,1,3),lambda=0)

fit_cmplt_1 <- Arima(df_ts, order = c(0,0,0), seasonal = list(order = c(3,1,3), period=12))
autoplot(forecast(fit_cmplt_1, h=24))
summary(fit_cmplt_1)

