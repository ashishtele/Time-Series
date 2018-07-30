
rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(anomalize))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(Matrix))
  suppressPackageStartupMessages(require(lubridate))
  suppressPackageStartupMessages(require(forecast))
  suppressPackageStartupMessages(require(tseries))
}

load_lb()

## Import the file

gas <- fread("E:\\Data Mining\\Assignment1\\gas.csv")
glimpse(gas)

# date column change

gas %>% 
  mutate(week = as.Date(week, "%m/%d/%Y")) -> gas

sapply(gas, function(x) sum(is.na(x)))
# no blanks in data

library(ggthemes)
# plot the series
gas %>% 
  drop_na() %>% 
  ggplot(aes(week, price)) + 
  geom_line()+
  theme_economist()+
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    #text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(vjust = 0.4, size = 7),
    axis.ticks.x = element_blank()
  )+
  labs(title = "US Gasoline Price Trend",
       subtitle = "Trend shows upward movement",
       x = "Year",
       y = "Price per barrel")

# converting to ts object

glimpse(gas)
gas_ts <- ts(gas[,"price"])
gas$price_clean <- tsclean(gas_ts)

library(anomalize)

gas %>% 
  as.tibble() %>% 
  time_decompose(price) %>% 
  anomalize(remainder) %>% 
  time_recompose() %>% 
  plot_anomalies(time_recomposed = TRUE, ncol = 3)


# price and clean price check
ggplot(gas) + 
  geom_line(aes(week, price, color = "blue")) +
  geom_line(aes(week, price_clean, color = "red"))

# decompose the time series data
gas_ts <- ts(gas[,"price_clean"])
decom <- stl(gas_ts)
# it says series is not periodic or less than two periods

### stationarity

# augmented Dcikey-fuller (ADF) test:
# H0- the series is non-stationary

adf.test(gas_ts, alternative = "stationary")
## non-stationary

Acf(gas_ts)
Pacf(gas_ts)

# differencing

diff_gas <- diff(gas_ts, differences = 1)
plot(diff_gas)

adf.test(diff_gas, alternative = "stationary")
# stationary

Acf(diff_gas)
# 5
Pacf(diff_gas)
# 1

# model fitting

fit <- auto.arima(gas_ts, seasonal = FALSE)
fit
# evaluate
library(lmtest)
bgtest(fit, 5)

tsdisplay(residuals(fit), lag.max = 45)
# lags at 7

fit1 <- arima(gas_ts, order = c(1,1,5))
fit1
tsdisplay(residuals(fit1), lag.max = 45)

forst <- forecast(fit1, h=30)
plot(forst)

gastbats <- tbats(gas_ts)
fc <- forecast(gastbats, h = 52)
autoplot(fc)
