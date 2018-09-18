# Installing packages

install.packages("zoo")
install.packages("tseries")
install.packages("forecast")
install.packages("sos")
install.packages("MTS")
install.packages("TTR")

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(zoo))
  suppressPackageStartupMessages(require(tseries))
  suppressPackageStartupMessages(require(forecast))
  suppressPackageStartupMessages(require(lubridate))
  suppressPackageStartupMessages(require(sos))
  suppressPackageStartupMessages(require(MTS))
  suppressPackageStartupMessages(require(TTR))
  }

load_lb()

# Local system path in C drive
path = "C:/Users/adity/Desktop/Data Mining/Project"  
setwd(path)

#Loading the data file
df <- read_excel("Retail.xlsx")    

#Taking a glipmse at data file
glimpse(df)

#Converting dates column from character to date
df$Date = as.Date(df$Date)
df_rev = df$Revenue

#Applying missing value and outlier treatment
df_rev_cleaned=tsclean(df_rev)

#Converting series into a time series
Revenue = ts(df_rev_cleaned, start = (decimal_date(as.Date("2010-12-01"))))

#Plotting the time series
plot(Revenue)   # Doesn't seem t exhibit any seasonality and very slight trend 

#Checking for stationarity using Augmented dicky fuller unit root test 
adf.test(Revenue)

#Applying basic models
meanm = meanf(Revenue, h=30)
naivem = naive(Revenue, h=30)
driftm = rwf(Revenue, h=30, drift = T)

#Plotting forecasted values of each model together for model comparison
plot(meanm, plot.conf = F, main = "")
lines(naivem$mean, col=123, lwd=2)
lines(driftm$mean, col = 22, lwd =2)
legend("topleft", lty=1,col=c(4,123,22),
legend=c("Mean Method", "Naive Method", "Drift Method"))


#checking autocorrelation in the dataset
l=length(Revenue)
library(lmtest)
dwtest(Revenue[-l] ~ Revenue[-1])

#checking acf and pacf plots in the dataset
tsdisplay(Revenue, lag.max = 20)   #Seems to have lot of correlation among observations

#Checking seasonal decompostion

Revenue_decomposed = (decompose(Revenue, type="additive"))
plot(Revenue_decomposed)


#EXPONENTIAL SMOOTHING
# Using function ets
etsmodel = ets(Revenue)
etsmodel 
plot(etsmodel)
#There is no trend and seasonality present in the dataset as per automated exponential smoothing

#Forecasting with ets
etsfore = forecast(etsmodel, h=30)

# Plotting the model vs original
plot(Revenue, lwd = 3)
lines(etsmodel$fitted, lwd = 3, col = "red")

# Plotting the forecast
plot(forecast(etsmodel, h = 30)) # ETS is giving a constant forecast as it not capturing any trend and seasonality in the dataset

#ARIMA MODELS
auto.arima(Revenue)
ts_arima = auto.arima(Revenue, stepwise = F, approximation = F)
ts_arima
checkresiduals(ts_arima)

#Forecasting with Arima
arimafore = forecast(ts_arima, h=30)
plot(arimafore)  
plot(arimafore, xlim = c(2370,2408))  
arimafore$mean

#Model comparison
autoplot(Revenue) +
  forecast::autolayer(etsfore$mean, series = 'ETS model') +
  forecast::autolayer(arimafore$mean, series = 'ARIMA model') +
  xlab('year') + ylab('Revenue') + xlim (c(2370,2408)) + 
  guides(colour = guide_legend(title = 'Forecast Method')) +
  theme(legend.position = c(0.3, 0.9))

#Explorinf external regressors
library(ggplot2)
ggplot(df,
       aes(y = Revenue, x =IS_weekend)) + 
  geom_point () +
  aes(colour = IS_weekend)

attach(df)
gg <- ggplot(df, aes(x=seq(1,365), y=Revenue)) + 
  geom_point(aes(col=IS_weekend)) + 
  geom_smooth(method="loess", se=F) + 
    labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

#Adding external regressors to ARIMA model 
regressor = as.matrix(df[3:10])
glimpse(regressor)
View(regressor)
length(regressor)

ts_arima_reg = auto.arima(Revenue, xreg = regressor, stepwise = F, approximation = F)
ts_arima_reg
