rm(list = ls())

library(readxl)
library(dplyr)
library(imputeTS)


# input the data
d <- read_excel("E:\\Data Mining\\Project\\Retail_date.xlsx")

glimpse(d)

sum(is.na(d$Revenue))


# imputting the missing values
df1 <- ts(d$Revenue)
df2 <- na.interpolation(df1)
df3 <- na.interpolation(df1, option = "spline")

xlsx::write.xlsx(df3,"E:\\Data Mining\\Project\\spline.xlsx")

d <- read_excel("E:\\Data Mining\\Project\\spline.xlsx")

df3 <- ts(d$x)

library(ggplot2)
autoplot(df3)

# checking the anomaly
library(anomalize)
df %>% 
  mutate(date = as.Date(date)) %>% 
  as_tibble() -> df

df %>% 
  time_decompose(revenue) %>% 
  anomalize(remainder) %>% 
  time_recompose() %>% 
  plot_anomalies()


df3 %>% data.frame() -> df
names(df) <- "revenue"

# hybrid package
install.packages("forecastHybrid")
library(forecastHybrid)

train <- df[1:1080,] %>% data.frame()
test <- df[1081:1121,] %>% data.frame()
names(test) <- "revenue"
names(train) <- "revenue"

tr <- ts(train$revenue)
te <- ts(test$revenue)
  
fit <- hybridModel(tr, weights = "equal")
fc1 <- forecast(fit, h = length(te))
autoplot(fc1)

test$est <- fc1$mean
# 11766.2
rmse(test, revenue, est)

glimpse(test)


# RMSE calculation
library(yardstick)
fit1 <- hybridModel(tr, weights = "insample")
fc2 <- forecast(fit1, h = length(te))
autoplot(fc2)

test$est1 <- fc2$nnetar$mean

rmse(test, revenue, est1)



