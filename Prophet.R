path = "E:\\Data Mining\\Project"  
setwd(path)

#Loading the data file

df <- read_excel("spline.xlsx")    
glimpse(df)

library(tidyverse)
library(tidyquant)
library(prophet)

# splitting dataset for train and test
retail_p_day <- df %>%
  mutate(day = as.Date(Date)) %>% 
  rename(Revenue = x) %>% 
  mutate(model = ifelse(day <= "2011-12-01", "train", "test"))


train <- filter(retail_p_day, model == "train") %>%
  select(day, Revenue) %>%
  rename(ds = day,
         y = Revenue)

test <- filter(retail_p_day, model == "test") %>%
  select(day, Revenue) %>%
  rename(ds = day)

## adding holiday data
off_days <- data.frame(ds = as.Date(c("2010-12-24", "2010-12-25", "2010-12-26", "2010-12-27", "2010-12-28", 
                                      "2010-12-29", "2010-12-30", "2010-01-01", "2010-01-02", "2010-01-03",
                                      "2011-04-22", "2011-04-23", "2011-04-24", "2011-04-25", "2011-05-02", 
                                      "2011-05-30", "2011-08-29", "2011-04-29", "2011-04-30"))) %>%
  mutate(holiday = paste0("off_day_", seq_along(1:length(ds))))

prophet_model_test <- prophet(train, 
                              growth = "linear", 
                              n.changepoints = 100,
                              yearly.seasonality = FALSE, 
                              weekly.seasonality = TRUE, 
                              holidays = off_days)

prophet_model_test

# predicting on test data
forecast_test <- predict(prophet_model_test, test)

test$yhat <- forecast_test$yhat


# plotting the curve
test %>%
  mutate(resid = Revenue - yhat) %>%
  ggplot(aes(x = ds, y = resid)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_smooth() +
  theme_tq()


test %>%
  gather(x, y, Revenue, yhat) %>%
  ggplot(aes(x = ds, y = y, color = x)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = palette_light()) +
  theme_tq()+
  labs(x = "Date", y = "Revenue")

future <- make_future_dataframe(prophet_model_test, periods = 30)
forecast <- predict(prophet_model_test, future)
plot(prophet_model_test, forecast) +
  theme_tq()+
  labs(x = "Date", y = "Revenue")

library(yardstick)

# calculating the RMSE
rmse(test, Revenue, yhat)


