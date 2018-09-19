#packages
library(dplyr)
library(randomForest)
library(caret)
library("reshape2")
library("ggplot2")
library(tidyr)

#data
Retail<-readxl::read_excel("C:/Users/Dell/Desktop/Retail.xlsx")
glimpse(Retail)
Retail[,8:15]<-lapply(Retail[,8:15],factor)

#partitioning data
training<-Retail[Retail$Date<=as.Date("2011-11-30","%Y-%m-%d"),]
testing<-Retail[Retail$Date>as.Date("2011-11-30","%Y-%m-%d"),]

#Random forest baseline
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(123)
mtry <- 4
tunegrid <- expand.grid(.mtry=mtry)
rf_baseline <- train(Revenue~., data=training, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_baseline)

#random search
control <- trainControl(method="repeatedcv", number=10, repeats=3,search ="random")
set.seed(123)
mtry <- 4
rf_random <- train(Revenue~., data=training, method="rf", tunelength=15, trControl=control)
print(rf_random)
plot(rf_random)

#grid search
control <- trainControl(method="repeatedcv", number=10, repeats=3,search ="grid")
set.seed(123)
mtry <- 4
tunegrid <- expand.grid(.mtry=c(1:15))
rf_grid <- train(training[,-c(2,3)],as.vector(as.matrix(training[,2])),data=training, method="rf", tuneGrid=tunegrid, trControl=control,importance=TRUE)
print(rf_grid)
plot(rf_grid)

plot(varImp(rf_grid),main="Random forest")


#predict on test
predictions<-predict.train(object=rf_grid,testing[,-2],type="raw")

RMSE(predictions,testing[,2])
rf_grid$modelInfo
#14546.96


x<-data.frame(Retail[,1:2],predict.train(object=rf_grid,Retail[,-2],type="raw"))
names(x)<-c("Date","Actual","predictions")


x<-gather(x,"Type","Revenue",2:3)

ggplot(data=x,
       aes(x=Date, y=Revenue, colour=Type)) +
  geom_line()+ggtitle("Actual vs Predicted - RF")


#Linear regression

ctrl<-trainControl(method = "cv",number = 10)
lmfit<-train(Revenue~., data=training[,-c(3,4,9,10)], method="lm", trControl=ctrl)
summary(lmfit)

predictions_lm<-predict.train(object=lmfit,testing[,-2],type="raw")
RMSE(predictions_lm,testing[,2])
#13033.77
#12763.26
varImp(lmfit)
plot(varImp(lmfit),main="Linear regression")

y<-data.frame(Retail[,1:2],predict.train(object=lmfit,Retail[,-2],type="raw"))
names(y)<-c("Date","Actual","predictions")


y<-gather(y,"Type","Revenue",2:3)

ggplot(data=y,
       aes(x=Date, y=Revenue, colour=Type)) +
  geom_line()+ggtitle("Actual vs Predicted - Linear")


#xgboost baseline
trctrl <- trainControl(method = "cv", number = 5)
tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)
xgb_fit<-train(Revenue~., data=training[,-3], method = "xgbTree",
               trControl=trctrl,
               tuneGrid = tune_grid,
               tuneLength = 10)

xgb_predict <- predict(xgb_fit,testing[,-2])

RMSE(xgb_predict,testing[,2])
varImp(xgb_fit)
#13373.6

#xgboost grid
trctrl <- trainControl(method = "cv", number = 5)
tune_grid <- expand.grid(nrounds=c(100,200,300,400), 
                         max_depth = c(3:7),
                         eta = c(0.05, 1),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0))
                         
xgb_fit1<-train(Revenue~., data=training[,-3], method = "xgbTree",
               trControl=trctrl,
               tuneGrid = tune_grid,
               tuneLength = 10)
xgb_predict1 <- predict(xgb_fit1,testing[,-2])

RMSE(xgb_predict1,testing[,2])
varImp(xgb_fit1)
plot(xgb_fit1)
#13210.13

z<-data.frame(Retail[,1:2],predict.train(object=xgb_fit1,Retail[,-2],type="raw"))
names(z)<-c("Date","Actual","predictions")


z<-gather(z,"Type","Revenue",2:3)

ggplot(data=z,
       aes(x=Date, y=Revenue, colour=Type)) +
  geom_line()+ggtitle("Actual vs Predicted - Xgb")
plot(varImp(xgb_fit1),main="XGboost")



#last try
trctrl <- trainControl(method = "cv", number = 5)
tune_grid <- expand.grid(nrounds=c(25,50,75,100,125,150,175,200,300,400), 
                         max_depth = c(1:7),
                         eta = c(0.05,0.1,0.15,0.2),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0))

xgb_fit2<-train(Revenue~., data=training[,-3], method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)
plot(xgb_fit2)
xgb_predict2 <- predict(xgb_fit2,testing[,-2])
RMSE(xgb_predict2,testing[,2])


#Ensemble
RMSE((predictions_lm*2+xgb_predict1)/3,testing[,2])
#12855.33
#12754.59
#12704.9

p<-data.frame(Retail[,1:2],((predict.train(object=lmfit,Retail[,-2],type="raw"))*2+predict.train(object=xgb_fit1,Retail[,-2],type="raw"))/3)
names(p)<-c("Date","Actual","predictions")


p<-gather(p,"Type","Revenue",2:3)

ggplot(data=p,
       aes(x=Date, y=Revenue, colour=Type)) +
  geom_line()+ggtitle("Actual vs Predicted - Ensemble")


q<-data.frame(testing[,1:2],(predictions_lm*2+xgb_predict1)/3)
names(q)<-c("Date","Actual","predictions")


q<-gather(q,"Type","Revenue",2:3)

ggplot(data=q,
       aes(x=Date, y=Revenue, colour=Type)) +
  geom_line()+ggtitle("Actual vs Predicted - Ensemble")

