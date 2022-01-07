
rm(list=ls(all=TRUE))
gc(reset=TRUE)



setwd("./")


#####################
## Preprocess Data ##
#####################

#### Load training and testing data produced by 'preprocess_data.R'
load('D:/project/Mini_project/data/training_testing_data.RData')



plot(dfTrain$Size,dfTrain$Weekly_Sales, main="Weekly sales vs store size", xlab="dfTrain$size", ylab="dfTrain$weekly_sales")

plot(dfTrain$Temperature,dfTrain$Weekly_Sales, main="Weekly sales vs Temperature", xlab="dfTrain$temp", ylab="dfTrain$weekly_sales")

plot(dfTrain$IsHoliday,dfTrain$Weekly_Sales, main="Weekly sales vs holiday", xlab="dfTrain$holiday", ylab="dfTrain$weekly_sales")

plot(dfTrain$Year,dfTrain$Weekly_Sales, main="Weekly sales vs yearly ", xlab="dfTrain$year", ylab="dfTrain$weekly_sales")

plot(dfTrain$Month,dfTrain$Weekly_Sales, main="Weekly sales vs monthly", xlab="dfTrain$month", ylab="dfTrain$weekly_sales")

plot(dfTrain$Day,dfTrain$Weekly_Sales, main="Weekly sales vs daywise", xlab="dfTrain$days", ylab="dfTrain$weekly_sales")

plot(dfTrain$Store,dfTrain$Weekly_Sales, main="Weekly sales vs stores", xlab="dfTrain$store", ylab="dfTrain$weekly_sales")

plot(dfTrain$Dept,dfTrain$Weekly_Sales, main="Weekly sales vs Dept", xlab="dfTrain$dept", ylab="dfTrain$weekly_sales")


set.seed(123)
library(caTools)
split = sample.split(dfTrain$Weekly_Sales, SplitRatio = 2/3)
train_weekly = subset(dfTrain, split == TRUE)
test_weekly = subset(dfTrain, split == FALSE)
str(test_weekly$Weekly_Sales)
str(dfTrain)
str(train_weekly)
str(test_weekly)

relation <- lm(Weekly_Sales~IsHoliday+Temperature+Dept+Store+Holiday+Day_Index+Year+Month+Day,data=train_weekly)
summary(relation)

week_sales_predict=predict(relation, newdata=test_weekly)
str(week_sales_predict)
View(week_sales_predict)

week_sales_predict=as.data.frame(week_sales_predict)
str(week_sales_predict)

round(week_sales_predict$week_sales_predict)
#View(rage)
table(week_sales_predict$week_sales_predict,test_weekly$Weekly_Sales)

summary(relation)
relation
plot(test_weekly$Dept,test_weekly$Weekly_Sales)
lines(test_weekly$Dept, week_sales_predict$week_sales_predict,type='l',col='blue')

# Function for Root Mean Squared Error
RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(relation$residuals)


# If you want, say, MAE, you can do the following:

# Function for Mean Absolute Error
mae <- function(error) { mean(abs(error)) }
mae(relation$residuals)

