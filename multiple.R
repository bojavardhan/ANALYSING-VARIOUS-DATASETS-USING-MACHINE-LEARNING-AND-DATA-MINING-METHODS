#Multiple Regression
library(caret)
library(rpart)
library(party)
library(tree)
library(class)
library(car)

#Retreving the dataset from the location
setwd("C:/Users/bojav/OneDrive/Desktop/DM PROJECT/SET")
data<- read.csv("kc_house_data.csv",header = TRUE, sep = ",")
data

str(data)

nrow(data)

#Identify NA values
is.na(data)

#Remove NA
data1<-na.omit(data)

#row count
n = nrow(data1)

#Random sample taken from data
indexes = sample(n,n*(31/100))

#Sub set with approx 12k rows
data_new = data1[indexes,]
data_new

names(data_new)

#Training and Testing the Model
data_train <- data_new[1:2000,]
data_test <- data_new[2001 : 4000,]

#Performing Linear Regression Algorithm
lm.fit <- lm(price ~ bedrooms+floors+sqft_living+sqft_lot+condition+grade+yr_built,data=data_new)
lm_pred <- predict(lm.fit,data_test)
vif(lm.fit)

str(data_new)

summary(lm.fit)

plot(lm.fit)

cor(price, lm.fit, method="pearson")

plot(lm_pred, type = "l", lty = 1.8, col = "blue")
#----------------------------------------------------------------------------
#Evaluation Methods
rmse = RMSE(lm_pred,data_test$price)
rmse

rsquare = (cor(lm_pred,data_test$price))^2
rsquare
