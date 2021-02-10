#Logistic_Regression
library(caTools)
library(class)
library(caret)
library(psych)

#Retreving the dataset from the location
setwd("C:/Users/bojav/Downloads/ML")
data<- read.csv("knnlog.csv",stringsAsFactors = FALSE,header = TRUE, sep = ",")
View(data)

str(data)

data$Side <- as.factor(data$Side)

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

set.seed(1234)
indx <- sample(2,nrow(data), replace=T, prob=c(0.7,0.3))
log_training <- data[indx ==1,]
log_test <- data[indx == 2,]


#Fitting the model
log_model <- glm(Side~., log_training,family='binomial')
summary(log_model)
vif(log_model)


#Confusion Matrix
p1<- predict(log_model,log_training, type='response')
pred1 <- ifelse(p1>0.5,1,0)
tab<- table(predicted= pred1 ,Actual=log_training$Side)
tab
# Calculating Misclassification Error
1-sum(diag(tab))/sum(tab)

sum(diag(tab))/sum(tab)


#Confusion Matrix
p2<- predict(log_model,log_test, type='response')
pred2 <- ifelse(p2>0.5,1,0)
tab2<- table(predicted= pred2, Actual=log_test$Side)
tab2

1-sum(diag(tab2))/sum(tab2)
sum(diag(tab2))/sum(tab2)
modelChi <- log_model$null.deviance - log_model$deviance
pseudo.R2 <- modelChi / log_model$null.deviance
pseudo.R2


