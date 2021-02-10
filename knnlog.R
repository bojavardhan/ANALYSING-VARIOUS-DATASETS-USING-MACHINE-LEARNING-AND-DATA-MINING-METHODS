#KNN Classifier
library(caTools)
library(class)
library(gmodels)
library(caret)
library(randomForest)
library(car)

#Retreving the dataset from the location
setwd("C:/Users/bojav/Downloads/ML")
data<- read.csv("knn.csv",stringsAsFactors = FALSE,header = TRUE, sep = ",")
str(data)

sapply(data,function(x) sum(is.na(x)))
str(data)

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

set.seed(1234)
ind <- sample(2,nrow(data), replace=T, prob=c(0.7,0.3))
training <- data[ind ==1,]
test <- data[ind == 2,]

# randamoly assigns rows to the each fold
trcontrol <- trainControl(method="repeatedcv",number=10,repeats=3)
#trcontrol <- trainControl(method="repeatedcv",number=10,repeats=3,classProbs = TRUE,summaryFunction = twoClassSummary())

set.seed(333)

# by default in outcome is accuracy instead of it we can use roc for that we need to ass class probs and summary in trcontrol
fit <- train(Side~., data=training,tuneLength= 20, method='knn',trControl=trcontrol, preProc=c('center','scale'))


fit 
plot(fit)


pred <-predict(fit,newdata=training)
pred1 <-predict(fit,newdata=test)

#confusion matrix
confusionMatrix(table(pred,training$Side))
confusionMatrix(table(pred1,test$Side))

