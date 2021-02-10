#Random Forest Classifier
library(caret)
library(randomForest)

##Retreving the dataset from the location
setwd("C:/Users/bojav/OneDrive/Desktop/DM PROJECT/SET")
data<- read.csv("AB_NYC_2019.csv",header = TRUE, sep = ",")
data

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

#Removing the null values alone from the room type
is.na(data_new$room_type)

#To check the summary of the variable
summary(data_new$room_type)

#Dividing the data for Training and Testing 
data_train <- data_new[1:2000, ]
data_test <- data_new[2001:4000,]

data_train_labels <- data_new[1:2000, 8]
data_test_labels <- data_new[2001:4000, 8]

table(data_train)
table(data_test_labels)

str(data_new)
str(data_train)
prop.table(table(data_train$room_type))
prop.table(table(data_test$room_type))

#Changing the datatype to other datatype
data_new$last_review   <- as.integer(data_new$last_review)
data_new$neighbourhood  <- as.integer(data_new$neighbourhood)
data_new$host_name  <- as.character(data_new$host_name)
data_new$name  <- as.character(data_new$name)
data_new$neighbourhood_group  <- as.integer(data_new$neighbourhood_group)

str(data_new)
data_new


#Cheking for the values in the room type
table(data_new$room_type)

nrow(data_new)

#Eliminates null values in the room type alone
#data_new['room_type']=data_new[!(is.na(data_new) | data_new$room_type==""), ]
#---------------------------------------------------------------------------
#performing the Random Forest Algorithm
rf <- randomForest(room_type ~ . -host_name -name -neighbourhood -last_review, data = data_train)
rf

#Predicting the output
pred <- predict(rf, data_test, type="response")
pred 

data_new$room_type  <- factor(data_new$room_type, labels=c("Private room","Shared room","Entire home/apt"),levels = c(0,1,2))
#-------------------------------------------------------------------------
#Evaluation Methods
confusionMatrix(pred,data_test$room_type)
#---------------------------------------------------
rmse = RMSE(y,data_test$room_type)
rmse

rsquare = (cor(y,data_test$room_type))^2
rsquare
#----------------------------------------------------------------------------
