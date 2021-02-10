#Decision Tree Classifier
library(caret)
library(rpart)
library(party)
library(tree)
library(class)
library(rattle)

#Retreving the dataset from the location
setwd("C:/Users/bojav/OneDrive/Desktop/DM PROJECT/SET")
data<- read.csv("AB_NYC_2019.csv",header = TRUE, sep = ",")
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

str(data_new)

#plot(data_new)
set.seed(1234)

#Changing the datatype to other datatype
data_new$last_review   <- as.integer(data_new$last_review)
data_new$neighbourhood  <- as.integer(data_new$neighbourhood)
data_new$neighbourhood_group  <- as.integer(data_new$neighbourhood_group)
data_new$host_name  <- as.character(data_new$host_name)
data_new$name  <- as.character(data_new$name)

#Checking the datatype for all the variables
str(data_new)

table(data_new $ room_type)

summary(data_new $ room_type)

#Dividing the data for training and testing
data_train <- data_new[1:9000, ]
data_test <- data_new[9001:11979,]

prop.table(table(data_train$room_type))
prop.table(table(data_test$room_type))

#Performing the Decision Tree Algorithm
tree <- ctree(room_type ~ price+minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365, data = data_new, controls = ctree_control(mincriterion = 0.99, minsplit  =3000))
tree

#Plotting the Tree
plot(tree)

#Predicting the test dataset by using the model
y <- predict(tree,data_test)

#Evaluation Method
confusionMatrix(data_test$room_type, y) 
