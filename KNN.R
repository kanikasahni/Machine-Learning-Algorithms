
###      K-NN      ###

library(ISLR)

library(FNN)

?Caravan

data <-  Caravan

summary(Caravan$Purchase)
table(Caravan$Purchase)


# saving the response variable

purchase <-  Caravan$Purchase


# knn needs to have all IV as numeric as we need to compute the distance between the observations


var(Caravan[,1])
var(Caravan[,2])

# we require aur dataset to scaled or standardised due to high variability among IVs

std_data <-  scale(Caravan[,-86])
var(std_data[,1])
var(std_data[,2])

## variables are on the same scale so we are safe to run KNN
## splitting data into training and testing

set.seed(1)
test <- sample(1:5822, 1000)

train <- -test


training_data <-  std_data[train,]
testing_data <-  std_data[test,]

testing_y <-  purchase[test]
table(testing_y)

training_y <- purchase[train]
table(training_y)



## run knn with k=1
#knn() has randomness in it , so we need to set.seed to see the similar results

set.seed(1)

predicted_y = knn(training_data, testing_data, training_y, k=1)

head(predicted_y)

## compute the MSE

mean(predicted_y != testing_y)


## confusion matrix

table(testing_y, predicted_y)




### run knn for k=3

set.seed(1)

predicted_y = knn(training_data, testing_data, training_y, k=3)

head(predicted_y)

## compute the MSE

mean(predicted_y != testing_y)


## confusion matrix

table(testing_y, predicted_y)



# according to the confusion matrix only two people buying our policies which is not preferable,
# in previous model when our k=1, we were able to have 6 people buy the policy


## iterate for different values of K

MSE = NULL

for(i in 1:15){
  set.seed(1)
  
  predicted_y = knn(training_data, testing_data, training_y, k=i)
  
  MSE[i]= mean(predicted_y != testing_y)
}

### it seems that when k=8, and k=14, we are geting lower MESs. we can use KNN with k=8 because this will be a simpler model

set.seed(1)

predicted_y = knn(training_data, testing_data, training_y, k=8)


## confusion matrix

table(testing_y, predicted_y)

