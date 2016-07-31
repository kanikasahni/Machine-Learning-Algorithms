#### RANDOM FOREST AND BAGGING

library(randomForest)

# split the data
set.seed(1)
test <- sample(1:nrow(Boston), 400)

train = -test


training_data <-  Boston[train,]
testing_data <-  Boston[test,]

## fit a bagging model

bag_model = randomForest(medv~.,
                         data = training_data,
                         mtry= 13,
                         importance = T)

## assess the model

predicted_y = predict(bag_model, testing_data)
testing_y = Boston$medv[test]

mean((predicted_y - testing_y)^2)


## fit a random forest
bag_model = randomForest(medv~.,
                         data = training_data,
                         mtry= 4,
                         importance = T)

## assess the model

predicted_y = predict(bag_model, testing_data)
testing_y = Boston$medv[test]

mean((predicted_y - testing_y)^2)



### variable Importance
imp = importance(bag_model)[,1]

barplot(sort(imp))
