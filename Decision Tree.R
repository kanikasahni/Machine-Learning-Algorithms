## Classification and Regression trees
library(tree)
library(MASS)
# we will use the boston dataset to predict the mdev of the house
# split training and testing

set.seed(1)
train= sample(1:nrow(Boston), 400)
test = -train


training_data = Boston[train,]
testing_data = Boston[test,]

# train using training dataset
model = tree(medv~. , data = training_data)
model


plot(model)
text(model)


## check if the tree needs pruning
validated_tree = cv.tree(model) # cross validation

plot(validated_tree$size,
     validated_tree$dev,
     type = "b",
     ylab = "RSS",
     xlab = "Size of the Tree")

## according to the plot the size of the tree should be 9, so no need to prune the tree.

## assess the model and find MSE

predicted_y= predict(model, testing_data)
testing_y = Boston$medv[test]

MSE = mean((predicted_y - testing_y)^2)
MSE


## let's assume that we need to prune our tree to size =7 even though we don't need to prune it,
## this is just to show how to proceed, in case we needed to prune

pruned_tree = prune.tree(model, best=7)
plot(pruned_tree)
text(pruned_tree)


predicted_y= predict(pruned_tree, testing_data)
testing_y = Boston$medv[test]

MSE = mean((predicted_y - testing_y)^2)
MSE


## MSE is 13.04, 
## which is more compared to when we didn't pruned the tree, this proves pruning the tree won't work


### CLASSIFICATION TREE

library(ISLR)
library(tree)

# use carseats datasets

attach(Carseats)

# the sales variable is numeric . we will categorixe to see if the dales are high or low
# high(sales>=8) ; low(sales<8)

range(Carseats$Sales)


data <-  Carseats

data$High = ifelse(data$Sales >= 8,
                   "yes", "no")

### split the data

set.seed(1)
train= sample(1:nrow(data), 200)

test = -train

training_data = data[train,]
testing_data = data[test,]

# fit decision tree model for classification

model = tree(as.factor(High)~. - Sales, training_data)

plot(model)
text(model)

text(model, pretty = 0)


# check if we need to prune the tree

set.seed(1)
validated_tree = cv.tree(model, 
                         FUN = prune.misclass)  ## this is for  telling classification model


which.min(validated_tree$dev)
validated_tree$size

# since the validated tree is of size 20, no need to prune the tree

##assess the model

predicted_prob = predict(model, testing_data)
predicted_y = rep("yes", 200)
predicted_y[predicted_prob <0.5] = "no"

testing_y = data$High[test]

mean((predicted_y- testing_y)^2)
