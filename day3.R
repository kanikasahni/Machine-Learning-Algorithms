
## Multinomial Logistic Regression

library(nnet)
data <- read.csv("C:/Users/Kanika/Desktop/Loan approval.csv",header = TRUE)

#Regression model
model <- multinom(Loan.approval~.,data=data)
summary(model)


########### Only salary
model <- multinom(Loan.Approval~Salary,data=data)
summary(model)

############ Only Age
model <- multinom(Loan.Approval~Age,data=data)
summary(model)

library(MASS)
###### MASS
?fgl
View(fgl)
#HomeWork

?iris
View(iris)
desc(iris)
str(iris)
plot(iris)
corrplot()
# homework iris

######################### ggvis

library(ggvis)
iris %>% ggvis(~Sepal.Length,~Sepal.Width,fill=~Species) %>% layer_points()


iris %>% ggvis(~Petal.Length,~Petal.Width,fill=~Species) %>% layer_points()


#######################
library(class)

normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# not normalising the species

iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
summary(iris_norm)

set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.67, 0.33))
table(ind)

iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2,1:4]

iris.trainLabels <- iris[ind==1,5]
iris.testLabels <- iris[ind==2,5]

iris_pred <- knn(train = iris.training, test = iris.test, cl=iris.trainLabels, k=4)
iris_pred

iris.testLabels

table(iris_pred,iris.testLabels)


########################################### fgl data
View(fgl)
# ggplot(data = fgl, mapping=aes(x= Na, y = Mg, col= type)) + geom_point()
?ggvis
