########################################## CLUSTERING #########################################
######################### K-MEANS CLUSTERING ################################
food <- read.csv(file.choose())
View(food)

set.seed(2) # To fix the random starting cluster

grpProtein <- kmeans(food[,-1],centers=2)
grpProtein

#Creates a table
table(food$Country, grpProtein$cluster)

# OR.. list of cluster assignment
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])


################################# Hierarchical Clustering
#Protein data set
library(cluster)
food <- read.csv(file.choose(),row.names = 1)
View(food)
##### Euclidean distance, dendogram 
foodagg <- agnes(food,diss = FALSE,metric = "manhattan")
plot(foodagg)

#### Euclidean Distance, single linkage
foodaggsin <- agnes(food,diss = FALSE,metric = "Euclidian",method = "single")
plot(foodaggsin)

#### Euclidean Distance, Average linkage
foodaggsin <- agnes(food,diss = FALSE,metric = "Euclidian",method = "average")
plot(foodaggsin)


###################### example2 #############################

library(ISLR)
View(NCI60)
?NCI60
# dividing the data into labs and data
table(NCI60$labs)
table(NCI60$data)
# Scaling the data
sd.data <- scale(NCI60$data)

par(mfrow=c(1,3)) # To plot three graphs
data.dist <- dist(sd.data)
a = hclust(data.dist,method = "average")

plot(hclust(data.dist,method = "average"),labels=NCI60$labs,main = "Average Linkage",xlab = "",sub = "",ylab = "")
plot(hclust(data.dist,method = "single"),labels=NCI60$labs,main = "Single Linkage",xlab = "",sub = "",ylab = "")
plot(hclust(data.dist,method = "complete"),labels=NCI60$labs,main = "Complete Linkage",xlab = "",sub = "",ylab = "")

rect.hclust(a,k=4)

#nci <- agnes(NCI60$data,diss = FALSE,metric ="Euclidian",method = "single")
#plot(nci)

#nci <- agnes(NCI60$data,diss = FALSE,metric ="Euclidian",method = "average")
#plot(nci)

#nci <- agnes(NCI60$data,diss = FALSE,metric ="Euclidian",method = "complete")
#plot(nci)

################################################### K-means example ##############################


View(NCI60)
?NCI60
# dividing the data into labs and data
table(NCI60$labs)
table(NCI60$data)


set.seed(2) # To fix the random starting cluster

cl <- kmeans(sd.data,centers=2)
cl

#Creates a table
table(NCI60$labs,cl$cluster)

# OR.. list of cluster assignment
o=order(grpProtein$cluster)

