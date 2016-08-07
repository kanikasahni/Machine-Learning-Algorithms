####################### DBSCAN #########################
library(dbscan)
View(iris)
summary(iris)
data(iris)
iris <- as.matrix(iris[,1:4])
res <- dbscan(iris,eps=0.2,minPts = 4)
# 0 is noise, 1 has 47 points ...
res

res$cluster
res$eps
res$minPts


###################################
#example, protein data set
##################################

protein <- read.csv(file.choose(),row.names = 1 )
View(protein)
pro <- as.matrix(protein[,1:4])
res <- dbscan(protein,eps=9.2,minPts = 3)
res
res$cluster
summary(res)
table(res$cluster)
res$cluster


cbind(protein,res$cluster)