
########################## Principal Component Analysis  ##################################
pr <- prcomp(USArrests,scale=TRUE)
pr

dim(USArrests)


biplot(pr, scale=0)
pr$rotation=-pr$rotation
pr$x=-pr$x
biplot(pr,scale=0)

pr$sdev
pr.var <- pr$sdev ^2
pr.var



######################## Example, protein ############################################
######################################################################################

library(psych)
protein = read.csv("C:/Users/Kanika/Downloads/protein.csv",sep=',',header=TRUE,row.names = 1)
apply(protein,2,mean)
apply(protein,2,var)

pca.out= prcomp(protein,scale = TRUE)

biplot(pca.out,scale = 0)
pca.out$rotation
pca.out$rotation = -pca.out$rotation
pca.out$x = -pca.out$x

biplot(pca.out,scale = 0)

pr.var = pca.out$sdev ^ 2
pr.var
pve = pr.var/sum(pr.var)
pve
plot(pve,xlab='Principal Component',ylab = "Proportion of Variance Explained",ylim = c(0,1),type = "b")
plot(cumsum(pve),xlab ="Principal Component",ylab= "Cumulative population of variance explained",ylim = c(0,1),type = "b")
biplot(pca.out,choices = c(1,3),scale = 0)

biplot(pca.out$rotation[,1],scale = 0)
biplot(pca.out$rotation[,c(1,2)],scale=0)

