lastfm <- read.csv(file.choose())
lastfm[1:19,]
View(lastfm)
length(lastfm$user)
lastfm$user <- factor(lastfm$user)
levels(lastfm$user) #15,000 users , these many transactions
levels(lastfm$artist) # 1,004 artists
class(lastfm)


# arules package for association rules
# Computational environment for mining association rules and frequent item sets

library(arules)

# Manipulating data for arules
playlist <- split(x=lastfm[,"artist"],f=lastfm$user) # Split into list
playlist <- lapply(playlist,unique) # Remove artist duplicates
playlist[1:2] # The first 2 listeners (1 & 3) listen to the following bands

playlist <- as(playlist,"transactions")
# view this as a list of transactions
# Transactions is a data class defined in arules

itemFrequency(playlist)
# lists the support of the 1,004 bands 
# no of times band is listed to on the shopping trips of 15,000 users
# computes the rel frequency each artist mentioned by the 15,000 users

itemFrequencyPlot(playlist, support = .08, cex.names = 1.5)
# Plots the item frequencies (only bands with frequency > % support)

## Finally, we build the association rules
## Only rules with support >0.01 and confidence > .50
## so it can't be a super rare band
musicrules <- apriori(playlist,parameter=list(support=.10,confidence=.5))
inspect(musicrules)



######################################################################
#   Item data set example   #
#####################################################################

itemdataset <- read.csv(file.choose())
View(itemdataset)
class(itemdataset)
one	<- c("I1","I2","I5")
two <- c("I2","I4")
three <- c("I2","I3")
four <- c("I1","I2","I4")
five <- c("I1","I3")
six <- c("I2","I3")
sev <- c("I1","I3")
eig <- c("I1","I2","I3","I5")
nine <- c("I1","I2","I3")

data <- list(one,two,three,four,five,six,sev,eig,nine)

class(data)
library(arules)

data <- lapply(data,unique) # Remove artist duplicates
data[1:2] # The first 2 listeners (1 &3) listen to the following bands

data <- as(data,"transactions")
base <- apriori(data,parameter = list(support=.20,confidence=.6))
# Shows the rule, depending upon the values of support and confidence 
inspect(base)


