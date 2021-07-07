#Library will load the existing loaded package. 
#Require will install or update when the package is not in our repository

require(cluster)
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
library(digest)

numdata <- apphappy.3.num.frame

dev.off()

### Following are EDA

str(numdata)
head(numdata)
tail(numdata)
summary(numdata)

numdata$q5r1 <- NULL

summary(numdata)

numdata <- na.omit(numdata)

numdata$age <- numdata$q1
age_table=table(numdata$age)
age_table

numdata$gender <- numdata$q57
gender_table=table(numdata$gender)
gender_table

numdata$education <- numdata$q48
education_table=table(numdata$education)
education_table

numdata$marital <- numdata$q49
marital_table=table(numdata$marital)
marital_table

numdata$race <- numdata$q54
race_table=table(numdata$race)
race_table

numdata$hispanic <- numdata$q55
hispanic_table=table(numdata$hispanic)
hispanic_table

numdata$income <- numdata$q56
income_table=table(numdata$income)
income_table

numdata$MusicID <- numdata$q4r1
numdata$AppCount <- numdata$q11
numdata$FreeApps <- numdata$q12
numdata$Pandora <- numdata$q13r4
numdata$Vevo <- numdata$q13r5
numdata$AOL <- numdata$q13r7
numdata$Last.FM <- numdata$q13r8
numdata$Yahoo <- numdata$q13r9
numdata$Important <- numdata$q24r7
numdata$Creative <- numdata$q25r9
numdata$Entertainment <- numdata$q26r17
numdata$CantGetEnough <- numdata$q26r8
numdata$Cool <- numdata$q26r9
numdata$Show <- numdata$q26r10
numdata$ChildrenImpact <- numdata$q26r11
numdata$TryNew <- numdata$q25r5
numdata$TechDev <- numdata$q24r1


table(numdata$MusicID)
table(numdata$AppCount)
table(numdata$FreeApps)
table(numdata$Pandora)
table(numdata$Vevo)
table(numdata$AOL)
table(numdata$Last.FM)
table(numdata$Yahoo)
table(numdata$Important)
table(numdata$Creative)
table(numdata$Entertainment)
table(numdata$CantGetEnough)
table(numdata$Cool)
table(numdata$Show)
table(numdata$ChildrenImpact)
table(numdata$TryNew)
table(numdata$TechDev)


### Creating subsets ###

numdata$UseMusicApps <- 0

numdata$UseMusicApps[(numdata$q13r4 == 1) | (numdata$q13r5 == 1) | (numdata$q13r7 == 1) | (numdata$q13r8 == 1) | (numdata$q13r9 == 1)] <- 1

numsub <- subset(numdata, select=c("MusicID", "UseMusicApps", "Important", "CantGetEnough", "FreeApps", 
                                   "income", "age", "Entertainment", "AppCount"))

numsub <- na.omit(numsub)

str(numsub)
summary(numsub)
dev.off()
attach(numsub)

a=barplot(age)
a

b=table(MusicID)
barplot(b)

c=table(race)
barplot(c)

d=table(AppCount)
barplot(d)

e=table(FreeApps)
barplot(e)

f=table(TechDev)
barplot(f)

g=table(Entertainment)
barplot(g)

rcorr(as.matrix(numsub), type="pearson")

require(corrplot)
numsubcorrelation <- cor(numsub)
corrplot(numsubcorrelation)

mcor <- cor(numsub)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black")

corrplot(numsubcorrelation, method="shade", addCoef.col="black", 
addCoefasPercent=TRUE ,type="lower", shade.col=NA, tl.col="black", 
tl.srt=45, addcolorlabel="no", order="AOE",insig = "p-value")

###################################################
### Create a 'scree' plot to determine the num of clusters
#####################################################

dev.off()
wssplot <- function(numsub, nc=15, seed=1234) {
  wss <- (nrow(numsub)-1)*sum(apply(numsub,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(numsub, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(numsub)

######################################
### Create a Kmeans with 2 clusters
#########################################
clusterresults <- kmeans(numsub,2)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

######################################
### Create a Kmeans with 3 clusters
#########################################
clusterresults <- kmeans(numsub,3)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

######################################
### Create a Kmeans with 4 clusters
#########################################
clusterresults <- kmeans(numsub,4)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

######################################
### Create a Kmeans with 5 clusters
#########################################
clusterresults <- kmeans(numsub,5)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

######################################
### Create a Kmeans with 6 clusters
#########################################
clusterresults <- kmeans(numsub,6)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

######################################
### Create a Kmeans with 7 clusters
#########################################
clusterresults <- kmeans(numsub,7)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

######################################
### Create a Kmeans with 5 clusters
#########################################
clusterresults <- kmeans(numsub,5)
names(clusterresults)
clusterresults$withinss
clusterresults$tot.withinss
clusterresults$totss
clusterresults$betweenss
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

### Create a PC (Principal Componenet plot)

plot(clusterresults, data=numsub)

clusterresults$centers


head(clusterresults$cluster)

dev.off()
dissE <- daisy(numsub)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)
str(sk2)
plot(sk2)


newdf <- as.data.frame(clusterresults$cluster)

write.csv(newdf, file = "clusterresults.csv")

write.csv(numsub, file = "numsub.csv")

############################
### Create a dataset with the original data with the cluster info
### This will be useful for creating profiles for the clusters
#####################################33

numdata <- na.omit(numdata)

newdf <- read.csv("clusterresults.csv")
combdata <- cbind(numsub,newdf,numdata$gender, numdata$education, numdata$marital, numdata$race, numdata$hispanic)
head(combdata)

require(reshape)
combdata <- rename(combdata, c(clusterresults.cluster="cluster"))
head(combdata)

aggregate(combdata,by=list(byvar=combdata$cluster), mean)

#######################################
############### PCA Plots ##############
######################################
dev.off()
pca <-princomp(numsub)
plot(pca$scores[,1],pca$scores[,2])

names(pca)
str(pca)
summary(pca)
head(pca$scores)

sort(pca$scores[,1])

numsub["1823",]
numsub["1667",]
numsub["1581",]
numsub["1982",]

sort(pca$scores[,1], decreasing = TRUE)

numsub["1926",]
numsub["358",]
numsub["57",]
numsub["713",]

sort(pca$scores[,2])

numsub["1776",]
numsub["394",]
numsub["834",]
numsub["181",]

sort(pca$scores[,2], decreasing = TRUE)

numsub["51",]
numsub["2003",]
numsub["1484",]
numsub["640",]


##  Create cuts:
pcadf <- as.data.frame(pca$scores)
pca1 <- cut(pcadf$Comp.1, 10)
pca2 <- cut(pcadf$Comp.2, 10)

##  Calculate joint counts at cut levels:
z <- table(pca1, pca2)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")


###################################################
### Create a 'scree' plot to determine the num of clusters
#####################################################

dev.off()
wssplot <- function(numsub, nc=15, seed=1234) {
  wss <- (nrow(numsub)-1)*sum(apply(numsub,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(numsub, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(numsub)

#######################################################
##########  k means with raw data with 5 clusters######
#######################################################

clusterresults <- kmeans(numsubr,5)
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare
str(clusterresults)
plot(clusterresults, data=numsubr)
dev.off()
dissE <- daisy(numsubr)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)
str(sk2)

plot(sk2)
