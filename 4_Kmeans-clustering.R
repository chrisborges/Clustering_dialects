########################
## K-means clustering ##
########################

## import Mean Sorensen
mean.sor=read.csv("Mean-Sorensen.csv", sep=",")
mean.sor[1:10,1:16]
# Mean Sorensen is orded by UF first

## data with coordinates
data=read.csv("Dummy-Cities.csv", sep=",", na.strings="")
head(data)

# Distance matrices are ordered by UFs, so to align the coordinates, must order by UF
# order by UF
data2=data[order(data[,5], decreasing = F),, drop=F]
head(data2)
muncols=data2[,c(1:6,32)]
muncols[1:10,1:7] # data frame with ID, XY, UF and MUNICIPIO

class(mean.sor)
mean.sor=as.matrix(mean.sor)
mean.sor[is.nan(mean.sor)] <- NA # transform NAN into NA
mean.sor[is.na(mean.sor)] <- 1 # Transform NA into 1 (means they are diff)
#mean.sor2 <- na.omit(mean.sor) # listwise deletion of missing
sum(is.na(mean.sor))

###
# Determine number of clusters
#K-means clustering is the most popular partitioning method. 
#It requires the analyst to specify the number of clusters to extract. A plot of the within groups sum of squares by number of clusters extracted can help determine the appropriate number of clusters. The analyst looks for a bend in the plot similar to a scree test in factor analysis. See Everitt & Hothorn (pg. 251).

wss <- (nrow(mean.sor)-1)*sum(apply(mean.sor,2,var))
for (i in 2:5) wss[i] <- sum(kmeans(mean.sor, centers=i)$withinss)

plot(1:5, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# 30 classes stabilizes at 8 ?

# Compute k-means with k = number of groups
km.dialetos=kmeans(mean.sor, centers=2) #centers = # of k

print(km.dialetos)
km.dialetos$cluster
km.dialetos$size

data.aff=aggregate(mean.sor,by=list(km.dialetos$cluster),FUN=mean) # get cluster means 
mydata <- data.frame(muncols, km.dialetos$cluster) # append cluster assignment

head(mydata)
map(mydata, 0)

getwd()
write.csv(mydata, "Cluster-with-Coords-2b.csv", row.names = F)

library(factoextra)
fviz_cluster(km.dialetos, data=mean.sor, main="K-Means Results k=2") 
