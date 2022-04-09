#################################################
## Create dummy variables by nearest neighbor ##
#################################################

library(tidyr)

# Calculate distance between cities
data<-read.csv("MunicipiosXYcoor.csv", h=T, sep=",")
datan<-data[,c(1,3,4)]
distmat=dist(datan[,2:3],method="euclidean")
distmat=(as.matrix(distmat))
distmat[1:5,1:5]

row.names(distmat)<- datan[,1]
distmat[1:5,1:5]

### find filled rows

data2=data
data2=data2 %>% drop_na
row.names(data2)<- data2[,1]
data2[1:5,1:10]

data.clean=data #for comparison

##
# find nearest neighbor with information and fill city
# check 3 nearest neighbors

for(i in 1:5572){
  print(i)
  if(is.na(data[i,7])==TRUE){
    distmat.ordered=distmat[order(distmat[,i], decreasing=F),, drop=F] #order a column
    xx=rownames(distmat.ordered)[2] #save city ID # choose row 2, always second smallest to zero
    rown=which(data$ID.x==xx) #save row number
    data[i,c(7:32)]<-data[rown,c(7:32)] # fill blank city

    if(is.na(data[rown,7])==TRUE){
     xx=rownames(distmat.ordered)[3]
     rown=which(data$ID.x==xx)
     data[i,c(7:32)]<-data[rown,c(7:32)] # fill blank city
      
      if(is.na(data[rown,7])==TRUE){
       xx=rownames(distmat.ordered)[4]
      rown=which(data$ID.x==xx)
      data[i,c(7:32)]<-data[rown,c(7:32)] # fill blank city
      }
   }
  } else{
    if(is.na(data[i,7])==FALSE){
      next
    }
  }
}

data[1:6,1:10]
sum(as.numeric(is.na(data[,10]))) #check for NAs


write.table(data, "Dummy-Cities.csv", row.names=F, sep=",")

