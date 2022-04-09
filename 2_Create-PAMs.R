##########################################################
## Fill empty cities with information from nearest city ##
##########################################################

data=read.csv("Dummy-Cities.csv", sep=",", h=T)
head(data)
coords=data[,c(1:6)]

for(i in 1:25){

i=25
XX=i+6
Q1=data[,c(1,5,6,XX)]
head(Q1)

library(tidyr)
## The ";\\s+" means that the separator is a ";" followed by one or more spaces
Qi=paste("Q",i, sep="")
Q1sep=separate_rows(Q1,Qi,sep=" ") #each line is one municipio, if it has more than one answer then it repeats, thats why it has 6384+ lines
Q1sep
class(Q1sep[,4])

Q1sep<- apply(Q1sep, 2, function(x) gsub("^$|^ $", NA, x))
Q1sep <- as.data.frame(Q1sep)
levels(Q1sep[,4])
respostas=levels(Q1sep[,4])
respostas

library(reshape2)
formula=paste0("UF+NM_MUNICIP~Q",i)
Q1PAM=dcast(Q1sep, noquote(formula), length) # create PAM! :D
head(Q1PAM)

EX=length(respostas)+3
Q1PAM=Q1PAM[,-EX]
head(Q1PAM)
Q1PAM["n"]<-rep(0, nrow(Q1PAM))
head(Q1PAM)
Q1PAM_order=Q1PAM[,c(1,2,6,7,3,4,8,5,9:16)]
head(Q1PAM_order)
Q1PAM=Q1PAM_order
## -- ##
Qx=paste("Q",i,"-PAM.csv", sep="")

setwd("~/Library/Data/")
write.csv(Q1PAM, Qx, row.names=F)
}

