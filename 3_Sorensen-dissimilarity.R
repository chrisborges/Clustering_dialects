rm(list = ls()) 

library(vegan)
library(betapart)

######################################
## Calculate Sorensen dissimilarity ##
######################################

calcular.edist=function(i){
  setwd("~/Library/folder")
  QPam=paste("Q",i,"-PAM.csv", sep="")
  Qx=read.csv(QPam, h=T)
  # Euclidean Distance
  #Qxdist=dist(Qx, method="euclidean")
  #matriz=as.matrix(Qxdist)
  
  # Sorensen dissimilarity
  Qxsor=beta.pair(Qx[,-(1:2)], index.family = "sorensen")  
  matriz=as.matrix(Qxsor$beta.sim) #Simpson's index
  matriz[is.nan(matriz)] <- NA # remove NANs, transform into NAs 
  return(matriz)
}

# calculating distance matrix per question

Q1.sim=calcular.edist(i=1)
Q2.sim=calcular.edist(i=2)
Q3.sim=calcular.edist(i=3)
Q4.sim=calcular.edist(i=4)
Q5.sim=calcular.edist(i=5)
Q6.sim=calcular.edist(i=6)
Q7.sim=calcular.edist(i=7)
Q8.sim=calcular.edist(i=8)
Q9.sim=calcular.edist(i=9)
Q10.sim=calcular.edist(i=10)

Q11.sim=calcular.edist(i=11)
Q12.sim=calcular.edist(i=12)
Q13.sim=calcular.edist(i=13)
Q14.sim=calcular.edist(i=14)
Q15.sim=calcular.edist(i=15)
Q16.sim=calcular.edist(i=16)
Q17.sim=calcular.edist(i=17)
Q18.sim=calcular.edist(i=18)
Q19.sim=calcular.edist(i=19)
Q20.sim=calcular.edist(i=20)

Q21.sim=calcular.edist(i=21)
Q22.sim=calcular.edist(i=22)
Q23.sim=calcular.edist(i=23)
Q24.sim=calcular.edist(i=24)
Q25.sim=calcular.edist(i=25)

# Calculate mean

Matrices <- list(Q1.sim,Q2.sim,Q3.sim,Q4.sim,Q5.sim,
                 Q6.sim,Q7.sim,Q8.sim,Q9.sim,Q10.sim,
                 Q11.sim,Q12.sim,Q13.sim,Q14.sim,Q15.sim,
                 Q16.sim,Q17.sim,Q18.sim,Q19.sim,Q20.sim,
                 Q21.sim,Q22.sim,Q23.sim,Q24.sim,Q25.sim)

Matrices2 <- do.call(cbind, Matrices)
Matrices2 <- array(Matrices2, dim=c(dim(Matrices[[1]]), length(Matrices)))
Mean.simp=apply(Matrices2, c(1, 2), mean, na.rm = TRUE) # mean of all distances

head(Mean.simp)
class(Mean.simp)
length(Mean.simp)

write.csv(Mean.simp, "Mean-Simp.csv", row.names = F)
