#Load Package
library(stats)
library(rcompanion)

#Obtain Data
Normal<-read.csv(file="Analysis/Data/kc_house_data.csv",sep=",",header = TRUE)
Enriched<-read.table(file="Analysis/Data/MainData",sep=",",header=TRUE)

PCAData2<-Enriched[,c("price","NumberOfBedrooms","NumberOfBathrooms","LivingSpace","TotalArea","NumberOfFloors","WaterfrontView","View","YearBuilt",condition)]

#Principal Components
PComp<-prcomp(PCADataAll,center=TRUE,scale=TRUE)

# Generate Predicted Data
PCADataAll2<-predict(PComp,PCADataAll)

#Calculate Generalised Distances

S<-cov(PCADataAll2)
Sinv<-solve(S)
d<-rep(0,times=21436)

for (i in c(1:21436)){
  d[i]<-crossprod(PCADataAll2[i,],crossprod(Sinv,PCADataAll2[i,]))
}


