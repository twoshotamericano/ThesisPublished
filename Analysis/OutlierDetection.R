#Outliers Code

#Principal Compontent Analysis

#Obtain Data
Normal<-read.csv(file="Analysis/Data/kc_house_data.csv",sep=",",header = TRUE)

PCADataAll<-Normal[,c("price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","view","condition","yr_built")]

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

#Summary

PComp$rotation[,1:5]

#Remove Outliers
PCADataAnalysis<-as.data.frame(PCADataAll2)
PCADataAnalysis<-PCADataAnalysis[PCADataAnalysis$PC1<10.5,]
PCADataAnalysis<-PCADataAnalysis[PCADataAnalysis$PC2>-6.5,]
PCADataAnalysis<-PCADataAnalysis[d<23.9,]

Normal<-Normal[PCADataAnalysis$PC1<10.5,]
Normal<-Normal[PCADataAnalysis$PC2>-6.5,]
Normal<-Normal[d<23.9,]

#Mixture Modelling

#Load Package
library(stats)
library(rcompanion)
library(mclust)
data1<-PCADataAnalysis[sample(1:21462,size=1000,replace=FALSE),1:5]
faithfulBIC <- mclustBIC(data1,1:5])
faithfulSummary <- summary(faithfulBIC, data = data1)
faithfulSummary

plot(faithfulBIC, G = 1:3, ylim = c(-8000,-4000), legendArgs = list(x = "bottomright", ncol = 5))
plot(faithfulBIC)

,modelNames = c("VII")

faithfulMclust <- Mclust(data1,G=3,modelNames = c("EEE"))
summary(faithfulMclust,parameters=TRUE)
plot(faithfulMclust,ylim = c(-15,-5))
2

#Summary
pairs(PCADataAll2[,1:3]
)
PComp$rotation[,1:5]

#Remove Outliers
PCADataAnalysis<-as.data.frame(PCADataAll2)
PCADataAnalysis<-PCADataAnalysis[PCADataAnalysis$PC1<10.5,]
PCADataAnalysis<-PCADataAnalysis[PCADataAnalysis$PC2>-6.5,]
PCADataAnalysis[,4]<-log(abs(PCADataAnalysis[,4]))

PCADataAnalysis2<-as.matrix(PCADataAnalysis[,1:5])
#Calculate Generalised Distances

S<-cov(PCADataAnalysis2)
Sinv<-solve(S)
d<-rep(0,times=21462)

for (i in c(1:21462)){
  d[i]<-crossprod(PCADataAnalysis2[i,],crossprod(Sinv,PCADataAnalysis2[i,]))
}
pairs(PCADataAnalysis[,1:5])
