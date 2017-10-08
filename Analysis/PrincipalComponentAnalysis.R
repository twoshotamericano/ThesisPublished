#Load Package
library(stats)
library(rcompanion)

#Obtain Data
PCAData<-read.table(file="Analysis/Data/EnrichedData",sep=",",header = TRUE)

#Drop Unwanted Columns
PCAData2<-PCAData[,c(2,5:9,11,13:15,17,19,25:34,67)]

#Convert Factor to Numeric
PCAData2$RenovationFlag<-as.numeric(levels(PCAData2$RenovationFlag)=="Yes")[PCAData2$RenovationFlag]
PCAData2$SeattleFlag<-as.numeric(levels(PCAData2$SeattleFlag)=="Yes")[PCAData2$SeattleFlag]

flag<-as.numeric(irisCluster$cluster)

#Split single floor properteis from rest
PCADataAll<-PCAData2
#PCADataAppt<-PCAData2[PCAData2$NumberOfFloors<2,]
#PCADataHses<-PCAData2[PCAData2$NumberOfFloors>=2,]
#PCADataCluster<-PCAData2[flag==1,]

#Principal Components
PComp<-prcomp(PCADataAll,center=TRUE,scale=TRUE)

summary(PComp
        )


# Generate Predicted Data
PCADataAll2<-predict(PComp,PCADataAll)

#Transform univariates


pairs(PCADataAll2[,1:6])
