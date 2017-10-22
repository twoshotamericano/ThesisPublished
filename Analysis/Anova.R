# Import data

nooutlierdata<-read.csv(file="Analysis/Data/NoOutliers.csv",header=TRUE,
                 sep=",",stringsAsFactors = TRUE)
nooutlierdata$TransactionNo<-nooutlierdata$id

data<-read.csv(file="Analysis/Data/HousePriceData.csv",header=TRUE,
               sep=",",stringsAsFactors = TRUE)

data<-merge(data,nooutlierdata,by="TransactionNo")

# Add Quantile Information
a<-cut(data$LogSalePrice, breaks=c(quantile(data$LogSalePrice, probs = seq(0, 1, by = 0.25))),
       labels=c("0-25","25-50","50-75","75-100"), include.lowest=TRUE)

data$quantile<-a

#create data frame for the ANOVA

data2<-data[,c("AboveGroundFloor",
               "RenovationFlag",
               "grade",
               "condition",
               "View",
               "WaterfrontView",
               "NumberOfFloors",
               "NumberOfBedrooms",
               "NumberOfBathrooms",
               "SaleYear",
               "ConstructionYear",
               "TotalArea",
               "BasementSize",
               "LivingSpace",
               "quantile")]
#Manova Test

test<-manova(cbind(AboveGroundFloor,
                   RenovationFlag,
                   grade,
                   condition,
                   View,
                   WaterfrontView,
                   NumberOfFloors,
                   NumberOfBedrooms,
                   NumberOfBathrooms,
                   SaleYear,
                   ConstructionYear,
                   TotalArea,
                   LivingSpace)
             ~quantile,data=data2)

summary(test,test="Wilks")

