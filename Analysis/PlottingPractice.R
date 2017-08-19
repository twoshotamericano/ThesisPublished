library(sp)
library(RColorBrewer)

#Read data file with geo data
data6<-read.table(file="Analysis/Data/EnrichedData",header=TRUE,
                  sep=",",stringsAsFactors = TRUE)

#Get the Condition and Grade variables
origdata<-read.csv(file="Analysis/Data/HousePriceData.csv",header=TRUE,
                     sep=",",stringsAsFactors = TRUE)

origdata<-origdata[,c("TransactionNo","condition","grade")]

data6<-merge(data6,origdata,by.x="TransactionNo",by.y="TransactionNo")

data6$condition<-origdata$condition[data6$TransactionNo]

#Create a Spatial Points Data-frame
coordinates(data6)<-c("Long","Lat")

#Remove data-points assumed to be errors
errors<-which(coordinates(data6)[,1]> -121.7 |
        coordinates(data6)[,1]< -122.6)
data6<-data6[-errors,]

#Generate the Predictions from Fitted Model
data6$FlatFlag<-as.factor((data6$NumberOfFloors<2)*1)
data7<-slot(data6,"data")

#FIt a Linear Model
fit3<-lm(LogSalePrice ~
           condition+
           grade+
           SeattleFlag+
           RenovationYear+
           TotalArea+
           NumberOfBedrooms+
           NumberOfBathrooms+
         LivingSpace+
           NumberOfBedrooms+
            #NumberOfFloors+
            ConstructionYear+
            WaterfrontView+
            SeattleFlag+
            #Restaurants250m+
            Schools1000m+
            PoliceStation1000m+
            SupermarketGrocery750m+
            #Library750m+
            #LiquorStore250m+
            DoctorDentist500m+
            #DepartmentStoreShoppingMall750m+
            #BusTrainTransitStation100m+
            BarNightclubMovie500m+
            #RZestimateHighValueRange+
            #RZestimateAmount+
            NumberOfFloors+
            ConstructionYear+
            WaterfrontView+
            SeattleFlag+
            #Restaurants250m+
            Schools1000m+
            PoliceStation1000m+
            SupermarketGrocery750m+
            Library750m
            #LiquorStore250m+
            , data=data6)

data6$prediction<-predict(fit3)

#Generate a Grid covering area of interest
NoCells<-round(apply(bbox(data6),1,diff)/0.005)
gt<-GridTopology(c(-122.5190,47.1559),
                 c(0.005,0.005),NoCells)
coarseGrid<-SpatialGrid(gt,proj4string(data6))

#FieldList
Names<-c("LogSalePrice",
         "SalePrice",
         "condition",
         "grade",
         "prediction",
         "NumberOfBedrooms",
         "NumberOfFloors",
         "ConstructionYear",
         "WaterfrontView",
         "SeattleFlag",
         "Restaurants250m",
         "Schools1000m",
         "PoliceStation1000m",
         "SupermarketGrocery750m",
         "Library750m",
         "LiquorStore250m",
         "DoctorDentist500m",
         "DepartmentStoreShoppingMall750m",
         "BusTrainTransitStation100m",
         "BarNightclubMovie500m",
         "RZestimateHighValueRange",
         "RZestimateAmount")

data6[,Names]

agg2<-aggregate(data6[,Names],coarseGrid,var)

#Plot Graphs

par(pin=c(7.6,3.5))
rw.colors<-colorRampPalette(c("white","red"))
#rw.colors<-brewer.pal(10,"Reds")
plot(agg[c("prediction")]),col=rw.colors(5000))

plot(agg[c("SalePrice")])

spplot(agg2[c("LogSalePrice","prediction")],col=rw.colors(5000),cex=0.05,pch=16)

##Useful Functions for Plotting

SpatialPolygonsRescale(layout.scale.bar(),
                       offset=locator(1),
                       scale=0.17,
                       fill=c("transparent","black"),
                       plot.grid=FALSE)
text(locator(1),"0")
text(locator(1),"20 km")
SpatialPolygonsRescale(layout.north.arrow(),
                       offset=locator(1),
                       scale=0.05,
                       plot.grid=FALSE)

box()
title("Avg Monthly Rental Value")


