#Preliminaries
library(ggplot2)
library(plyr)
library(scales)
library(corrplot)
library(Hmisc)
#setwd("C:/Users/FED/Desktop/Thesis/HousePriceRegression")
data<-read.csv(file="Analysis/Data/NoOutliers.csv")

#Removing duplicate entries
data<-data[!duplicated(data$id),]

#Transforming data into nice format
data$year<-as.numeric(substr(data$date,1,4))
data$month<-as.numeric(substr(data$date,5,6))
data$day<-as.numeric(substr(data$date,7,8))
data$quartile<-cut(data$price,breaks=c(0,322000,450000,645000,7700000),labels=c("1Qr","2Qr","3Qr","4Qr"))
data$pricel<-log(data$price)
totalvalue<-sum(data$price)
totalvaluel<-sum(data$pricel)
bath_max<-max(data$bathrooms)
data$pricel_perc<-data$pricel/totalvaluel
data$sqm_living<-data$sqft_living*0.09290304
data$sqm_lot<-data$sqft_lot*0.09290304*0.001
data$sqm_above<-data$sqft_above*0.09290304
data$sqm_basement<-data$sqft_basement*0.09290304
data$bedroom_group<-cut(data$bedrooms,breaks=c(-Inf,0.99,1.99,2.99,3.99,4.99,5.99,Inf),labels=c("<1","1","2","3","4","5",">5"))
data$bathroom_group<-cut(data$bathrooms,breaks=c(-Inf,0.99,1.99,2.99,3.99,4.99,5.99,Inf),labels=c("<1",1:5,">5"))
data$livingspace_group<-cut(data$sqm_living,breaks=c(0,seq(50,400,50),Inf),labels=c(seq(50,400,50),">400"))
data$lotsize_group<-cut(data$sqm_lot,breaks=seq(0,4000,500))

#SummaryStatistics
price_summ<-ddply(data,"quartile",summarise,Valuation=sum(price)/(1e9),
                  number=length(price),average=mean(price),min=min(price),max=max(price),log_valuation=sum(pricel),percent_total=sum(price)/totalvalue,percent_total_l=sum(pricel)/totalvaluel)
price_summ<-data.frame(price_summ)

price_summ_bedroom<-ddply(data,"bedroom_group",summarise,Valuation=sum(price)/(1e9),
                          number=length(price),average=mean(price),min=min(price),max=max(price),log_valuation=sum(pricel),percent_total=sum(price)/totalvalue,percent_total_l=sum(pricel)/totalvaluel)
price_summ_bedroom<-data.frame(price_summ_bedroom)

price_summ_bathroom<-ddply(data,"bathroom_group",summarise,Valuation=sum(price)/(1e9),
                          number=length(price),average=mean(price),min=min(price),max=max(price),log_valuation=sum(pricel),percent_total=sum(price)/totalvalue,percent_total_l=sum(pricel)/totalvaluel)
price_summ_bathroom<-data.frame(price_summ_bathroom)

price_summ_livingspace<-ddply(data,"livingspace_group",summarise,Valuation=sum(price)/(1e9),
                              number=length(price),average=mean(price),min=min(price),max=max(price),log_valuation=sum(pricel),percent_total=sum(price)/totalvalue,percent_total_l=sum(pricel)/totalvaluel)
price_summ_livingspace<-data.frame(price_summ_livingspace)

price_summ_lotsize<-ddply(data,"lotsize_group",summarise,Valuation=sum(price)/(1e9),
                              number=length(price),average=mean(price),min=min(price),max=max(price),log_valuation=sum(pricel),percent_total=sum(price)/totalvalue,percent_total_l=sum(pricel)/totalvaluel)
price_summ_lotsize<-data.frame(price_summ_lotsize)

#Set the Theme For Charts
mytheme<-theme_bw()+
          theme(axis.ticks=element_blank(),
          axis.title.y=element_text(colour="darkred",face="italic",size=rel(3.0)),
          axis.title.x=element_text(colour="darkred",face="italic",size=rel(3.0)),
          axis.text=element_text(size=rel(3.0)),
          plot.title=element_text(size=rel(4.0))
          )

plot1
#Distribution of Property Values

plot1<-ggplot(data=price_summ,aes(x=quartile,y=Valuation))+
        coord_fixed(0.5)+
        geom_bar(stat="identity",fill="lightblue",colour="black") +
        geom_text(aes(label=number),size=rel(3),vjust=1.5) +
        scale_y_continuous(limits=c(0,8),breaks=seq(0,8,1)) +
        scale_x_discrete(labels=c("1","2","3","4")) +
        mytheme+
        labs(x="Price Quartile (1= Low)",y="Aggregate Value ($Bn)") #+
        #ggtitle("Aggregate Property Prices")

#Size of the Upper Tail
upper_tail<-subset(data,price>1050000)

plot2<-ggplot(data=data,aes(,x=quartile,y=price/1000000))+
      coord_fixed(0.5)+
      geom_boxplot(fill="lightblue") +
      mytheme +
      scale_y_continuous(limits=c(0,8)) +
      scale_x_discrete(labels=c("1","2","3","4")) +
      labs(x="Price Quartile (1= Low)",y="Individual Value ($1m)") +
      #ggtitle("Individual Property Prices")+
      annotate("segment",x=3.5,xend=3.5,y=1.5,yend=7.7,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
      annotate("text",label="The upper 6% of properties \n account for 18% portfolio size",x=2,y=5.0,colour="darkred",size=rel(5))
plot2

#Distribution of log property values
plot3<-ggplot(data=price_summ,aes(x=quartile,y=log_valuation/10000))+
  coord_fixed(0.5)+
  mytheme+
  geom_bar(stat="identity",fill="lightblue",colour="black") +
  geom_text(aes(label=number),size=rel(3),vjust=1.5) +
  scale_y_continuous(limits=c(0,8),breaks=seq(0,10,1)) +
  scale_x_discrete(labels=c("1","2","3","4")) +
  labs(x="Price Quartile (1= Low)",y="LogPrice") #+
  #ggtitle("Aggregate Logged Prices")
plot3

#Size of the Upper Tail with logged values

plot4<-ggplot(data=data,aes(x=quartile,y=pricel))+
  coord_fixed(0.5)+
  geom_boxplot(fill="lightblue") +
  mytheme+
  scale_y_continuous(limits=c(10,18),breaks=seq(10,20,2)) +
  scale_x_discrete(labels=c("1","2","3","4")) +
  labs(x="Price Quartile (1=Low)",y="Logged Price (11.5=$100k)") #+
  #ggtitle("Individual Property Prices")#+
  #annotate("segment",x=3.5,xend=3.5,y=10.5,yend=77,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
  #annotate("text",label="The upper 6% of properties account for 18% portfolio size",x=2,y=5,colour="darkred")

#Response Data
pdf("plot1.pdf", width=4, height=4)
plot1
dev.off()

pdf("plot2.pdf", width=4, height=4)
plot2
dev.off()

pdf("plot3.pdf", width=4, height=4)
plot3
dev.off()

pdf("plot4.pdf", width=4, height=4)
plot4
dev.off()

#Size Related Explanatory Variables

#Exploratory Analysis for internal space
size_data<-rbind(cbind(data$sqm_living,c(1)),
                 #cbind(data$sqm_lot,c(2)),
                 cbind(data$sqm_above,c(3)),
                 cbind(data$sqm_basement,c(4))
                )

size_data<-data.frame(Space=size_data[,1],Category=size_data[,2])
size_data$Category<-factor(size_data$Category,labels=c("Living","Above","Base"))

plot7<-ggplot(data=size_data,aes(x=Category,y=Space))+
  geom_violin(fill="lightblue")+
  #geom_boxplot(width=.025, fill="black", outlier.colour=NA)+
  mytheme+
  coord_fixed(0.002)+
  labs(y="Area (sqm)",x="Category of Space")+
  stat_summary(fun.y="median", geom="point", shape=23, size=3, fill="white")
plot7

pdf("plot7.pdf", width=4, height=4)
plot7
dev.off()

#Exploratory Analysis of External Space
plot8<-ggplot(data=data,aes(x="",y=sqm_lot))+
        geom_boxplot(fill="lightblue")+
        scale_y_continuous(limits=c(0,5),breaks=seq(0,5,0.5))+
        mytheme+
        labs(x="Size of Lot",y="1000 sqm")+
        stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
        coord_fixed(0.15)
plot8

pdf("plot8.pdf", width=4, height=4)
plot8
dev.off()


#Exploratory Analysis for bedrooms
plot5<-ggplot(data=price_summ_bedroom,aes(x=bedroom_group,y=percent_total_l*100))+
  coord_fixed(ratio=0.14)+
  scale_y_continuous(breaks=seq(0,60,10),limits=c(0,50))+
  geom_bar(stat="identity",fill="lightblue",colour="black",width=0.9) +
  mytheme+
  labs(x="Number of Bedrooms",y="% Portfolio (Logged Values)")+
  geom_text(aes(label=number),size=rel(3),vjust=-1.0) +
  mytheme
plot5

pdf("plot5.pdf", width=4, height=4)
plot5
dev.off()

#Exploratory Analysis for bathrooms
plot6<-ggplot(data=price_summ_bathroom,aes(x=price_summ_bathroom$bathroom_group,y=price_summ_bathroom$percent_total_l*100))+
  coord_fixed(0.14)+
  geom_bar(stat="identity",fill="lightblue",colour="black")+
  geom_text(aes(label=number),size=rel(3),vjust=-0.5) +
  #scale_x_discrete(breaks=seq(1,32,2)/4)+
  #scale_y_continuous(breaks=seq(0,30,5))+
  labs(x="Number of Bathrooms",y="% Portfolio (Logged Values)")+
  mytheme

pdf("plot6.pdf", width=4, height=4)
plot6
dev.off()

#Exploratroy Analysis for living space

#Exploratory Analysis for floors
summary_floor<-ddply(data,"floors",summarise,count=length(id),value=sum(pricel))
summary_floor$floors<-factor(summary_floor$floors)
plot11<-ggplot(data=summary_floor,aes(x=floors,y=value/sum(value)))+
  coord_fixed(8.5)+
  geom_bar(stat="identity",position="dodge",fill="lightblue",colour="black")+
  labs(y="% Portfolio Logged Value",x="Number of Floors")+
  mytheme+
  scale_y_continuous(labels=percent,limits=c(0,0.6),breaks=seq(0,0.6,0.1))+
  geom_text(aes(label=count),size=rel(3),vjust=-0.7)
plot11

pdf("plot11.pdf", width=4, height=4)
plot11
dev.off()

#Exploratory Analysis for Waterfront
summary_waterfront<-ddply(data,"waterfront",summarise,count=length(id),value=sum(pricel))
summary_waterfront$waterfront<-factor(summary_waterfront$waterfront)

plot12<-ggplot(data=summary_waterfront,aes(x=waterfront,y=value/sum(value)))+
  coord_fixed(1.9)+
  geom_bar(stat="identity",fill="lightblue",colour="black",width=0.3)+
  mytheme+
  labs(x="Waterfront",y="% Portfolio Logged Value")+
  geom_text(aes(label=count),vjust=1.4,size=rel(3))+
  scale_y_continuous(breaks=seq(0,1,0.1),labels=percent)+
  scale_x_discrete(labels=c("No","Yes"))
plot12

pdf("plot12.pdf", width=4, height=4)
plot12
dev.off()


#Exploratory Analysis for View
summary_view<-ddply(data,"view",summarise,count=length(id),value=sum(pricel))
summary_view$view<-factor(summary_view$view)

plot13<-ggplot(data=summary_view,aes(x=view,y=value/sum(value)))+
  coord_fixed(5)+
  geom_bar(stat="identity",fill="lightblue",colour="black",position=position_dodge(5))+
  mytheme+
  labs(x="View",y="% Portfolio Logged Value")+
  geom_text(aes(label=count),vjust=-0.4,size=rel(3))+
  scale_y_continuous(breaks=seq(0,1,0.2),labels=percent) #+
  #scale_x_discrete(labels=c("No","Yes"))
plot13

pdf("plot13.pdf", width=4, height=4)
plot13
dev.off()


#Exploratory Analysis for Condition
summary_condition<-ddply(data,"condition",summarise,count=length(id),value=sum(pricel))
summary_condition$condition<-factor(summary_condition$condition)

plot14<-ggplot(data=summary_condition,aes(x=condition,y=value/sum(value)))+
  coord_fixed(7)+
  geom_bar(stat="identity",fill="lightblue",colour="black",position=position_dodge(5))+
  mytheme+
  labs(x="Property Condition",y="% Portfolio Logged Value")+
  geom_text(aes(label=count),vjust=-0.2,size=rel(3))+
  scale_y_continuous(breaks=seq(0,1,0.2),labels=percent) #+
#scale_x_discrete(labels=c("No","Yes"))
plot14

pdf("plot14.pdf",width=4,height=4)
plot14
dev.off()

#Exploratory Analysis by Grade
data$gradef<-cut(data$grade,breaks=c(0,5.1,6.1,7.1,8.1,9.1,10.1,14),labels=c("<5","6","7","8","9","10",">10"))
data$gradef<-factor(data$gradef)
summary_grade<-ddply(data,"gradef",summarise,count=length(id),value=sum(pricel))
summary_grade$gradef<-factor(summary_grade$gradef)

plot15<-ggplot(data=summary_grade,aes(x=gradef,y=value/sum(value)))+
  coord_fixed(18)+
  geom_bar(stat="identity",fill="lightblue",colour="black",position=position_dodge(5))+
  mytheme+
  labs(x="Grade",y="% Portfolio Logged Value")+
  geom_text(aes(label=count),vjust=-0.2,size=rel(3))+
  scale_y_continuous(breaks=seq(0,1,0.1),labels=percent) #+
#scale_x_discrete(labels=c("No","Yes"))
plot15

pdf("plot15.pdf",width=4,height=4)
plot15
dev.off()

#Exploratory Analysis by Year Built
data$yr_builtf<-cut(data$yr_built,breaks=seq(1899.1,2020,20),labels=c("<\n 1920","20-\n40","40-\n60","60-\n80","80-\n00",">\n2000"))
data$yr_builtf<-factor(data$yr_builtf)

summary_yearbuilt<-ddply(data,"yr_builtf",summarise,count=length(id),value=sum(pricel))
summary_yearbuilt$yr_builtf<-factor(summary_yearbuilt$yr_builtf)

plot16<-ggplot(data=summary_yearbuilt,aes(x=yr_builtf,y=value/sum(value)))+
  #coord_fixed(1)+
  geom_bar(stat="identity",fill="lightblue",colour="black",position=position_dodge(5))+
  mytheme+
  labs(x="Built Year",y="% Portfolio Logged Value")+
  geom_text(aes(label=count),vjust=-0.2,size=rel(3))+
  scale_y_continuous(breaks=seq(0,1,0.1),labels=percent) +
  theme(axis.text.x=element_text(angle=0,size=rel(0.8)))
#scale_x_discrete(labels=c("No","Yes"))
plot16

pdf("plot16.pdf",width=4,height=4)
plot16
dev.off()

#Location Data
summary_location<-ddply(data,"zipcode",summarise,count=length(id),value=sum(pricel))
summary_location$zipcode<-factor(summary_location$zipcode)
summary_location$zipcode<-reorder(summary_location$zipcode,summary_location$count,FUN=mean)

  plot17<-ggplot(data=summary_location,aes(x=zipcode,y=value/sum(value)))+
    #coord_fixed(1)+
    geom_bar(stat="identity",fill="lightblue",colour="black",position=position_dodge(5))+
    mytheme+
    labs(x="Zipcode",y="% Portfolio Logged Value")+
    #geom_text(aes(label=count),vjust=-0.2,size=rel(3))+
    scale_y_continuous(breaks=seq(0,0.03,0.005),labels=percent) +
    scale_x_discrete(breaks=NULL)
  #scale_x_discrete(labels=c("No","Yes"))
  plot17

pdf("plot17.pdf",width=4,height=4)
plot17
dev.off()

#Correlation Matrix
data2<-subset(data,select=
                c(price,bedrooms,bathrooms,
                  floors,waterfront,view,condition,
                  grade,yr_built,yr_renovated,
                  year,month,sqm_living,sqm_lot,
                  sqm_above,sqm_basement))

mcor<-round(cor(data2),digits=2)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

pdf("plot18.pdf")
corrplot(mcor,
           method="shade",
           cl.cex=1,
           shade.col=NA,
           order="AOE",
           type="lower",
           addCoef.col="black",
          tl.srt=45,
         addCoefasPercent = TRUE,
           number.cex=0.8)
dev.off()

#######Facet Plots
##Stack the explanatory variable data ontop of eachother
##This is a preliminary step for a facet chart

data<-data[data$bedrooms<30,]
a<-rep(data$pricel,12)
b<-c(data$sqm_living,
         data$sqm_above,
         data$sqm_basement,
         data$sqm_lot,
         data$bedrooms,
         data$floors,
         data$waterfront,
         data$view,
         data$grade,
         data$yr_built,
         data$zipcode,
         data$month)
c<-rep(1:12,each=length(data$pricel))

name<-c("sqm_living",
        "sqm_above",
        "sqm_basement",
        "sqm_lot",
        "bedrooms",
        "floors",
        "waterfront",
        "view",
        "grade",
        "yr_built",
        "zipcode",
        "month")

stack<-data.frame(pricel=a,variable=b,flag=factor(c))
stack$flag<-mapvalues(stack$flag,
                      c("1","2","3","4","5","6","7","8","9","10","11","12"),
                      name)


#### Create custom data labels
## For each chart (ie. explanatory variable),
##I create a unique label.
##For geom_text the labels must be stored in a dataframe


coeff<-data.frame(Name=NULL,
                  Sxx=NULL,
                  b1_sq=NULL,
                  SSreg=NULL,
                  phrase=NULL,
                  x=NULL,
                  y=NULL,
                  flag=NULL)

##This loop creates the customised data labels
##The only trick is to include a column with the same name
##and class as the facet variable

for (i in 1:length(name)){
  options(digits=5)
  #Calculate the coefficients to plot
  z<-lm(data$pricel~data[,name[i]])
  coeff[i,c("Name")]<-i
  coeff[i,c("Sxx")]<-sprintf("Sxx=%s",format(var(data[,name[i]])*(length(data[,name[i]])-1),scientific = TRUE, digits = 3))
  coeff[i,c("b1_sq")]<-sprintf("b1=%s",format(summary(z)$coefficients[2],scientific = TRUE, digits = 3))
  coeff[i,c("SSreg")]<-sprintf("SSreg=%s",format(anova(z)[1,2],scientific = TRUE, digits = 3))
  coeff[i,c("phrase")]<-paste(coeff[i,c("Sxx")],"\n",
                              coeff[i,c("b1_sq")],"\n",
                              coeff[i,c("SSreg")],
                              sep="")
  coeff[i,c("x")]<--Inf
  coeff[i,c("y")]<-Inf
  coeff[i,c("flag")]<-name[i]

}

##Need to ensure the flag variable is a factor
coeff$flag<-as.factor(coeff$flag)

####Create the facet plot
##Start with creating a ggplot object
p<-ggplot(stack,aes(y=pricel,x=variable))+
  stat_binhex()+
  scale_fill_gradient(low="lightblue", high="red")+
  geom_smooth(method=lm,se=TRUE)+
  theme(axis.text.x=element_text(angle=90))+
  guides(fill=FALSE)+
  labs(y="Log Value")

##Now create the facet plot with custom labels
#pdf("plot19.pdf")
#dev.off()
p+facet_wrap(~flag, scales="free_x",ncol=3,nrow=4)+
geom_text(aes(x=Inf, y=Inf, label=SSreg),data=coeff,vjust=1.0,hjust=1.5)+
geom_text(aes(x=Inf, y=Inf, label=b1_sq),data=coeff,vjust=2.5,hjust=1.65)+
geom_text(aes(x=Inf, y=Inf, label=Sxx),data=coeff,vjust=4.0,hjust=1.6)
#dev.off()

###Performing Facet Plots for Enriched Data

library(dplyr)

data3<-read.table(file="Analysis/Data/EnrichedData",header=TRUE,sep=",")

data3<-rename(data3,
              AboveGroundFloorArea=AboveGroundFloor,
              LotSize=TotalArea,
              SchoolCount=Schools1000m,
              DentistCount=DoctorDentist500m,
              LibraryCount=Library750m,
              FoodStoreCount=SupermarketGrocery750m,
              StationCount=BusTrainTransitStation100m,
              PoliceCount=PoliceStation1000m,
              pricel=LogSalePrice)


data<-subset(data3,select=c(pricel,
                           SchoolCount,
                           DentistCount,
                           LibraryCount,
                           FoodStoreCount,
                           StationCount,
                           PoliceCount))

a<-rep(data$pricel,6)
b<-c(data$SchoolCount,
     data$LibraryCount,
     data$DentistCount,
     data$FoodStoreCount,
     data$StationCount,
     data$PoliceCount)

c<-rep(1:6,each=length(data$pricel))

name<-c("SchoolCount",
        "LibraryCount",
        "DentistCount",
        "FoodStoreCount",
        "StationCount",
        "PoliceCount")

stack<-data.frame(pricel=a,variable=b,flag=factor(c))
stack$flag<-mapvalues(stack$flag,
                      c("1","2","3","4","5","6"),
                      name)


#### Create custom data labels
## For each chart (ie. explanatory variable),
##I create a unique label.
##For geom_text the labels must be stored in a dataframe


coeff<-data.frame(Name=NULL,
                  Sxx=NULL,
                  b1_sq=NULL,
                  SSreg=NULL,
                  phrase=NULL,
                  x=NULL,
                  y=NULL,
                  flag=NULL)

##This loop creates the customised data labels
##The only trick is to include a column with the same name
##and class as the facet variable

for (i in 1:length(name)){
  options(digits=5)
  #Calculate the coefficients to plot
  z<-lm(data$pricel~data[,name[i]])
  coeff[i,c("Name")]<-i
  coeff[i,c("Sxx")]<-sprintf("Sxx=%s",format(var(data[,name[i]])*(length(data[,name[i]])-1),scientific = TRUE, digits = 3))
  coeff[i,c("b1_sq")]<-sprintf("b1=%s",format(summary(z)$coefficients[2],scientific = TRUE, digits = 3))
  coeff[i,c("SSreg")]<-sprintf("SSreg=%s",format(anova(z)[1,2],scientific = TRUE, digits = 3))
  coeff[i,c("phrase")]<-paste(coeff[i,c("Sxx")],"\n",
                              coeff[i,c("b1_sq")],"\n",
                              coeff[i,c("SSreg")],
                              sep="")
  coeff[i,c("x")]<--Inf
  coeff[i,c("y")]<-Inf
  coeff[i,c("flag")]<-name[i]

}

##Need to ensure the flag variable is a factor
coeff$flag<-as.factor(coeff$flag)

####Create the facet plot
##Start with creating a ggplot object
p<-ggplot(stack,aes(y=pricel,x=variable))+
  stat_binhex()+
  scale_fill_gradient(low="lightblue", high="red")+
  geom_smooth(method=lm,se=TRUE)+
  theme(axis.text.x=element_text(angle=90))+
  guides(fill=FALSE)+
  labs(y="Log Value")

##Now create the facet plot with custom labels
#pdf("plot19.pdf")
#dev.off()
p+facet_wrap(~flag, scales="free_x",ncol=3,nrow=4)+
  geom_text(aes(x=Inf, y=Inf, label=SSreg),data=coeff,vjust=1.0,hjust=1.5)+
  geom_text(aes(x=Inf, y=Inf, label=b1_sq),data=coeff,vjust=2.5,hjust=1.65)+
  geom_text(aes(x=Inf, y=Inf, label=Sxx),data=coeff,vjust=4.0,hjust=1.6)
#dev.off()

####Checking the quality of the data fit
#Normality Assumptions
q<-ggplot(stack,aes(sample=pricel))+stat_qq()+mytheme

pdf("plot24.pdf")
q+facet_wrap(~flag,ncol=3,nrow=4)
dev.off()

##Constant Variance
## Populate a data-frame with the residuals from the model
data3<-matrix(nrow=length(data$pricel),ncol=length(name))

for (i in 1:length(name)) {
  z<-lm(data$pricel~data[,c(name[i])])
  data3[,i]<-residuals.lm(z,type=c("working"))
}

##Stack the residuals into a column vector
##Needed for facet plots
a=NULL
b=NULL

for (i in 1:length(name)){
  a<-c(a,data3[,i])
  b<-c(b,rep(i,length(data$pricel)))
  c<-seq(1,length(data3[,1]))
                          }

data4<-data.frame(
  residual=c(a),
  flag=c(b),
  id=c(c)
                )


##Create a factor variable for facetting
data4$flag<-as.factor(data4$flag)
data4$flag<-mapvalues(data4$flag,as.character(1:12),name)

##Create ggplot object
p<-ggplot(data4,aes(y=residual,x=id))+
  stat_binhex()+
  scale_fill_gradient(low="lightblue", high="red")+
  guides(fill=FALSE)+
  labs(y="Residual (Log Value)")+
  mytheme+
  theme(axis.text.x=element_blank())

pdf("plot25.pdf")
  p+facet_wrap(~flag,ncol=3,nrow=4)
dev.off()

