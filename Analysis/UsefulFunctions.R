#################
# Useful Functions
# Created by: Ed Anderson
# Date: 09/04/2017
##################

# Radar Search With Google API
# Function contacts the Google API and returns local area information
# Example Query:
#
# RadarSearch(1,data2[,c("Lat","Long")],100,"AIzaSyC5dvHAt1QbcWtdIaDWHnLLUaAARJSSSPs","School")
#
# A key value needs to be obtained from here:
# Here is my key "AIzaSyC5dvHAt1QbcWtdIaDWHnLLUaAARJSSSPs"
# https://developers.google.com/maps/documentation/geocoding/get-api-key

## Load Packages

library(httr)

#Input1 is an integer value, it identifies the row in the data-frame
#Input2 is a data-frame, it contains the lat-long information of the properties
#Input3 is a numeric value. It is used for the radius of the search
#Input4 is the Google API key
#Input5 is the Place of Interest (eg. Restaurants)

RadarSearch<-function(Input1,Input2,Input3,Input4,Input5){

  #Declare some variables
  i<-c(0)
  LatLon<-c(0)
  Radius<-c(0)
  Type<-c(0)
  Key<-c(0)

  #Populate the variables
  i<-Input1
  LatLong<-paste(Input2[i,1],",",Input2[i,2],sep="")
  Radius<- paste(Input3)
  Type<- Input5
  Key<-Input4


  #Build an API query and store the results
  sample2<-GET("https://maps.googleapis.com/maps/api/place/nearbysearch/json",
               query=list(location=LatLong,
                          radius=Radius,
                          #type=typ,
                          types=Type,
                          #keyword=keyw,
                          key=Key))

  #Output the number of successful search results
  length(content(sample2)$results)

}


######################################

# Reverse Geocoding function with Google API
# This function takes lat,long location information and returns address information

# Input1 is an integer and is used to select a row of data
# Input2 a data-frame containing three columns: TransNo (pkey) latitude, longitude
# Input3 is the value of the google API key

# The keys which I used are given at the end of the function:

ReverseGeo<-function(Input1,Input2,Input3){


  #initialise some variables
  j<-c(0)
  k<-c(0)
  LatLng<-c(0)
  KeyVal<-c(0)
  Output<-c(0)
  Output2<-rep(NA,each=7)

  #Put the inputs into dummy variables
  j<-Input1
  LatLng<-paste(Input2[j,2],",",Input2[j,3],sep="")
  KeyVal<-Input3

  #Use HTTR to query google and store the results
  sample2<-GET("https://maps.googleapis.com/maps/api/geocode/json",
               query=list(latlng=LatLng,
                          key=KeyVal))

  #Parse the results using httr
  Output<-content(sample2)

  #Parse the response again
  #Ensures the information is placed in the correct order

  for (k in 1:(min(7,length(Output$results[[1]]$address_components))))

    {

      if (Output$results[[1]]$address_components[[k]]$types[[1]]=="street_number")
          {Output2[1]<-strsplit(
                          Output$results[[1]]$address_components[[k]]$long_name,
                          "-")[[1]][1]}

      if (Output$results[[1]]$address_components[[k]]$types[[1]]=="route")
          {Output2[2]<-Output$results[[1]]$address_components[[k]]$long_name}

      if (Output$results[[1]]$address_components[[k]]$types[[1]]=="locality")
          {Output2[3]<-Output$results[[1]]$address_components[[k]]$long_name}

      if (Output$results[[1]]$address_components[[k]]$types[[1]]=="administrative_area_level_2")
          {Output2[4]<-Output$results[[1]]$address_components[[k]]$long_name}

      if (Output$results[[1]]$address_components[[k]]$types[[1]]=="administrative_area_level_1")
          {Output2[5]<-Output$results[[1]]$address_components[[k]]$long_name}

      if (Output$results[[1]]$address_components[[k]]$types[[1]]=="country")
          {Output2[6]<-Output$results[[1]]$address_components[[k]]$long_name}

      if (Output$results[[1]]$address_components[[k]]$types[[1]]=="postal_code")
          {Output2[7]<-Output$results[[1]]$address_components[[k]]$long_name}
  }

  #Return the parsed address details
  Output2

}

# Keys

# "AIzaSyADJ1pDacwBp_gfNO91-iriGOpSVXEiZEc"
# "AIzaSyDM4RczKIBMu6jA5lJFlDyi5SnF2ptmvxw"
#"AIzaSyDyhJhgRaWD3Db7xfPguQRck9eURUbz3T0"
#"AIzaSyBx6CMYUuXGmqEocmwaxK9cQEIlD_4il8I"
# "AIzaSyDmEFZ0CkgjL0fNY7cvBZRE7A79Sx4_AD0"
# "AIzaSyBlcnMdtqCfSf5f5zl-rNn6JLcQxwItjgU"
#"AIzaSyAvsfxbx_SKT5t-vDnYJmYyxDG4_TammHs"
#"AIzaSyCMBxCXXaU2-W7u2L3KigfT7cuzEviIFyQ"
#"AIzaSyD1jLcZDwCZmHl4z4oNX1Noois0oiylBbA"
#"AIzaSyAlYIxtZhLPkF7P8Rv9Tl1yZehh4BgoZuA"

#########################################################

# This function parses the response from the Zillow API in list format

Zillowf<-function(property) {

  #Create a local variable to strore the search results
  Output<-list(zpid=character(999),
               AddressStreet=character(999),
               AddressZipCode=character(999),
               AddressCity=character(999),
               AddressState=character(999),
               AddressLatitude=character(999),
               AddressLongitude=character(999),
               UseCode=as.character(999),
               YearBuilt=as.character(999),
               LotSizeSqFt=as.character(999),
               FinishedSizeSqFt=as.character(999),
               BathroomNo=as.character(999),
               BedroomNo=as.character(999),
               #LastSoldDate=character(999),
               #LastSoldPrice=character(999),
               ZestimateAmount=as.character(999),
               ZestimateLastUpdated=as.character(999),
               ZestimateOneWeekChange=as.character(999),
               ZestimateValueChange=as.character(999),
               ZestimateValueChangeDuration=as.character(999),
               ZestimateLowValueRange=as.character(999),
               ZestimateHighValueRange=as.character(999),
               ZestimateValuePercentile=as.character(999),
               RZestimateAmount=as.character(999),
               RZestimateLastUpdated=as.character(999),
               RZestimateOneWeekChange=as.character(999),
               RZestimateValueChange=as.character(999),
               RZestimateValueChangeDuration=as.character(999),
               RZestimateLowValueRange=as.character(999),
               RZestimateHighValueRange=as.character(999),
               RegionIndexValue=as.character(999),
               RegionAttributeName=as.character(999),
               RegionAttributeNeighbourhood=as.character(999),
               RegionAttributeID=as.character(999))

  # Loop through the list of results and place the data in appropriate place

  for (k in 1:length(row.names(property))){

    if (row.names(property)[k]=="zpid")
    {Output$zpid=property[[k]]}

    else if (row.names(property)[k]=="address")
    {Output$AddressStreet=property[[k]]$street
    Output$AddressZipCode=property[[k]]$zipcode
    Output$AddressCity=property[[k]]$city
    Output$AddressState=property[[k]]$state
    Output$AddressLatitude=property[[k]]$latitude
    Output$AddressLongitude=property[[k]]$longitude}

    else if (row.names(property)[k]=="useCode")
    {
      Output$UseCode=property[[k]]
    }

    else if (row.names(property)[k]=="finishedSqFt")
    {
      Output$FinishedSizeSqFt=property[[k]]
    }
    else if (row.names(property)[k]=="bathrooms")
    {
      Output$BathroomNo=property[[k]]
    }
    else if (row.names(property)[k]=="bedrooms")
    {
      Output$BedroomNo=property[[k]]
    }

    else if (row.names(property)[k]=="zestimate")
    {

      if(length(property[[k]]$amount)!=1){Output$ZestimateAmount=property[[k]]$amount$text} else {Output$ZestimateAmount="999"}
      Output$ZestimateLastUpdated=property[[k]]$`last-updated`
      Output$ZestimateOneWeekChange=property[[k]]$oneWeekChange[[1]]
      if(!is.null(property[[k]]$valueChange$text)) {Output$ZestimateValueChange=property[[k]]$valueChange$text}
      if (!is.null(property[[k]]$valueChange$.attrs[[1]])) {Output$ZestimateValueChangeDuration=property[[k]]$valueChange$.attrs[[1]]}
      Output$ZestimateLowValueRange=property[[k]]$valuationRange[[1]]
      if (length(property[[k]]$valuationRange)>2) {Output$ZestimateHighValueRange=property[[k]]$valuationRange[[3]]} else {Output$ZestimateHighValueRange="999"}
      Output$ZestimateValuePercentile=property[[k]]$percentile[[1]]
    }
    else if (row.names(property)[k]=="rentzestimate")
    {
      Output$RZestimateAmount=property[[k]]$amount$text
      Output$RZestimateLastUpdated=property[[k]]$`last-updated`
      Output$RZestimateOneWeekChange=property[[k]]$oneWeekChange[[1]]
      Output$RZestimateValueChange=property[[k]]$valueChange$text
      Output$RZestimateValueChangeDuration=property[[k]]$valueChange$.attrs[[1]]
      Output$RZestimateLowValueRange=property[[k]]$valuationRange[[1]]
      Output$RZestimateHighValueRange=property[[k]]$valuationRange[[3]]
    }

    else if (row.names(property)[k]=="localRealEstate")
    {
      Output$RegionIndexValue="999" #property[[k]][[1]]
      Output$RegionAttributeName="999" #property[[k]][[3]][[1]]
      Output$RegionAttributeNeighbourhood="999" #property[[k]][[3]][[3]]
      Output$RegionAttributeID="999" #property[[k]][[3]][[2]]
    }

  }

  Output



}


