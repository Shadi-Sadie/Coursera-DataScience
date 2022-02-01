stormdata<-read.csv("repdata-data-StormData.csv",header = TRUE)
library(leaflet)

storm<-stormdata

storm$EVTYPE<-gsub("TORNADO","TORNADO",storm$EVTYPE)


nstormdata<-subset(storm,   storm$EVTYPE=="TORNADO", select=c(STATE,EVTYPE, FATALITIES,LONGITUDE,LATITUDE)) 



nstrom<-nstormdata[complete.cases(nstormdata),] 
nstrom$LATITUDE<-nstrom$LATITUDE/100
nstrom$LONGITUDE<-nstrom$LONGITUDE/-100
astrom<-nstrom[!nstrom$FATALITIES==0,]





astrom %>%
    leaflet() %>%
    addTiles() %>%
    addCircles(weight = 4, radius = sqrt(astrom$FATALITIES) * 100, group=astrom$EVTYPE )
    

astrom %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(clusterOptions = markerClusterOptions())

    


