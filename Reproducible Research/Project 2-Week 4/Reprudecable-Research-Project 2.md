---
title: "Impacts of sever weather events on public health and economics"
author: "Shadi"
date: "4/21/2020"
output: github_document
  
  html_document: default
  pdf_document: default
---


## Synopsis
In the Second Project of Reprudicble research, we deal with the data from NOAA.This database contains data of major storms and weather events in US as well as the location, fatality , injuries, property and crop damages.
In what follows are aim is to figure out which type of sever weather event has the most damage in terms of health and economics. To process this effect we need to transform and clean the data looking at the code book provide by NOAA there are 48 event type while when we look at the storm data set we face 985 event type this might happened because of the dictaion error or any other kind of human error. This would be a tedious work, one way to deal with this is to remove the events with zero fatality and damage. Afterward trying to fix the gramartical mistake.
ALong this property damage exp and crop damage exp are charachter vector with variables 0_8,hH,KK,Mm,bB which respectivily shown 10,100,1000,10^6,10^9 this vectors should be substitue with their numerical values so that we could continue the analysis. Conducting the analysis we could conclude that the Tornado had the greatest health impact measured by the number of injuries and fatalities. Moreover, The analysis also shows that floods cause the greatest economic impact as measured by property damage and crop damage.


## Data Processing

Data has been download previously and unziped. directory would be setup where the data have been download and placed, and data loaded from the directory

```{r loading data, echo=TRUE}

setwd("~/Desktop/test")
storm_data<-read.csv("repdata-data-StormData.csv")
```

The required package and libraries will be loaded


```{r loading libraries, echo=TRUE}

library(dplyr)
library(ggplot2)

```

On first Step of Processing data we take a look at the dimension and summaries of the data:

```{r loading libraries 2, echo=TRUE}

dim(storm_data)
str(storm_data)

```
Looking at data we see that we have 985 event type which is in contrast with the 48 event type we have in the provided data information from NOAA. This error might had happend because of human error. 
To deal with this error, In subseting I would remove the event with zero obserivation and then fix the gramatical mistakes.

There are two question to be answered so I need a sub dataset to answer each. I will name them health dataset and economics dataset 

1. Starting with subset for the health analysis.
```{r Health data, echo=TRUE}

health<- subset(storm_data,!storm_data$FATALITIES == 0 & !storm_data$INJURIES == 
    0 , select=c(EVTYPE, FATALITIES, INJURIES))

```
now we are trying to fix the error on the events 

```{r Health data error fix, echo=TRUE}

health$EVTYPE <- toupper(health$EVTYPE)
health$EVTYPE <- gsub("^EXTREME WIND ?CHILL$|^(EXTENDED|EXTREME|RECORD)? COLDS?$","EXTREME COLD/WIND CHILL", health$EVTYPE)

health$EVTYPE <- gsub("^FLOODS?.*|FLOOD(ING)?S?$","FLOOD", health$EVTYPE)
health$EVTYPE <- gsub("^HEAVY SURF(/HIGH SURF)?.*$|^(ROUGH|HEAVY) SEAS?.*|^ROUGH SURF.*|^HIGH WIND AND SEAS$|^HIGH SURF.*", 
                                   "HIGH SURF", health$EVTYPE)

health$EVTYPE <- gsub("^HEAT WAVES?$|^WARM WEATHER$", "HEAT", health$EVTYPE)
health$EVTYPE <- gsub("^GLAZE.*|^FREEZING (RAIN|DRIZZLE|RAIN/SNOW|SPRAY$)$|^WINTER WEATHER MIX$|^LIGHT SNOW$|^SLEET.*", 
                                    "SLEET", health$EVTYPE)

health$EVTYPE <- gsub("^RAIN/SNOW$|^(BLOWING|HEAVY||ICE AND|RECORD)? ?SNOWS?.*", "HEAVY SNOW", health$EVTYPE)
health$EVTYPE <- gsub("^EXCESSIVE RAINFALL$|^RAIN.*|^(HEAVY|HVY)? (RAIN|MIX|PRECIPITATION).*", "HEAVY RAIN", health$EVTYPE)
health$EVTYPE <- gsub("^WINDCHILL$|^COLD.*", "COLD/WIND CHILL", health$EVTYPE)
health$EVTYPE <- gsub("^FROST[/\\]FREEZE$|^FROST$|^(DAMAGING)? ?FREEZE$|^HYP[OE]R?THERMIA.*|^ICE$|^(ICY|ICE) ROADS$|^BLACK ICE$|^ICE ON ROAD$",
                      "FROST/FREEZE", health$EVTYPE)
health$EVTYPE <- gsub("^FOG.*", "FREEZING FOG", health$EVTYPE)
health$EVTYPE <- gsub("^HEAT WAVE DROUGHT?$", "HEAT", health$EVTYPE)
health$EVTYPE <- gsub("^HIGH WIND.*", "HIGH WIND", health$EVTYPE)
health$EVTYPE <- gsub("^STRONG WIND.*", "STRONG WIND", health$EVTYPE)
health$EVTYPE <- gsub("^RIP CURRENT.*", "RIP CURRENT", health$EVTYPE)
health$EVTYPE <- gsub("^WINTER STORM.*", "WINTER STORM", health$EVTYPE)
health$EVTYPE <- gsub("^WINTER WEATHER/MIX*", "WINTER WEATHER", health$EVTYPE)

health$EVTYPE <- gsub("TSTM|THUNDERSTORMS?", "THUNDERSTORM", health$EVTYPE)
health$EVTYPE <- gsub("^TH?UN?DEE?RS?TO?RO?M ?WIND.*|^(SEVERE )?THUNDERSTORM$|^WIND STORM$|^(DRY )?MI[CR][CR]OBURST.*|^THUNDERSTORMW$",
                      "THUNDERSTORM WIND", health$EVTYPE)
health$EVTYPE <- gsub("^WILD/FOREST FIRE$|^(WILD|BRUSH|FOREST)? ?FIRES?$", "WILDFIRE", health$EVTYPE)


```

Now  that our health data is ready we could start to find the fatality per event, to do so I am aslo adding another variable which is sum of the FATALITIES and INJURIES I will call it TOTAl_Health_Damage


```{r define new variable, echo=TRUE}

health$TOTAl_Health_Damage<-health$FATALITIES+health$INJURIES



```
Our health data is now ready for the further analysis, before starting with the analysis for health data we are going to do the same thing for the economic data, when we have both data set ready we could continue with the  analysis.

2. Moving to economic sub data set

Subseting the data set which dosen't have Zero obeservation
```{r Economics data, echo=TRUE}


Econ_damage<- subset(storm_data, !PROPDMG==0 & !CROPDMG==0 , select=c(EVTYPE, PROPDMG, PROPDMGEXP,CROPDMG,CROPDMGEXP))



```

Fixing the gramatical mistakes


```{r Economics data fixing error, echo=TRUE}

Econ_damage$EVTYPE <- toupper(Econ_damage$EVTYPE)

Econ_damage$EVTYPE <- gsub("^EXTREME WIND ?CHILL$|^(EXTENDED|EXTREME|RECORD)? COLDS?$","EXTREME COLD/WIND CHILL", Econ_damage$EVTYPE)

Econ_damage$EVTYPE <- gsub("^FLOODS?.*|FLOOD(ING)?S?$","FLOOD", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^HEAVY SURF(/HIGH SURF)?.*$|^(ROUGH|HEAVY) SEAS?.*|^ROUGH SURF.*|^HIGH WIND AND SEAS$|^HIGH SURF.*", 
                      "HIGH SURF", Econ_damage$EVTYPE)

Econ_damage$EVTYPE <- gsub("^HEAT WAVES?$|^WARM WEATHER$", "HEAT", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^GLAZE.*|^FREEZING (RAIN|DRIZZLE|RAIN/SNOW|SPRAY$)$|^WINTER WEATHER MIX$|^LIGHT SNOW$|^SLEET.*", 
                      "SLEET", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^RAIN/SNOW$|^(BLOWING|HEAVY||ICE AND|RECORD)? ?SNOWS?.*", "HEAVY SNOW", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^EXCESSIVE RAINFALL$|^RAIN.*|^(HEAVY|HVY)? (RAIN|MIX|PRECIPITATION).*", "HEAVY RAIN", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^WINDCHILL$|^COLD.*", "COLD/WIND CHILL", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^FROST[/\\]FREEZE$|^FROST$|^(DAMAGING)? ?FREEZE$|^HYP[OE]R?THERMIA.*|^ICE$|^(ICY|ICE) ROADS$|^BLACK ICE$|^ICE ON                            ROAD$","FROST/FREEZE", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^FOG.*", "FREEZING FOG", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^HEAT WAVE DROUGHT?$", "HEAT", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^HIGH WIND.*", "HIGH WIND", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^STRONG WIND.*", "STRONG WIND", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^RIP CURRENT.*", "RIP CURRENT", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^WINTER STORM.*", "WINTER STORM", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^WINTER WEATHER/MIX*", "WINTER WEATHER", Econ_damage$EVTYPE)

Econ_damage$EVTYPE <- gsub("TSTM|THUNDERSTORMS?", "THUNDERSTORM", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^TH?UN?DEE?RS?TO?RO?M ?WIND.*|^(SEVERE )?THUNDERSTORM$|^WIND STORM$|^(DRY )?MI[CR][CR]OBURST.*|^THUNDERSTORMW$",
                      "THUNDERSTORM WIND", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^WILD/FOREST FIRE$|^(WILD|BRUSH|FOREST)? ?FIRES?$", "WILDFIRE", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^FLOODS?.*|^URBAN/SML STREAM FLD$|^(RIVER|TIDAL|MAJOR|URBAN|MINOR|ICE JAM|RIVER AND STREAM|URBAN/SMALL STREAM)? 
                      FLOOD(ING)?S?$|^HIGH WATER$|^URBAN AND SMALL STREAM FLOODIN$|^DROWNING$|^DAM BREAK$", "FLOOD", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^FLASH FLOOD.*|^RAPIDLY RISING WATER$", "FLASH FLOOD", Econ_damage$EVTYPE)

Econ_damage$EVTYPE <- gsub("^TORNADO.*", "TORNADO", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^WHIRLWIND$|^GUSTNADO$|^TORNDAO$", "TORNADO", Econ_damage$EVTYPE)

Econ_damage$EVTYPE <- gsub("^HURRICANE.*", "HURRICANE/TYPHOON", Econ_damage$EVTYPE)
Econ_damage$EVTYPE <- gsub("^TYPHOON", "HURRICANE/TYPHOON", Econ_damage$EVTYPE)

Econ_damage$EVTYPE <- gsub("^TROPICAL STORM.*", "TROPICAL STORM", Econ_damage$EVTYPE)

Econ_damage$EVTYPE <- gsub("^(SMALL )?HAIL.*", "HAIL", Econ_damage$EVTYPE)



```

if we take a look at data we see that for both PROPDMGEXP and CROPDMGEXP we have the unit written as H,K,B we want to fix that to it's numerical value as we remove the values that we didn't have any observation now we don't have any values for ? and - so we could skip them in our conversion.

```{r Data conversion-s, echo=TRUE}
Econ_damage$PROPDMGEXP<-gsub("[+]",1, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[0-8]",10, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[hH]",100, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[kK]",1000, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[mM]",1000000, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[bB]",1000000000, Econ_damage$PROPDMGEXP)
Econ_damage$CROPDMGEXP<-gsub("[+]",1, Econ_damage$CROPDMGEXP)
Econ_damage$CROPDMGEXP<-gsub("[0-8]",10, Econ_damage$CROPDMGEXP)
Econ_damage$CROPDMGEXP<-gsub("[hH]",100, Econ_damage$CROPDMGEXP)
Econ_damage$CROPDMGEXP<-gsub("[kK]",1000, Econ_damage$CROPDMGEXP)
Econ_damage$CROPDMGEXP<-gsub("[mM]",1000000, Econ_damage$CROPDMGEXP)
Econ_damage$CROPDMGEXP<-gsub("[bB]",1000000000, Econ_damage$CROPDMGEXP)
Econ_damage$PROPDMGEXP<-as.numeric(Econ_damage$PROPDMGEXP)
Econ_damage$CROPDMGEXP<-as.numeric(Econ_damage$CROPDMGEXP)
Econ_damage$PROPDMGEXP[is.na(Econ_damage$PROPDMGEXP)]<-0
Econ_damage$CROPDMGEXP[is.na(Econ_damage$CROPDMGEXP)]<-0
 Econ_damage$PROPDMGEXP<-as.numeric(Econ_damage$PROPDMGEXP)
Econ_damage$CROPDMGEXP<-as.numeric(Econ_damage$CROPDMGEXP)
```

define two variable total property damage and total crop damage  

```{r  new variable, echo=TRUE}

Econ_damage$totalprobdmg<-Econ_damage$PROPDMG*Econ_damage$PROPDMGEXP
Econ_damage$totalcrobdmg<-Econ_damage$CROPDMG*Econ_damage$CROPDMGEXP
Econ_damage$totaldamage<-Econ_damage$totalprobdmg+Econ_damage$totalcrobdmg


```


Now are economics data set is also ready for the analysis.

## Results

First we want to see the result for the first question health to do so we start by grouping the fatalitity,injuries and total harm  per event

```{r group by event, echo=TRUE}

 final_health <- health %>% group_by(EVTYPE) %>% summarise_all(list(sum))


```
Then we could do either sort the data or we could find the max value and then look at the event type for that specific max value.
we will do both way.

```{r find the max , echo=TRUE}


final_health[which.max(final_health$FATALITIES),]
final_health[which.max(final_health$INJURIES),]
final_health[which.max(final_health$TOTAl_Health_Damage),]
```
or 
```{r finding max , echo=TRUE}
Health_Result<-final_health[order(final_health$TOTAl_Health_Damage,decreasing =TRUE),]
final_health[order(final_health$FATALITIES,decreasing =TRUE),]
final_health[order(final_health$INJURIES,decreasing =TRUE),]

```

We see that the event types with the big impact on public health are: TORNADO, EXCESSIVE HEAT, FLASH FLOOD
It is the same among injuries and fatalities.

for economics impact we do the same analysis 



```{r grouping , echo=TRUE}

 
Final_econ <- Econ_damage %>% group_by(EVTYPE) %>% summarise_all(list(sum))


```


for finding the event with maximum damage :

```{r find  max , echo=TRUE}

Final_econ[order(Final_econ$totalprobdmg,decreasing =TRUE),]
Final_econ[order(Final_econ$totalcrobdmg,decreasing =TRUE),]
Econ_Result<-Final_econ[order(Final_econ$totaldamage,decreasing =TRUE),]


```
or 
```{r find a max, echo=TRUE}

Final_econ[which.max(Final_econ$totalprobdmg),]
Final_econ[which.max(Final_econ$totalcrobdmg),]


```

looking at the result we see that flood has maximum damage on the property and Thyphon has the maximum damage on crops looking the totol economic damage flood is the winner.




Just to make the result clear we take a look at the graph for each of the questions

## Figures 

graph for the top total health damage :

```{r graph-a, echo=TRUE}

ggplot() + geom_bar(data = head(Health_Result), aes(x = EVTYPE, y = TOTAl_Health_Damage, fill = interaction(TOTAl_Health_Damage, 
         EVTYPE)), stat = "identity", show.legend = F) + theme(axis.text.x = element_text(angle = 30, 
                                         hjust = 1)) + xlab("Event Type") + ylab("Total Health Damage")
```
graph for total economic damage :

    
    
```{r graph-b, echo=TRUE}
  
    
ggplot() + geom_bar(data = head(Econ_Result), aes(x = EVTYPE, y = totaldamage, fill = interaction(totaldamage, 
         EVTYPE)), stat = "identity", show.legend = F) + theme(axis.text.x = element_text(angle = 30, 
                                         hjust = 1)) + xlab("Event Type") + ylab("Total Damage")


```
