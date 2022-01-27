
## first Question

storm_data<-read.csv("repdata-data-StormData.csv")

library(dplyr)
library(ggplot2)


str(storm_data)

health<- subset(storm_data,!storm_data$FATALITIES == 0 & !storm_data$INJURIES == 
                    0 , select=c(EVTYPE, FATALITIES, INJURIES))



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


health$TOTAl_Health_Damage<-health$FATALITIES+health$INJURIES

final_health<-health %>% group_by(EVTYPE) %>% summarise_all(list(sum))


dool[which.max(dool$FATALITIES),]

shoole<-doole[order(doole$harm,decreasing =TRUE),]
hoole<-doole[order(doole$EVTYPE,decreasing =TRUE),]

c<-final_health[order(final_health$TOTAl_Health_Damage,decreasing =TRUE),]
final_health[order(final_health$INJURIES,decreasing =TRUE),]


## Second Question

# subseting
table(Econ_damage$PROPDMGEXP)

Econ_damage<- subset(storm_data, !PROPDMG==0 & !CROPDMG==0 , select=c(EVTYPE, PROPDMG, PROPDMGEXP,CROPDMG,CROPDMGEXP))

# fixing human errors.

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
Econ_damage$EVTYPE <- gsub("^FROST[/\\]FREEZE$|^FROST$|^(DAMAGING)? ?FREEZE$|^HYP[OE]R?THERMIA.*|^ICE$|^(ICY|ICE) ROADS$|^BLACK ICE$|^ICE ON ROAD$",
                      "FROST/FREEZE", Econ_damage$EVTYPE)
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

##conversion


Econ_damage$PROPDMGEXP<-gsub("[+]",1, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[0-8]",10, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[hH]",100, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[kK]",1000, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[mM]",1000000, Econ_damage$PROPDMGEXP)
Econ_damage$PROPDMGEXP<-gsub("[bB]",1000000000, Econ_damage$PROPDMGEXP)



Econ_damage$CROPDMGEXP<-gsub(^"",0, Econ_damage$CROPDMGEXP)
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


Econ_damage$totalprobdmg<-Econ_damage$PROPDMG*Econ_damage$PROPDMGEXP
Econ_damage$totalcrobdmg<-Econ_damage$CROPDMG*Econ_damage$CROPDMGEXP

Econ_damage$totaldamage<-Econ_damage$totalprobdmg+Econ_damage$totalcrobdmg

table(Econ_damage$PROPDMGEXP)
king<-Econ_damage %>% group_by(EVTYPE) %>% summarise_all(list(sum))
king[order(king$totalprobdmg,decreasing =TRUE),]
king[order(king$totalcrobdmg,decreasing =TRUE),]
b<-king[order(king$totaldamage,decreasing =TRUE),]




library(ggplot2)


qplot(FATALITIES,data=dool,fill=EVTYPE)



ggplot() + geom_bar(data = head(c), aes(x = EVTYPE, y = TOTAl_Health_Damage, fill = interaction(TOTAl_Health_Damage, 
         EVTYPE)), stat = "identity", show.legend = F) + theme(axis.text.x = element_text(angle = 30, 
                                         hjust = 1)) + xlab("Event Type") + ylab("Total Health Damage")





ggplot(data = head(population_data, 15), aes(x = factor(EVTYPE), y = (fatalities + injuries), fill = EVTYPE)) + 
    geom_bar(stat="identity") + coord_flip() + 
    labs(y = "Injuries and fatalities", x = "Event type", title = "Injuries and fatalites per event type across US")



library(knitr)














