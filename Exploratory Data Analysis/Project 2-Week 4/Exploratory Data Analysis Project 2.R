library(dplyr)
library(ggplot2) 

SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
PM25<- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")


# Plot 1 :   Have total emissions from PM2.5 decreased in the United States from 999 to 2008? 
             # Using the base plotting system, make a plot showing the total PM2.5 emission 
             # from all sources for each of the years 1999, 2002, 2005, and 2008.
    # Solution 1

TotalEMS<-tapply(PM25$Emissions, PM25$year,sum) 

png("Plot results/plot1.png", width=480, height=480)
barplot(TotalEMS, main = "Total emission by year", ylab = "PM25 emission", col= heat.colors(8))
dev.off()

    # Solution 2

Q1<-PM25 %>% group_by(year) %>% summarise(Emission=sum(Emissions))  

png("plot1.png", width=480, height=480)  
barplot(Emission~year, data=Q1, col = rainbow(20), main ="Total PM2.5 emission for each years ")  
dev.off()


# Plot 2 :  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
            #from 1999 to 2008? Use the base plotting system to make a plot answering this question.

    # Solution 1

MRPM25<-PM25[PM25$fips == "24510",]
MREMS<-tapply(MRPM25$Emissions, MRPM25$year,sum) 

png("Plot results/plot2.png", width=480, height=480)  
barplot(MREMS, main = "Maryland Total emission by year", ylab = "PM25 emission", col= rainbow(12))
dev.off()

    # Solution 2

subNEI<- subset(PM25,fips == "24510") 
Q2<-subNEI %>% group_by(year) %>% summarise(Emission=sum(Emissions)) 

png("plot2.png", width=480, height=480) 
barplot(Emissions~year, data=Q2, col = rainbow(8),main ="Total PM2.5 emission for each years in Maryland ") 
dev.off()



# Plot 3 : Of the four types of sources indicated by the type(point, nonpoint, onroad, nonroad) variable, 
           # which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
           # Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
            #a plot answer this question.

    # Solution 1
P4data <- filter(PM25, fips == '24510') %>% group_by(year, type) %>% summarise(totalEmit = sum(Emissions))

png("Plot results/plot3.png", width=480, height=480) 

g <-ggplot(P4data, aes(year,totalEmit,color= type))
g+geom_line()

dev.off()

    # Solution 2
subNEI<- subset(NEI,fips == "24510")  
Q3<-subNEI %>% group_by(year,type) %>% summarise(Emission=sum(Emissions)) 

png("plot3.png", width=600, height=600) 

ggplot(data=Q3,aes( x=factor(year),y=Emission,fill = type))+ 
geom_bar( stat="identity") + facet_grid(. ~ type ) + xlab("Year")+ ylab("Total emission") + ggtitle("Emission by type for maryland") 

dev.off()


# Plot 4 : Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

place<-SCC$SCC[grep("[Cc]oal", SCC$EI.Sector)]
Q4data<-filter(PM25, SCC %in% place ) %>% group_by(year) %>% summarise(TotEmission=sum(Emissions)) 

png("Plot results/plot4.png", width=480, height=480) 
g<-ggplot(data=Q4data,aes(x=factor(year),y=TotEmission)) 
g+geom_col(fill="green")+labs(y="Total emssion", x= "year")
dev.off()


# Plot 5 : How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
    
place<-SCC$SCC[grep("[Vv]ehicle", SCC$EI.Sector)]
Q5data<-filter(PM25, SCC %in% place & fips == "24510") %>% group_by(year) %>% summarise(TotEmission=sum(Emissions)) 

png("Plot results/plot5.png", width=480, height=480) 
g<-ggplot(data=Q5data,aes(x=factor(year),y=TotEmission)) 
g+geom_col(fill="green")+labs(y="Total emssion", x= "year")
dev.off()


# Plot 6 : Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
           # sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes 
           # over time in motor vehicle emissions?
    
    
Q6data<-filter(PM25, fips == "06037" | fips == "24510") %>% group_by(year,fips) %>% summarise(TotEmission=sum(Emissions)) 

png("Plot results/plot6-1.png", width=480, height=480) 
g<-ggplot(data=Q6data,aes(x=year,y=TotEmission, col=fips)) 
g+geom_line()+labs(y="Total emssion", x= "year")
dev.off()


png("Plot results/plot6-2.png", width=480, height=480) 
g<-ggplot(data=Q6data,aes(x=year,y=TotEmission)) 
g+geom_line()+facet_grid(fips ~ ., scale = 'free')+labs(y="Total emssion", x= "year")
dev.off()