
######## Reading Data in two diffrent ways ##########

## : 1- Reading and subsetting at the same time using grep one could also use SQLDF package

colNames<-read.table("household_power_consumption.txt",sep = ";", stringsAsFactors = FALSE, na.strings = "?", nrow=1)


PowerData<-read.table("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = "?", col.names = colNames,
                      skip=(grep("1/2/2007", readLines("household_power_consumption.txt"))[1]-2), 
                      nrows = (grep("3/2/2007" , readLines("household_power_consumption.txt"))[1]) - 
                          grep("1/2/2007" ,readLines("household_power_consumption.txt"))[1])

                            
## 2- Reading and subseting sepreatly 
## Reading table                             
                                                        
power_consm<-read.table("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = "?" )
                            
# converting date                            

power_consm$Date<-as.Date(power_consm$Date, "%d/%m/%Y")       

# subseting the data for two days

power_consm<-power_consm[which("2007-02-01"<=power_consm$Date & power_consm$Date<"2007-02-03"),]

x<-paste(PowerData$Date,PowerData$Time)
PowerData$Datetime<-strptime(x, "%d/%m/%Y %H:%M:%S")       
PowerData$Date<-as.Date(PowerData$Date, "%d/%m/%Y")       


######## Creating Plots ##########


# plot 1 

jpeg("Plot results/plot1.jpg", width = 450, height = 450)

hist(PowerData$Global_active_power,col = "red",main = "Global Active Power", xlab="Global active power(kilowatts)")

dev.off()


# plot 2

jpeg("Plot results/plot2.jpg", width = 450, height = 450)

plot(PowerData$Datetime, PowerData$Global_active_power, type = "l", 
     ylab="Global Active Power(kilowatts)",
     xlab=""
     )
dev.off()


# plot 3

jpeg("Plot results/plot3.jpg", width = 450, height = 450)

plot(PowerData$Datetime, PowerData$Sub_metering_1, type = "l",
     ylab="Energy sub metering",
     xlab=""
     )
lines(PowerData$Datetime, PowerData$Sub_metering_2, col="red")
lines(PowerData$Datetime, PowerData$Sub_metering_3, col="blue")
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),
       col=c("black", "red", "blue"),bty="n", lwd=2)

dev.off()


# plot 4


jpeg("Plot results/plot4.jpg", width = 450, height = 450)


par(mfrow=c(2,2))

#1
plot(PowerData$Datetime, PowerData$Global_active_power, type = "l", 
     ylab="Global Active Power(kilowatts)",
     xlab="")
#2
plot(PowerData$Datetime, PowerData$Voltage, type = "l", ylab="Voltage")

#3
plot(PowerData$Datetime, PowerData$Sub_metering_1, type = "l",
     ylab="Energy sub metering",
     xlab=""
)
lines(PowerData$Datetime, PowerData$Sub_metering_2, col="red")
lines(PowerData$Datetime, PowerData$Sub_metering_3, col="blue")
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),
       col=c("black", "red", "blue"),bty="n", lwd=2)

#4
plot(PowerData$Datetime, PowerData$Global_reactive_power, type = "l", ylab="Global_reactive_power")


dev.off()



