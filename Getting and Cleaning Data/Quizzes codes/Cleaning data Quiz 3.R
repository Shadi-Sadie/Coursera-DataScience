## Cleaning Data Quiz 3

##  Question  1

## creat logical vector that identifies the households on greater than 10 acres 
##who sold more than $10,000 worth of agriculture products. 
#Assign that logical vector to the variable agricultureLogical. 
#Apply the which() function like this to identify the rows
# of the data frame where the logical vector is TRUE.


 fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
 download.file(fileUrl, destfile = "/Users/shadi/Desktop/test/q1.csv" , "curl")
 q1 <- read.csv("/Users/shadi/Desktop/test/q1.csv")
 str(q1) 
 agricultureLogical<- which(q1$ACR == 3 & q1$AGS ==6)
head(agricultureLogical,3)


##Q2 

# Read JPEG and report quntile :D

install.packages("jpeg")
library(jpeg)

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl,  "/Users/shadi/Desktop/test/q2.jpeg", "curl")
q2<-readJPEG("/Users/shadi/Desktop/test/q2.jpeg", TRUE )
quantile(q2,prob= c(0.3, 0.8))


## Q3 

#reading first data

#### Problem : I shouldn't sort them based on their gdp instead I should have used their gdp rank 
#### which I denoted as Obs by mistake. 
install.packages('dplyr')
library(dplyr)
gdp<- read.csv("/Users/shadi/Desktop/test/quiz4q2.csv",skip = 4,nrow=190)
gdp<-select (gdp, X:X.4)
colnames(gdp)<-c("CountryCode", "Obs", "N","Country","gdp")
head(as.integer(gdp$gdp))
## reading educatin data
edu<-read.csv("/Users/shadi/Desktop/test/quiz4q4.csv")

## mixing them with their country code 
all<- merge(gdp,edu,by="CountryCode")

y<- all[order(all$Obs,decreasing = TRUE),]

y$Country[13]


## Q4
## find mean for obs when incom= high oecd

nall<-all[which(all$Income.Group=="High income: OECD"),]
mean(nall$Obs)
|## find mean for obs when incom= high non oecd
      
xall<- all[which(all$Income.Group=="High income: nonOECD"),]
mean(xall$Obs)


## Q5

nqant<-quantile(all$Obs)
View(nqant)


