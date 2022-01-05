
## Quiz 1 Cleaning Data
##  Q1
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl,destfile = "~/Desktop/test/QuizQ1", method = "curl")
QuizQ1 <- read.csv("~/Desktop/test/QuizQ1")
QuizQ1$VAL==24    
sum(QuizQ1$VAL==24) & sum(!is.na(QuizQ1$VAL))
ActualData<- QuizQ1$VAL[!is.na(QuizQ1$VAL)]

## Q3
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile= "/Users/shadi/Desktop/test/QuizQ3.xlsx", method = "curl",mode="wb"
install.packages("openxlsx")
library("openxlsx")

dat <- read.xlsx("/Users/shadi/Desktop/test/QuizQ3.xlsx",
                    sheet = 1, 
                    , rows=18:23 , cols= 7:15)

sum(dat$Zip*dat$Ext,na.rm=T)

## 2 problem with this  question 
# a) xlsx package need a java installed in R if you don't want  to download it 
#then use openxlsx package but then b) keep in mind that the format of entering
#argunment in read.xlsx would not be the same a read.xlsx for xlsx package and you
# need to read the help in order to prevent any error.

## Q4 

install.packages("XML")
library(XML)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
xmlTreeParse(fileUrl, destfile = getwd)


## Q5

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

download.file(fileUrl,destfile = "/Users/shadi/Desktop/test/Q5.csv")


## to Use Fread function 

install.packages("data.table")

library(data.table)

DT<- fread ("/Users/shadi/Desktop/test/Q5.csv" )

Head(DT)















