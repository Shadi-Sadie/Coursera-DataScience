## Quiz4 

#Q1

If (!file.exists(“data”)) { 
  dir.create(“data”)
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "/Users/shadi/Desktop/test/quiz4q1.csv", method = "curl")
quiz4q1<-read.csv("/Users/shadi/Desktop/test/quiz4q1.csv")
split<-strsplit(names(quiz4q1),"wgtp")
split[[123]]

#Q2
 
##Removing commas
#some learning : first look at the data file not here in their fil 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl,"/Users/shadi/Desktop/test/quiz4q2.csv","curl")
# runing this code I got an error that num col are more than col.names which make sense cause there are
# some col which are just space 
#q2<- read.csv("/Users/shadi/Desktop/test/quiz4q2.csv",skip = 5,col.names = c("code","Num","Country","GDP") )
#this time I try to only select the gdp lest see if it's posible'
q2<- read.csv("/Users/shadi/Desktop/test/quiz4q2.csv",skip = 4,nrow=190)
q2<-select(q2, X:X.4)

x<-gsub(",", "", q2[,5])
head(x)
gdp<- as.numeric(x)
mean(gdp,,na.rm=TRUE)


##Q3
length(grep("^United", q2[,4]))
head(q2$X.3)

##Q4
colnames(q2)<-c("CountryCode", "Obs", "N","Country","Code")
fileUrl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl,destfile = "/Users/shadi/Desktop/test/quiz4q4.csv",method = "curl")
q4<-read.csv("/Users/shadi/Desktop/test/quiz4q4.csv")
head(q4,3)
all <- merge(q2, q4, by = "CountryCode")





