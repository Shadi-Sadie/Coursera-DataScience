## data from MySQL

install.packages("RMySQL")
install.packages("DBI")
library(RMySQL)


## dbConnect opens a connection
ucscDb <- dbConnect(MySQL(),user="genome", 
                   host="genome-mysql.soe.ucsc.edu", dbname="hgcentral")
## this function moves You to the connection and allows you to run a SQL command
result <- dbGetQuery(ucscDb,"show databases;");
## make sure when you are done get disconnect from connection the code below 
# allows you to do so
   dbDisconnect(ucscDb);
    result
   
## if you want to have an access to particular database instead of the whole 
## we should add db argument to our dbconnect command
hg19 <- dbConnect(MySQL(),user="genome", db="hg19",
                    host="genome-mysql.soe.ucsc.edu")
## what is the dbname ="hgcentral"

##listing the table

alltables <- dbListTables(hg19)
length(alltables)

## function for Reading from a table 
 
shasho <- dbReadTable(hg19,"affyU133Plus2")
head(shasho)


## Select a speecfic subset; 

query <- dbGetQuery(hg19 ,"select * from affyU133Plus2 WHERE misMatches between 1 and 3;");
 
affyMisSmall <-fetch(query,10);
dbClearResult(query)
dbDisconnect(hg19);


#### Reading FROM HDF5 

### API 



#Quiz Q1

install.packages("httr")
library(httr)
myapp <-oauth_app ("github", key="05c1d6b057e74cdaca2b", secret="a8391199f67b2872d139d52ffa8f7f71994d77bc")
sign <- sign_oauth1.0(myapp, token = "",token_secret = "fcf8899cb97cbfbbda2b1c7f06b304117f3aea71")
req <- GET("https://api.github.com/users/jtleek/repos", sign)

stop_for_status(req)
json1<-content(req)

# first way
datashare <- which(sapply(json1, FUN=function(X) "datasharing" %in% X))
datashare
list(json1[[17]]$name, json1[[17]]$created_at)

# second way
json2 <- jsonlite::fromJSON(jsonlite::toJSON(json1))

json2[json2$full_name == "jtleek/datasharing", "created_at"] 

## Q4 reading HTML by readline

con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode <- readLines(con)
close(con)

x<-htmlCode[c(10,20,30,100)]
nchar(x)

## Q5 reading fixed format file .for are fixed format 

x<- url("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for")
y <- read.fwf(x,widths)
head(y)














