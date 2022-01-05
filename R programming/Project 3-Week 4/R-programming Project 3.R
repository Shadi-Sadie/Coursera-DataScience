

###### Summary #####
# Reading the data from directory 
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# Looking at the first few rows.
head(outcome) 
#Checking the number of columns
ncol(outcome)
#Checking the names of all columns
names(outcome)

### Discriptive #####
# Looking at the histogram of the 30-day death rates from heart attack ( it is column 11)
data[, 11] <- as.numeric(data[, 11])
outcome[, 11] <- as.numeric(outcome[, 11])
## may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])




### Functions #####
data<-outcome
## 1- Function  :  Finding the best hospital in the state using a function 

best<-function (state, outcome) {
   # Read the data
    # Making the char data to numeric
    for (i in c(11,17,23) ){
        data[,i] <- as.numeric(data[,i], na.rm = TRUE)
    }
    # changing the data name to make it easier to deals with 
     names(data)[c(11,17,23)]<-c("heart attack", "heart failure", "pneumonia")
    # now the returning procedure
        # first check if the names are valid 
     if (!(state %in% data[,"State"])) { 
         stop("invalid state")
     } else if (!(outcome %in% names(data)[c(11,17,23)])) { 
         stop ("invalid outcome") 
     } else  
         # continue with actual procedure 
    data<-data[order(data[,"State"],data[,outcome]),] 
    cleandata<-data[which(data[,"State"]==state),2]
    cleandata[1]
}

# Testing the function result 
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

# 2- Function :  Ranking hospitals by outcome in a state

rankhospital<-function (state, outcome,j) {
    # Read the data
    # Making the char data to numeric
    for (i in c(11,17,23) ){
        data[,i] <- as.numeric(data[,i], na.rm = TRUE)
    }
    # changing the data name to make it easier to deals with 
    names(data)[c(11,17,23)]<-c("heart attack", "heart failure", "pneumonia")
    # now the returning procedure
    # first check if the names are valid 
    if (!(state %in% data[,"State"])) { 
        stop("invalid state")
    } else if (!(outcome %in% names(data)[c(11,17,23)])) { 
        stop ("invalid outcome") 
    } else  
        # continue with actual procedure 
        data<-data[order(data[,"State"],data[,outcome],data$Hospital.Name,na.last = NA),] 
      cleandata<-data[which(data[,"State"]==state),2]
    if (j=="best") {
        cleandata[1]
    } else if (j=="worst") {
        cleandata[length(cleandata)]
        }
    else
     (cleandata[j])
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")

# 3- Function :  Ranking hospitals in all states

rankall<-function (outcome, num ="best") {
    # Read the data
    # Making the char data to numeric
    for (i in c(11,17,23) ){
        data[,i] <- as.numeric(data[,i], na.rm = TRUE)
    }
    # changing the data name to make it easier to deals with 
    names(data)[c(11,17,23)]<-c("heart attack", "heart failure", "pneumonia")
    # now the returning procedure
    # first check if the names are valid 
    if (!(outcome %in% names(data)[c(11,17,23)])) { 
        stop ("invalid outcome") 
    } else  
        # continue with actual procedure 
        data<-data[order(data$State,data[,outcome],data$Hospital.Name),] 
            rankedresult<-data[, c("State","Hospital.Name",outcome)]
        splited<-split(rankedresult,rankedresult$State)
        bw=data.frame()
        cw=data.frame()
        for (i in 1:length(splited)){
            if (num=="best") {
                var<-1
            } else if (num=="worst") {
                var<-sum(!is.na(splited[[i]][3]))
            }else {
                var<-num
            }
            cw<-rbind(cw,splited[[i]][1,1], stringsAsFactors = FALSE)
            bw <-rbind(bw,splited[[i]][var,2],stringsAsFactors = FALSE )
        }
        nw<-cbind(bw,cw) 
        colnames(nw)<-c("Hospital","State")
        return(nw)
        
}

# testing the result
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
        

