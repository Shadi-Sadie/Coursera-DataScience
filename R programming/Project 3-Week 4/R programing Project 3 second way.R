setwd("~/Desktop/test/rprog-data-ProgAssignment3-data")
library(dplyr)

## 1. Histogram
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

### make  subset I need for this question
Subset<- outcome[,c(2,7,11,17,23)]
colnames(Subset)[c(3:5)] <- c('heart attack','heart failure','pneumonia')


### 2. Write the Best function : 

if (!'State' %in% c ('heart attack','heart failure','pneumonia') )
    stop('invalid death') 



best <- function(state,death) {
    
    if (!death %in% c('heart attack','heart failure','pneumonia')) {
        stop('invalid death') 
    }
    
    if (!state %in% Subset $State ) {
        stop('invalid state')
    }
    
BSS<-subset(Subset,State==state)

BSS[which.min(as.numeric(BSS[,death])),1]

}

best("NY", "pneumonia")
best("AK", "pneumonia")
## 3. ranking 

rankhospital<- function(x,y,z){

k<-subset(subset,State==x)
rank<-rank(as.numeric(k[,y]),na.last = TRUE,ties.method ="random")
k<- cbind(k,rank)
k[k$rank==z,1]
}

rankhospital("NY", "heart attack", 7)

rankhospital("TX", "pneumonia", 370)

rankhospital("NC", "heart attack", 360)


