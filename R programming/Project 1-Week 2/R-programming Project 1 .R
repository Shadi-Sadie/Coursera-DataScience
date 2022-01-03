
#1- creating first function  pollutantmean'
# will calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors.
 
meanpollutant<-function(directory, pollutant, id = 1:332) {
    files <- list.files(directory, full.names=TRUE)  # list.file will creat a char list of the name of the files, the option full.names will add the path directory to the name
    data <- data.frame()  # creating a data frame 
    for (i in id){   
            data<-rbind(data,read.csv(files[i]))
    }
    mean(data[,pollutant], na.rm=TRUE)   # instead of data[data[,pollutant] intially i used data$pollutant and removed the "" but it wasn't working and I don't understand why
}
#Testing the result 
meanpollutant("specdata","sulfate", 1:10)
meanpollutant("specdata", "nitrate", 23)


#2- Creatng a function that reads a directory full of files and reports the number of completely 
# observed cases in each data file. 

complete<-function(directory,id = 1:332) {
    files <- list.files(directory, full.names=TRUE)  
    nobs = c() #creat a vector of zeros
    ids=c()
    for (i in id){   
        data<-read.csv(files[i])
        nobs<-c(nobs,sum(complete.cases(data)))  # my initial mistake here was to write it as nobs=sum(complete.cases(data)) next I changed it to nobs(i)=sum(complete.cases(data)) giving back 12 
        ids<-c(ids,i)
    }
    data.frame(id=ids, nobs=nobs) # creating a data frame from 2 vectors :D my initial mistake here was wrting ids anb nobs sepeatly 
}

# tests
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)



#3- directory of data files and a threshold for complete cases and calculates the correlation 
#between sulfate and nitrate for monitor locations where the number of 
#completely observed cases (on all variables) is greater than the threshold.
#The function should return a vector of correlations for the monitors that meet the threshold
#requirement. 

corr<-function(directory,treshold=0){
    files <- list.files(directory, full.names=TRUE)
    data <- data.frame() 
    cors=c()
    for (i in 1:332){   
        data<-read.csv(files[i])
            if (sum(complete.cases(data))>treshold){
                cors= c(cors, cor(data[,"sulfate"], data[,"nitrate"] , use = "complete.obs"))
                 }
         }  
    cors
}

# testing the results 

cr<-corr("specdata", 150)
head(cr)
    
cr<-corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)

cr<-corr("specdata")












