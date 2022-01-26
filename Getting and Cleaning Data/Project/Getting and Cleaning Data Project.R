## load libraries

library(dplyr)


## 1- Reading and merging the date set

setwd("~/Desktop/R/Coursera-DataScience/Getting and Cleaning Data/Project/UCI HAR Dataset")

feature<-read.table("features.txt", header = FALSE)


X_test <- read.table("test/X_test.txt", sep = "", col.names=feature[,2])
y_test <-  read.table("test/y_test.txt", col.names = "Activity")
subject_test <-  read.table("test/subject_test.txt",col.names = "Subject")
test<-cbind(y_test,X_test,subject_test)

X_train <- read.table("train/X_train.txt", sep = "", col.names=feature[,2])
y_train <-  read.table("train/y_train.txt", col.names = "Activity")
subject_train <-  read.table("train/subject_train.txt", col.names = "Subject")

train<-cbind(y_train,X_train,subject_train)

# final data set 
data<-rbind(train,test)


##2-Extracts only the measurements on the mean and standard deviation for each measurement.
b<-grep("mean|std" , names(data))
teadydata<-data[,c(1,563,b)]

##3- Uses descriptive activity names to name the activities in the data set
teadydata$Activity<- ordered(teadydata$Activity,
                         levels = c(1,2,3,4,5,6),
                         labels = c("WALKING",
                                     "WALKING_UPSTAIRS",
                                    "WALKING_DOWNSTAIRS",
                                    "SITTING",
                                     "STANDING",
                                    "LAYING"
                         ))

## 4- Appropriately labels the data set with descriptive variable names. 

names(teadydata)<- gsub("^t","Time",names(teadydata))
names(teadydata)<- gsub("Acc","Accelerometer",names(teadydata))
names(teadydata)<- gsub("Gyro","Gyroscope",names(teadydata))
names(teadydata)<- gsub("Jerk","JerkSignals",names(teadydata))
names(teadydata)<- gsub("Mag","Magnitude",names(teadydata))
names(teadydata)<- gsub("^f","frequency",names(teadydata))
names(teadydata)<- gsub("mean()","MeanValue",names(teadydata))
names(teadydata)<- gsub("std()","StandarDeviation",names(teadydata))

## 5- creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#using the dplyer

finaldata<-teadydata %>% group_by(Activity,Subject)  %>%  summarise_all(funs(mean))


