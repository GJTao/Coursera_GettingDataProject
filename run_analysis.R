##############################################################
#           Getting and Cleaning Data Course Project         #
#               RUN ANALYSIS FOR SAMSUNG DATA                # 
##############################################################

## This project is to organize and clean the data collected 
## from the accelerometers of the Samsung Galaxy S smartphone

## Refer to README and CodeBook for the information of the data,
## the variables, and the analysis procedure.

## To run this analysis, you should have downloaded and saved 
## data in file folder "UCI HAR Dataset" at your work directory

##############################################################
#         1. Merge the training and the test sets            #
##############################################################

## 1.1 read all the tables in the work directory as data.frame

##     Measurement data
train <- read.table("./UCI HAR Dataset/train/X_train.txt")
test <- read.table("./UCI HAR Dataset/test/X_test.txt")

##     Activity Code
trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt")

##     Activity Names
activityLabel <- read.table("./UCI HAR Dataset/activity_labels.txt")
##     Measurement/Variable Labels
features <- read.table("./UCI HAR Dataset/features.txt")

##     Subject Data
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")


## name the varaibles of ActivityCode and subject
names(testLabel) <- c("ActivityCode")
names(trainLabel) <- c("ActivityCode")
names(subject_test) <- c("subject")
names(subject_train) <- c("subject")
names(activityLabel) <- c("ActivityCode", "ActivityName")

## 1.2 Combine train and test Data

## Join the 561 measurement varaibles of test and train data
## with their ActivityCode and subject 
test <- cbind(test, testLabel, subject_test)
train <- cbind(train, trainLabel, subject_train)

## Combine test and train set
CombinedData <- rbind(test, train)

##############################################################
#          2. Extract only the measurements on               #
#             the mean and standard deviation                #
##############################################################

## 2.1 Make a list of columns to be extracted

##Find the index of mean and standard deviation varaibles
meanlist <- grep("mean", features[,2], ignore.case = TRUE)
stdlist <- grep("std", features[,2], ignore.case = TRUE)

## Include columns of mean and standard deviation, ActivityCode, 
## and subject in the extract list
extractlist <- sort(c(meanlist, stdlist, 562, 563))

## 2.2 Extract data
ExtractData <- CombinedData[, extractlist]

##############################################################
#           3. Use descriptive activity names                #
#           for the activities in the data set               #
##############################################################

ExtractData <- merge(ExtractData, activityLabel, 
                      by = "ActivityCode")

##############################################################
#           4. Label the columns of data set                 #
#            with descriptive variable names.                #
##############################################################

## Find the names of mean and std variables
VariableNames <- as.character(features[sort(c(meanlist, stdlist)), 2])

## Rename columns of ExtractData2 with the name list
names(ExtractData)[2:87] <- VariableNames

##############################################################
#          5. Create a tidy data set with the average        #
#     of each variable for each activity and each subject.   # 
##############################################################
##Load package dplyr
library(dplyr)

##Drop the column of ActivityCode, group the data by subject and
##ActivityName, then calculate the average of each variable
SummaryData <- ExtractData[,2:89] %>% 
        group_by(subject, ActivityName) %>%
        summarize_each(funs(mean))

##Output the summarized data to "HAR_Summary.txt" at working
##directory. 
write.table(SummaryData, "HAR_Summary.txt", row.name=FALSE)

##The output data can be read into R using 
##read.table("HAR_Summary.txt", header = TRUE)

