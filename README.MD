### Introduction

The R script run_analysis.R is used to summarize the Human Activity Recognition (HAR) data collected from the recordings of subjects performing daily living activities while carrying a waist-mounted smartphone with embedded inertial sensors. 

The experiments have been carried out with a group of 30 volunteers, who performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist.A full description about the study is available at the website: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The data can be downloaded:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

After unzip the downloaded file at the work directory, the file folder is named as "UCI HAR Dataset".

The R script run_analysis.R is to create a tidy data set with the average of certian variables for each activity and each subject from the "UCI HAR Dataset".

---
### UCI HAR Dataset

Please check 'README.txt' in "UCI HAR Dataset" for the information of the files included in the dataset.

---
### Summary Procedure

The following summary steps are required to take on the data:

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement.
 
3. Uses descriptive activity names to name the activities in the data set

4. Appropriately labels the data set with descriptive variable names. 

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##### The details of these steps are described in the comments of run_analysis.R

---
### Use the script run_analysis.R 

1. Save run_analysis.R in the work directory. 

2. Make sure the dataset "UCI HAR Dataset" saved in the same directory.

3. Make sure the package "dplyr" installed in R. If not, use install.packages("dplyr") to install it.

4. Run the script in R: source("run_analysis.R")

5. The script output the tidy data as "HAR_Summary.txt" in the work directory, which can be read into R using read.table("HAR_Summary.txt", header = TRUE)

6. Please check CodeBook for the information of varaibles in "HAR_Summary.txt". 