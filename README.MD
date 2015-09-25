## Introduction

The R script run_analysis.R is used to process the Human Activity Recognition (HAR) data collected from the recordings of subjects performing daily living activities while carrying a waist-mounted smartphone with embedded inertial sensors. 

The experiments have been carried out with a group of 30 volunteers, who performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist.

A full description about the study is available at the website: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data to be processed:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

After unzip the downloaded file at the working directory, the file folder is named as "UCI HAR Dataset".

The R script run_analysis.R is to create a tidy data set for the average of any variables related to the signal means and standard deviations for each activity and each subject. 

The script output a data file "HAR_Summary.txt". Please check the CodeBook for the data information and processing procedure.

---

### Use the script run_analysis.R 

1. Save run_analysis.R in the working directory. 

2. Make sure the dataset "UCI HAR Dataset" saved in the same directory.

3. Make sure the package "dplyr" installed in R. If not, use install.packages("dplyr") to install it.

4. Run the script in R: source("run_analysis.R")

5. The output file "HAR_Summary.txt" in the working directory can be read into R using read.table("HAR_Summary.txt", header = TRUE)

---
### Data Processing

The following steps are taken to process the data:

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement.
 
3. Uses descriptive activity names to name the activities in the data set

4. Labels the data set with descriptive variable names. 

5. From the data set in step 4, creates a tidy data set with the average of each variable for each activity and each subject.

__The details of these steps are described in the script run_analysis.R__



