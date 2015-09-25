## Code Book

The script _*"run_analysis.R"*_ is to process the data in "UCI HAR Dataset" and output the data to _*"HAR_Summary.txt"*_ at the working directory.

---
### UCI HAR Dataset

Please check "README.txt" in "UCI HAR Dataset" for the information of the files included in the dataset.

---
### Data Processing

The following steps are taken to process the data:

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement.
 
3. Uses descriptive activity names to name the activities in the data set

4. Labels the data set with descriptive variable names. 

5. From the data set in step 4, creates a tidy data set with the average of each variable for each activity and each subject.

__The details of these steps are described in the script *"run_analysis.R"*__

---

### Output File

_*"HAR_Summary.txt"*_ lists the average of the variables for each subject and activity. The dataset includes 180 rows and 88 columns.

---

* subject [1 - 30]: ID for 30 volunteers participated in the experiments

* ActivityName [WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING]: Six daily living activities evaluated in the experiments

---
The left 86 columns are the average of 86 measurements, which come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. Check "features_info.txt" in "UCI HAR Dataset" for full information.

Briefly, The following signals were used to estimate variables of the feature vector for each pattern:  

'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag


The set of variables that were estimated from these signals and processed by *"run_analysis.R"* are: 

* mean(): Mean value
* std(): Standard deviation
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable and also processed by *"run_analysis.R"*:

* GravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean