### Code Book

HAR_Summary.txt list the average of the variables for each subject and activity. The dataset include 180 rows and 88 columns.

---

* subject [1 - 30]:ID for 30 volunteers participated in the experiments

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


The set of variables that were estimated from these signals are: 

* mean(): Mean value
* std(): Standard deviation
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency

---
Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

* GravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean