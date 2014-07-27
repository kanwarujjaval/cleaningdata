## Getting and Cleaning Data Course Project Codebook

## Study Design Description
This Codebook describes the variables, the data, and any work that I have performed to clean the data and provide an output file. 
The site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
The data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Code Book
### Feature Selection 

The features for the experimental database came from experiments carried out with 30 subjects performing certain activities. The subjects were wearing a smartphone on their waist while performing these activites and data from the accelerometer and gyroscope were collected.
Quoted From features_info.txt
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
These signals were used to estimate variables of the feature vector for each pattern:  
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
### Feature Data
In the original data there were multiple variables that were estimated from these signals but for our purposes we extracted only the following:
1. **Mean:** Mean value
2. **Std:** Standard deviation
In addition the following vectors, obtained by averaging the signals in a signal window sample, were retained for our calculations.
* gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean
Lastly two additional columns were added: 
1. **activity:** Descriptive name of each subject's activity Type: Factor Values: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
2. **subject:** Integer identifying subject who performed activities
### Data Analysis
Using the data from this experiment we analysed these feature variables. Processing each subject seperately, we grouped their data by activity and then calculated the group mean for each variable. Thus in the file subject_activity_variable_means.txt each row corresponds to a specific activity the individual performed and the averages for each variable of interest. 
There is one script run_analysis.R which analyzes the data. The original data has been provided with the project so no addtional work should have to be performed to run the script.