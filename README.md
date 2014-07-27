
## Description
The run_analysis.R script analyzes the data from the "Human Activity Recognition Using Smartphones Data Set". It then builds a new tidy data set with the average of each variable for each activity and each subject which is saved to the file subject_activity_variable_means.txt for further analysis. 

The features for the experimental database came from experiments carried out with 30 subjects performing certain activities. The subjects were wearing a smartphone on their waist while performing these activites and data from the accelerometer and gyroscope were collected.

## Original Dataset
The original dataset was obtained from [https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip]. Below is a list and description of each file. 

* activity_labels.txt - List of activity names that were performed
* features.txt - List of all features recorded/calculated
* features_info.txt - Info describing how features were obtained and what they mean
* test/X_test.txt - Test set
* test/y_test.txt - Test labels
* test/subject_test.txt - Subjects part of testing data
* train/X_train.txt - Training set
* train/y_train.txt - Training labels
* train/subject_train.txt - Subjects part of training data

Additional information about the test and original data files can be found at [http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones].

## Description of Script Analysis
The run_analysis.R script reads multiple text files, analyzes them, and builds a new tidy data set which is saved to file for future analysis. The original data and files are provided so that rerunning the script is simple. 

1. Reads in all three test data sets and combines them into a single data frame
  * This is done to merge the test set with its respective subject and activity
2. Reads in all three train data sets and combines them into a single data frame
  * This is done to merge the training set with its respective subject and activity
3. Merges test and train data frames together
  * The original experiment randomly seperated 30% of the subjects results from the full data set and used that to form the test data. This step is to merge these two back together as they originally were. 
4. Rename column headers to names provided in features.txt
  * Give all the columns descriptive names. The activity and subject colummns are also given names.
5. Only interested in mean or std columns so drop all others from df
  * For our tidy data set we only want to analyze mean or std features therefor all columns without "mean" or "std" in their name are dropped. 
6. Update activity column to use descriptive names instead of integer
  * All integers in activity column are replaced with their respective descriptive name.
7. update all columns to use clean names
  * The naming conventioned followed was to
    * Remove all characters not in [0-9A-Za-z]
    * Capitalize "mean" and "std"
  * ex. tBodyAcc-mean()-X is converted to tBodyAccMeanX
8. The df is analyzed and a new tidy data set is created with the mean of each subjects activities
9. Tidy data is saved to subject_activity_variable_names.txt


## Results
The resulting data set contains a single row for each activity an individual performed. The values for each feature column are the calculated mean of that subjects specific activity.















