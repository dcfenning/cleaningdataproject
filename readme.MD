==================================================================
README for Coursera Course Getting and Cleaning Data
Course Project Danielle Fenning
==================================================================
Uses the Human Activity Recognition Using Smartphones Dataset Version 1.0
Available at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Reference for Dataset used:
Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
==================================================================

Aim of run_analysis.R code, in 6 steps, is to make two tidy data sets ready for analysis from the Human Activity Recognition Using Smartphones Dataset v1.0
The first data set extracts just the mean and standrad deviation measurements
The second data set uses the first tidy data set to summarise the mean for each measurement, by subject and by activity

==================================================================

The data dictionary for both tidy data sets can be found in CodeBook.md

run_analysis.R 
Step 1:
Downloads and unzips all the data required from:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

Step2:
Binds the .txt files: X_test.txt and x_train.txt into one dataframe.  Creates a new factor column which identifies whether the data originated from test or train data

Step 3:
Extracts from the dataframe only the measurements on the mean and standard deviation for each record and re-labels columns to be R friendly readable names

Step 4:
Binds the.txt files: y_test.txt and y_train.txt into one data frame. Creates factors for activites with 6 levels and labeled according to activity_labels.txt

Step 5:
Binds the .txt files: subject_test.txt and subject_train.txt Binds subject ID's into the final dataframe to create a final tidy data set

Step 6:
Create another tidy data set which takes the average of the mean and standard deviation for each activity and each subject only.
 