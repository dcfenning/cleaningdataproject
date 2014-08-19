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
The first tidy data set extracts from the raw data just the mean and standrad deviation measurements.
The second data set uses the first tidy data set to summarise the mean for each measurement, by subject and by activity.
The output of run_analysis.R code is the second data set exported to tidydataset2.csv
See codebook.MD for reference to variables 
==================================================================

The data dictionary for both tidy data sets can be found in CodeBook.md

run_analysis.R 
Step 1:
Checks to see if file exists already, 
if the dile does not exist then downloads and all the data required from:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
and unzips the file.

Step 2:
Read all required .txt files into dataframes ready for tidying and binds together the test and train data, then the label column variables from the raw features.txt file

Step 3:
Extracts from the dataframe only the measurements on the mean and standard deviation for each record and re-labels columns to be R friendly readable names to create the first tidy data set

Step 4: 
Converts Activity variable to a factor variable using activity_labels.txt table column 2

Step 5:
Creates a new second tidy data set that contains the average for each variable for each activity and each subject


Step 6:
Exports tidydata2 to .csv file.