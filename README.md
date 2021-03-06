# Getting and cleaning Data Project
## Course Project

The purpose of this project is for students to demonstrate their ability in collecting, working and cleaning data set. The objectives of this project is to create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Description of raw data

The raw data is from human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors.

+ Data Set Characteristics:  Multivariate, Time-Series
+ Number of Instances: 10299
+ Area: Computer
+ Attribute Characteristics: NA
+ Number of Attributes: 561
+ Date Donated: 2012-12-10
+ Associated Tasks: Classification, Clustering
+ Missing Values: NA
+ Number of Web Hits: 112927

More detailed description can be accessed at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

## Description of the script and the tidy data
    
The script, called run_analysis.R merged the training and test data sets to create one data set (total 10299 cases and 561 variables). From the created data set, the mean and standard deviation of each measurement (total 9 measurement types) were extracted. There are 6 types of activities. The tidy data contains the average of each variable for each activity and each subject. Here the tidy data set is submitted as a txt file, named tidyData.txt. 
