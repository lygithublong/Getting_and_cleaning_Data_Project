# Codebook for getting and cleaning data project
## Original data sets used in extraction
- 'features.txt': List of all features, 561 features in total
- 'activity_labels.txt': Links the class labels with their activity name, 6 activities in total
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

## Descriptive activity names  used to label the activities in the data set 
* 1 WALKING
* 2 WALKING_UPSTAIRS
* 3 WALKING_DOWNSTAIRS
* 4 SITTING
* 5 STANDING
* 6 LAYING

## Descriptive variable names used to label the variables in the dataset
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. 
Prefix 't' is to denote time in original data sets.
The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ).
The magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).
Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccMag, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. 
Prefix 'f' is to indicate frequency domain signals.
* "activityId"                          
* "subjectId"                          
* "timeBodyAccMagnitudeMean"            
* "timeBodyAccMagnitudeStdDev"         
* "timeGravityAccMagnitudeMean"         
* "timeGravityAccMagnitudeStdDev"      
* "timeBodyAccJerkMagnitudeMean"        
* "timeBodyAccJerkMagnitudeStdDev"     
* "timeBodyGyroMagnitudeMean"           
* "timeBodyGyroMagnitudeStdDev"        
* "timeBodyGyroJerkMagnitudeMean"       
* "timeBodyGyroJerkMagnitudeStdDev"    
* "freqBodyAccMagnitudeMean"            
* "freqBodyAccMagnitudeStdDev"         
* "freqBodyAccJerkMagnitudeMean"    
* "freqBodyAccJerkMagnitudeStdDev" 
* "freqBodyGyroMagnitudeMean"       
* "freqBodyGyroMagnitudeStdDev"    
* "freqBodyGyroJerkMagnitudeMean"   
* "freqBodyGyroJerkMagnitudeStdDev"
* "activityType"                       
