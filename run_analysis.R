# intall needed packages
if (!require("data.table")) {
        install.packages("data.table")
}
if (!require("reshape2")) {
        install.packages("reshape2")
}
require("data.table")
require("reshape2")

#1. Merge the training and the test sets to create one data set.

# Read features of the data and activity types
features = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
colnames(activityType) = c('activityId','activityType');

# Read training data
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2];
colnames(yTrain) = "activityId";

# Merge (columns) yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Read test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

colnames(subjectTest) = "subjectId";
colnames(xTest) = features[,2];
colnames(yTest) = "activityId";

# Merge (columns) the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);

# Combine (rows) training and test data to create a final data set
finalData = rbind(trainingData,testData);

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement.

# Create a logical vector that contains TRUE values for the needed features of the ID, mean() & stddev() columns and FALSE for others
needed_features = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[needed_features==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector which only contains the needed features
colNames = colnames(finalData); 

# 4.Label the data set with more descriptive variable names.
# Cleaning up the variable names
for (i in 1:length(colNames))
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
# update the finalData set with more discriptive column names
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a data set, finalDataNoActivityType without the activityType column
finalDataNoActivityType = finalData[,names(finalData) != 'activityType'];
# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
# 6 activities and 30 subjects and 18 features
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
# Merging the tidyData with activityType to include descriptive acitvity names
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE);
# Export the tidyData set with write.table() using row.name=FALSE
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');

