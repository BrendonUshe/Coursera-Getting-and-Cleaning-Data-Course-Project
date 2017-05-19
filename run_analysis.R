#1.	Merges the training and the test sets to create one data set.

#set the actual working directory from local machine for UCI Folder 

setwd('/Brendon/BigData/DataScience-Coursera/3.Getting and Cleaning Data/UCI HAR Dataset/');


# Read the Train data from files into the programme flow and the Descriptive tables
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Assing column names
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Creation of trainingData set from merging yTrain, subjectTrain, xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Read in the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names for test data set
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# testData set by merging the xTest, yTest subjectTest 
testData = cbind(yTest,subjectTest,xTest);


# Merge trainingData and testData to create finalData set
finalData = rbind(trainingData,testData);

colNames  = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# logicalVector values for the ID, mean() & stddev() columns
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# finalData for required columns
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set


finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# new descriptive columns 
colnames(finalData) = colNames;

# 5. tidyData set. 

# finalDataNoActivityType without the activityType column

finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
