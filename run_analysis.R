#clean up workspace
rm(list=ls())

#1) Merge the training and the test sets to create one data set

#set working directory 
setwd('/Users/jodiluna/Documents/3 Getting and Cleaning Data/UCI HAR Dataset/');

#read data 
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

#assigin column names 
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

#create final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

#read in test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

#assign column names to test data
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


#create final test set by merging xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);


#combine training and test data to create final data set
finalData = rbind(trainingData,testData);

#create a vector for column names from finalData, which will be used to select the desired mean() & stddev() columns
colNames  = colnames(finalData); 

#2) extract only the measurements on the mean and standard deviation for each measurement

#create a logicalVector that contains TRUE values for ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

#subset finalData table based on logicalVector to keep desired columns
finalData = finalData[logicalVector==TRUE];

#3) use descriptive activity names to name the activities in the data set

#merge finalData set with acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

#update colNames vector to include new column names after merge
colNames  = colnames(finalData); 

# 4) appropriately label the data set with descriptive variable names 

#clean up variable names
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

#reassign new descriptive column names to finalData set
colnames(finalData) = colNames;

#5) create a second, independent tidy data set with the average of each variable for each activity and each subject

#create a new table, finalDataNoActivityType without activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

#summarize the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

#merge tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

#export tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
