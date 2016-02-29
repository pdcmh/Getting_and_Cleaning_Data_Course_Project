# Load packages
library(dplyr)
library(data.table)
library(tidyr)

# Set working directory
setwd("C:\\Coursera\\Getting_and_Cleaning_Data\\Week4\\getdata_projectfiles_UCI HAR Dataset")

# Read subject data files
dSubTrain <- tbl_df(read.table("./UCI HAR Dataset/train/subject_train.txt"))
dSubTest  <- tbl_df(read.table("./UCI HAR Dataset/test/subject_test.txt"))

# Read activity data files
dActTrain <- tbl_df(read.table("./UCI HAR Dataset/train/Y_train.txt"))
dActTest  <- tbl_df(read.table("./UCI HAR Dataset/test/Y_test.txt"))

#Read data files.
dTrain <- tbl_df(read.table("./UCI HAR Dataset/train/X_train.txt"))
dTest  <- tbl_df(read.table("./UCI HAR Dataset/test/X_test.txt"))

###############################################################################
# 1. Merges the training and the test sets to create one data set.
###############################################################################

# Merge Subject Data Set
mSubData <- rbind(dSubTrain, dSubTest)
setnames(mSubData, "V1", "subjectNum")

# Merge Activity Data Set
mActData<- rbind(dActTrain, dActTest)
setnames(mActData, "V1", "activityNum")

# Merge Subject and Activity Data
mData <- rbind(dTrain, dTest)

# Read Feature Data
dFeatures <- tbl_df(read.table("./UCI HAR Dataset/features.txt"))
setnames(dFeatures, names(dFeatures), c("featureNum", "featureName"))
colnames(mData) <- dFeatures$featureName

# Read Activity Labels 
actLabels<- tbl_df(read.table("./UCI HAR Dataset/activity_labels.txt"))
setnames(actLabels, names(actLabels), c("activityNum","activityName"))

# Merge Data Columns
mSubActData<- cbind(mSubData, mActData)
mData <- cbind(mSubActData, mData)

###############################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
###############################################################################

# Extracting mean and standard deviation from features.txt
dFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dFeatures$featureName,value=TRUE)

# Taking mean and standard deviation and adding "subject","activityNum"
dFeaturesMeanStd <- union(c("subjectNum","activityNum"), dFeaturesMeanStd)
mData <- subset(mData, select=dFeaturesMeanStd)

###############################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###############################################################################

# Add activity name into mData
mData <- merge(actLabels, mData , by="activityNum", all.x=TRUE)
mData$activityName <- as.character(mData$activityName)

## Modify mData with variable means sorted by subject and Activity
mData$activityName <- as.character(mData$activityName)
aggData <- aggregate(. ~ subjectNum - activityName, data = mData, mean) 
mData <- tbl_df(arrange(aggData, subjectNum, activityName))

###############################################################################
# 4. Appropriately labels the data set with descriptive variable names. 
###############################################################################

names(mData)<-gsub("std()", "SD", names(mData))
names(mData)<-gsub("mean()", "MEAN", names(mData))
names(mData)<-gsub("^t", "time", names(mData))
names(mData)<-gsub("^f", "frequency", names(mData))
names(mData)<-gsub("Acc", "Accelerometer", names(mData))
names(mData)<-gsub("Gyro", "Gyroscope", names(mData))
names(mData)<-gsub("Mag", "Magnitude", names(mData))
names(mData)<-gsub("BodyBody", "Body", names(mData))

###############################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
###############################################################################

write.table(mData, "./TidyDataSet.txt", row.name=FALSE)