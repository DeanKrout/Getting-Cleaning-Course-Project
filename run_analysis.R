#Getting and Cleaning Data Project

# Create one R script called run_analysis.R that does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the colHeadings on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.

# Clear all values
rm(list= ls())

# Install Packages (don't need these to install for each test run)
#install.packages("data.table")
#install.packages("reshape2")
#library("data.table")
#library("reshape2")

path <- getwd()

#Get the data (not needed after the file is downloaded and unzipped)
#url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(url, file.path(path, "dataFiles.zip"))
#unzip(zipfile = "dataFiles.zip")

# Label and Features
activityLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt"), col.names = c("classLabels", "activityName"))
features <- fread(file.path(path, "UCI HAR Dataset/features.txt"), col.names = c("index", "featureNames"))
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])
colHeadings <- features[featuresWanted, featureNames]
# Clean up the Column Headings
colHeadings <- gsub('[()]', '', colHeadings)

# Training data
training <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(training, colnames(training), colHeadings)
trainActivities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt"), col.names = c("Activity"))
trainSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt"), col.names = c("SubjectNum"))
training <- cbind(trainSubjects, trainActivities, training)

# Test data
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(test, colnames(test), colHeadings)
testActivities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt"), col.names = c("Activity"))
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt"), col.names = c("SubjectNum"))
test <- cbind(testSubjects, testActivities, test)

# Merge Training and Test data
mergedDataset <- rbind(training, test)

# Merge classLabels and activityName 
mergedDataset[["Activity"]] <- factor(mergedDataset[, Activity], levels = activityLabels[["classLabels"]], labels = activityLabels[["activityName"]])
mergedDataset[["SubjectNum"]] <- as.factor(mergedDataset[, SubjectNum])
mergedDataset <- reshape2::melt(data = mergedDataset, id = c("SubjectNum", "Activity"))
mergedDataset <- reshape2::dcast(data = mergedDataset, SubjectNum + Activity ~ variable, fun.aggregate = mean)

# Write txt file to be submitted
write.table(mergedDataset, 'tidyData.txt',row.names=FALSE,quote = FALSE, sep='\t')
