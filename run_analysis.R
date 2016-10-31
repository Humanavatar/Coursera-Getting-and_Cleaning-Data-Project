## Load needed (required) libraries.
## Require is chosen over library as require allows a recovery path
# whereas library aborts on a failed load.
## 
## load the reshape2 package (will be used in STEP 5)
if (!require("reshape2")) {
  install.packages("reshape2")
  require("reshape2")
}

##
## The purpose of this project is to demonstrate your ability to collect, 
## work with, and clean a data set.
## 
## Review criterialess 
## 
## 1. The submitted data set is tidy.
## 2. The Github repo contains the required scripts.
## 3. GitHub contains a code book that modifies and updates the 
##    available codebooks with the data to indicate all the variables 
##    and summaries calculated, along with units, and any other relevant information.
## 4. The README that explains the analysis files is clear and understandable.
## 5. The work submitted for this project is the work of the student who submitted it.
## 
## Getting and Cleaning Data Course Projectless 
## 
## The purpose of this project is to demonstrate your ability to collect, 
## work with, and clean a data set. The goal is to prepare tidy data that can be 
## used for later analysis. You will be graded by your peers on a series of yes/no 
## questions related to the project. You will be required to submit: 1) a tidy data 
## set as described below, 2) a link to a Github repository with your script for 
## performing the analysis, and 3) a code book that describes the variables, the 
## data, and any transformations or work that you performed to clean up the data 
## called CodeBook.md. You should also include a README.md in the repo with your scripts. 
## This repo explains how all of the scripts work and how they are connected.
## 
## One of the most exciting areas in all of data science right now is wearable 
## computing - see for example this article (
## http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/). 
## Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced 
## algorithms to attract new users. The data linked to from the course website represent 
## data collected from the accelerometers from the Samsung Galaxy S smartphone. A 
## full description is available at the site where the data was obtained:
## 
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## 
## Here are the data for the project:
## 
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##
## You should create one R script called run_analysis.R that does the following.
##

## 1. Merges the training and the test sets to create one data set.
## 

## Set the working directory.  Our input files are located in a sub-folder 
## named: "dataset".
## 

setwd("~/GitHub/Humanavatar/Getting_and_Cleaning_Data/Final_Exercise")

## read in the "features" (Variable labels) and activities.
features <- read.table("dataset/features.txt")
activities <- read.table("dataset/activity_labels.txt")


str(features)
str(activities)

## 
## Read in the test and training data along with the features (variable labels), 
##  and subject identifiers.

## Test X, Test Y, and Test Subject
testx <- read.table("dataset/test/X_test.txt",header = FALSE)
testy <- read.table("dataset/test/Y_test.txt",header = FALSE)
subjecttest <- read.table("dataset/test/subject_test.txt",header = FALSE)

## Train X, Train Y, and Train Subject.
trainx <- read.table("dataset/train/X_train.txt",header = FALSE)
trainy <- read.table("dataset/train/Y_train.txt",header = FALSE)
subjecttrain <- read.table("dataset/train/subject_train.txt",header = FALSE)

## The folowing checks ensure the observations and variables match-up to prevent NAs 
##  from being introduced when the data is merged.

## Verify the features match the number of variables in the "X" data.
if(dim(features)[1] != dim(testx)[2] | dim(testx)[2] != dim(trainx)[2]) {
  stop('Column dimensions of the features or "X" data do not match.  NAs will be introduced when merged.')
}
  
## Ensure the number of observations in the test X, Y, and Subjects data match.
if (dim(subjecttest)[1] != dim(testx)[1] | dim(testx)[1] != dim(testy)[1]) {
  stop('Number of observations in the Test X, Y, and Subjects files do not match.')
}

## Ensure the number of ovservations in the train X, Y, and Subjects data match.
if (dim(subjecttrain)[1] != dim(trainx)[1] | dim(trainx)[1] != dim(trainy)[1]) {
  stop ('Number of observations in the Train X, Y, and Subjects files do not match.')
}

## To make it to here, we have verified the datasets match to prevent the introduction
## of NAs and the data may safely be recombined.


## Create combined tables of "X", "Y" (activity), and subject data from the test and train
## sets.  
## It is important to keep the same test-train order when combining the data.
combinedtest <- rbind(testx, trainx)
combinedsubject <- rbind(subjecttest, subjecttrain)
combinedactivity <- rbind(testy, trainy)

## Set the "X" variable names
names(combinedtest) <- features$V2

## Set the activity column name
names(combinedactivity) <- c("activity")

## Set the subject column name
names(combinedsubject) <- c("subject")

## Before the three dataset are merged, verify the number of observations are the 
## same across the three datasets.
if(dim(combinedactivity)[1] != dim(combinedsubject)[1] | 
   dim(combinedactivity)[1] != dim(combinedtest)[1]) {
  stop('Number of observations in the merged test, activity, and subject datasets do not match.')
}

combined <- cbind(combinedtest, combinedsubject)
combined <- cbind(combined, combinedactivity)

## examine the structure of the combined data
dim(combined)
str(combined)

## End step 1.

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 

## using the variable (column) names of the combined data, find those columns which are the
## mean and standard deviation columns.  The initial data's read identifies these 
## columns as:
## The set of variables that were estimated from these signals are: 
## 
##    mean(): Mean value
##    std(): Standard deviation
## 
## The same readme also identifies the following mean column, but notes it is  
## not a simple mean value:
## 
##    meanFreq(): Weighted average of the frequency components to obtain a mean frequency
##
## Given the above, we are sticking with the "mean()" and "std" columns.  
## 
## Also, we want to keep the "subject" and "activity" columns.
## 
selectedColumns = grepl( '(-mean\\(\\)|-std\\(\\)|activity|subject)', names(combined) )
combined2 <- subset(combined, select = selectedColumns)

## Examine the structure of the new combined data.
dim(combined2)
str(combined2)

## End Step 2.

## 
## 3. Uses descriptive activity names to name the activities in the data set
## 
if(length(setdiff(combined2$activity, activities$V1))) {
  stop('Unexpected activity value found in the combined2$activity column')
}
combined2$activity = factor(combined2$activity, levels=activities$V1, 
                                       labels=activities$V2)
##
## Verify the changes
head(combined2$activity, 30)

## 
## 4. Appropriately label the data set with descriptive variable names.
## 
## "t" at the front of a name is replaced with "Time".
## "f" at the front of a name is replaced with "Frequency".
## "Gyro" in the name is replaced with "Gyroscope".
## "Acc" in the name is replaced by "Accelerometer".
## "Mag" in the name is replaced by "Magnitude
## "BodyBody" in the name is replaced by "Body"
names(combined2) <- gsub("^t", "Time", names(combined2))
names(combined2) <- gsub("^f", "Frequency", names(combined2))
names(combined2) <- gsub("Acc", "Accelerometer", names(combined2))
names(combined2) <- gsub("Gyro", "Gyroscope", names(combined2))
names(combined2) <- gsub("Mag", "Magnitude", names(combined2))
names(combined2) <- gsub("BodyBody", "Body", names(combined2))

names(combined2)

## 
## 5. From the data set in step 4, creates a second, independent tidy data 
##    set with the average of each variable for each activity and each subject.
## 
melteddata <- melt(combined2, id=c("subject","activity"))
tidydata <- dcast(melteddata, subject+activity ~ variable, mean)

## Examine the head and tructure of the tiday dataset
head(tidydata)
str(tidydata)

## Save the tiday dataset.
write.csv(tidydata, "tidy.csv", row.names = FALSE)
write.table(tidydata, "tidy.txt",  row.names = FALSE)
## 
## Good luck!
##

## Cleanup after outselves.  :)
## 
rm(list = ls())
