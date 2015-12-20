#1.Merges the training and the test sets to create one data set.
#read test data set in table
xtest <- read.table("./data/test/X_test.txt")
ytest <- read.table("./data/test/y_test.txt")
subtest <- read.table("./data/test/subject_test.txt")
#read train data in table
xtrain <- read.table("./data/train/X_train.txt")
ytrain <- read.table("./data/train/y_train.txt")
subtrain <- read.table("./data/train/subject_train.txt")
# Merge labels y table test and train
#Verify compatibilty with the column number before merging data
dim(ytest)
dim(ytrain)
labels <- rbind(ytrain,ytest)
# Merge data set X table train and test
#Verify compatibilty with the column number before merging data
dim(xtest)
dim(xtrain)
data <- rbind(xtrain,xtest)
# Merge subject table train and test
#Verify compatibilty with the column number before merging data
dim(subtrain)
dim(subtest)
subjects <- rbind(subtest,subtrain)

#2.Extracts only the measurements on the mean 
#and standard deviation for each measurement
# Read the feature.txt data and create data table
feat <- read.table("./data/features.txt")
#Verify the correct rows number before merging
dim(feat)
mean_std <- grep("mean\\(\\)|std\\(\\)", feat[, 2])
data <- data[,mean_std]
# Add names to the data table from the features table
names(data) <- gsub("\\(\\)", "", feat[mean_std, 2]) # remove "()"
#names(data) <- gsub("mean", "Mean", names(data)) # capitalize M
#names(data) <- gsub("std", "Std", names(data)) # capitalize S
#names(data) <- gsub("-", "", names(data)) # remove "-" in column names 

#3.Uses descriptive activity names to name the activities
#in the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
#substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
#substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activ_label <- activity[labels[, 1], 2]
labels[, 1] <- activ_label
names(labels) <- "activity"

#4.Appropriately labels the data set with descriptive variable names. 
names(subjects) <- "subject"
clean_data <- cbind(subjects, labels, data)
write.table(clean_data, "merged_data.txt") # saving the 1st dataset

#5.From the data set in step 4, creates a second, independent tidy
#data set with the average of each variable for each activity and each subject.
sub_len <- length(table(subjects)) 
activity_len <- dim(activity)[1] 
column_len <- dim(clean_data)[2]
result <- matrix(NA, nrow=sub_len*activity_len, ncol=column_len) 
result <- as.data.frame(result)
colnames(result) <- colnames(clean_data)
row <- 1
for(i in 1:sub_len) {
  for(j in 1:activity_len) {
    result[row, 1] <- sort(unique(subjects)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == clean_data$subject
    bool2 <- activity[j, 2] == clean_data$activity
    result[row, 3:column_len] <- colMeans(clean_data[bool1&bool2, 3:column_len])
    row <- row + 1
  }
}
require(dplyr)
tbl_df(result) # print result head
write.table(result, "data_with_means.txt") # saving the 2nd dataset


