# 1. Merges the training and the test sets to create one data set.

trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainSubject <-
        read.table("./UCI HAR Dataset/train/subject_train.txt")
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt")
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
joinData <- rbind(trainData, testData)
joinLabel <- rbind(trainLabel, testLabel)
joinSubject <- rbind(trainSubject, testSubject)

# 2. Extracts only the measurements on the mean
# and standard deviation for each measurement.
features <- read.table("./UCI HAR Dataset/features.txt")
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
joinData <- joinData[, meanStdIndices]
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2])
names(joinData) <- gsub("mean", "Mean", names(joinData))
names(joinData) <- gsub("std", "Std", names(joinData))
names(joinData) <- gsub("-", "", names(joinData))

# 3. Uses descriptive activity names to name the activities in the data set.

activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <-
        toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <-
        toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names.

names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
# write out the 1st dataset
write.table(cleanedData, row.names = FALSE, "merged_data.txt")


# 5. Creates a second, independent tidy data
# set with the average of each variable for each activity and each subject.

subjectLen <- length(table(joinSubject))
activityLen <- dim(activity)[1]
columnLen <- dim(cleanedData)[2]
result <-
        matrix(NA, nrow = subjectLen * activityLen, ncol = columnLen)
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for (i in 1:subjectLen) {
        for (j in 1:activityLen) {
                result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
                result[row, 2] <- activity[j, 2]
                bool1 <- i == cleanedData$subject
                bool2 <- activity[j, 2] == cleanedData$activity
                result[row, 3:columnLen] <-
                        colMeans(cleanedData[bool1 &
                                                     bool2, 3:columnLen])
                row <- row + 1
        }
}
head(result)
# write out the 2nd dataset
write.table(result, row.names = FALSE, "data_with_means.txt")
