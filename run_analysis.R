# Load required packages
packages <- c("dplyr", "reshape2")

for(package in packages) {
  if(!do.call(require, list(package))) {
    do.call(install.packages, list(package))
  }
  do.call(library, list(package))
}

#Merge training and test data sets

testFiles <- c("test/subject_test.txt", "test/y_test.txt", "test/X_test.txt")
trainingFiles <- c("train/subject_train.txt", "train/y_train.txt", "train/X_train.txt")

#Combine multiple files with tabular data to one data frame by cbind
combineFiles <- function(files) {
  for (file in files) {
    fileData <- read.table(paste("./UCI HAR Dataset", file, sep="/"), header=FALSE)
    if(exists("combinedData")) {      
      combinedData <- cbind(combinedData, fileData)
    } else {
      combinedData <- fileData
    }
  }
  combinedData
}

testData <- combineFiles(testFiles)
trainingData <- combineFiles(trainingFiles)

#Combine test data and training data rows
data <-rbind(testData, trainingData)

#Add descriptive activity labels
activities <- as.character(read.table("UCI HAR Dataset/activity_labels.txt")[,2])
data[,2] <- factor(data[,2], levels=c(1:6), labels=activities)

#Add column labels
labels <- c("subject", "activity")
features <- read.table("UCI HAR Dataset/features.txt")[,2]
for (i in 1:length(features)) {
  labels <- append(labels, paste(i, features[i], sep="-"))
}
names(data) <- labels

#Discard other than mean and standard deviation measurements
data <- select(data, subject, activity, contains("mean()"), contains("std()"))

#Creates a second data set with the average of each variable for each activity and each subject.
dataMelt <- melt(data, id=c("subject", "activity"), measure.vars=names(data[3:length(data)]))
data2 <- dcast(dataMelt, subject + activity ~ variable, mean)
write.table(data2, file="./means.txt", row.names=FALSE)
print(data2)