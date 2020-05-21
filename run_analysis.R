#Download the file and unzip the file in the working directory
library(dplyr)
getwd()

#loading the files into R
featureName <- read.table("C:/Users/Nikita Zade/Desktop/Piu/Rstudio/Cleaning_data/Assigment/UCI HAR Dataset/features.txt")
activity_labels <- read.table("C:/Users/Nikita Zade/Desktop/Piu/Rstudio/Cleaning_data/Assigment/UCI HAR Dataset/activity_labels.txt")

#1. Merging the training and the test sets to create one data set
subject_test <- read.table("C:/Users/Nikita Zade/Desktop/Piu/Rstudio/Cleaning_data/Assigment/UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("C:/Users/Nikita Zade/Desktop/Piu/Rstudio/Cleaning_data/Assigment/UCI HAR Dataset/train/subject_train.txt")
subject <- rbind(subject_test, subject_train)

x_test <- read.table("C:/Users/Nikita Zade/Desktop/Piu/Rstudio/Cleaning_data/Assigment/UCI HAR Dataset/test/X_test.txt")
x_train <- read.table("C:/Users/Nikita Zade/Desktop/Piu/Rstudio/Cleaning_data/Assigment/UCI HAR Dataset/train/X_train.txt")
features <- rbind(x_test, x_train)

y_test <- read.table("C:/Users/Nikita Zade/Desktop/Piu/Rstudio/Cleaning_data/Assigment/UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("C:/Users/Nikita Zade/Desktop/Piu/Rstudio/Cleaning_data/Assigment/UCI HAR Dataset/train/y_train.txt")
activity <- rbind(y_test, y_train) 
#naming the columns
colnames(features) <- t(featureName[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#2Extracting only the measurements on the mean and standard deviation for each measurement.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
extractedData <- completeData[,requiredColumns]
dim(extractedData)

#3. Using descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
    extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

#4. Appropriately labeling the data set with descriptive variable names
names(extractedData)
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#5. creating independent tidy data set with the average of each variable for each activity and each subject
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

