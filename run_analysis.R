# Getting and Cleaning Data Course Project 

# This block of code will get the list of files 

path_file <- file.path("C:/Users/habim/OneDrive/Desktop/RStudioProgms/GettingAndCleanDataFinalAssignment/UCI HAR Dataset")
files <- list.files(path_file, recursive = TRUE)
print(files)


# Read files and assign them to variables 

activityLabels  <- read.table(file.path(path_file, "activity_labels.txt"))
features <- read.table(file.path(path_file, "features.txt"))
subject_test <- read.table(file.path(path_file, "test/subject_test.txt"))
subject_train <- read.table(file.path(path_file, "train/subject_train.txt"))
x_test <- read.table(file.path(path_file, "test/X_test.txt"))
y_test <- read.table(file.path(path_file, "test/Y_test.txt"))
x_train <- read.table(file.path(path_file, "train/X_train.txt"))
y_train <- read.table(file.path(path_file, "train/Y_train.txt"))



# Set Column names 


colnames(x_train) <- features[,2]
colnames(y_train) <-"activityCode"
colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityCode"
colnames(subject_train) <- "subjectCode"
colnames(subject_test) <- "subjectCode"
colnames(activityLabels) <- c('activityCode','activityType')


#Merge the test and training set 

x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train,subject_test)
merged_DataSet <- cbind(subject_data, y_data, x_data)


#Extracts only the measurements on the mean and standard deviation for each measurement.
library(magrittr)
library(webuse)
library(dplyr)

colNames <- colnames(merged_DataSet)

mean_std <- (grepl("activityCode" , colNames) | 
                  grepl("subjectCode" , colNames) | 
                  grepl("mean.." , colNames) | 
                  grepl("std.." , colNames) 
)

TidyData <- merged_DataSet[ , mean_std == TRUE]


DataWithActivityNames <- merge(TidyData, activityLabels,
                              by='activityCode',
                              all.x=TRUE)


names(DataWithActivityNames)<-gsub("Acc", "Accelerometer", names(DataWithActivityNames))
names(DataWithActivityNames)<-gsub("Gyro", "Gyroscope", names(DataWithActivityNames))
names(DataWithActivityNames)<-gsub("BodyBody", "Body", names(DataWithActivityNames))
names(DataWithActivityNames)<-gsub("Mag", "Magnitude", names(DataWithActivityNames))
names(DataWithActivityNames)<-gsub("^t", "Time", names(DataWithActivityNames))
names(DataWithActivityNames)<-gsub("^f", "Frequency", names(DataWithActivityNames))
names(DataWithActivityNames)<-gsub("tBody", "TimeBody", names(DataWithActivityNames))
names(DataWithActivityNames)<-gsub("-mean()", "Mean", names(DataWithActivityNames), ignore.case = TRUE)
names(DataWithActivityNames)<-gsub("-std()", "STD", names(DataWithActivityNames), ignore.case = TRUE)
names(DataWithActivityNames)<-gsub("-freq()", "Frequency", names(DataWithActivityNames), ignore.case = TRUE)
names(DataWithActivityNames)<-gsub("angle", "Angle", names(DataWithActivityNames))
names(DataWithActivityNames)<-gsub("gravity", "Gravity", names(DataWithActivityNames))


Final_Tidy_Data <- aggregate(. ~subjectCode + activityCode, DataWithActivityNames, mean)
Final_Tidy_Data <- Final_Tidy_Data[order(Final_Tidy_Data$subjectCode, Final_Tidy_Data$activityCode),]

# Writing second tidy data set in txt file

write.table(Final_Tidy_Data, "Final_Tidy_Data.txt", row.name=FALSE)