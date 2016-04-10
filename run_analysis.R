run_analysis <- function() {

  #set working directory
  setwd('/Users/dan/Development/R-Projects/Coursera/GettingAndCleaningData/Week4/')
  
  ##(Step 1) - Merge the training and the test sets to create one data set.
  
  features <- read.table('Data/UCI HAR Dataset/features.txt', header = FALSE)
  
  subject_train <- read.table('Data/UCI HAR Dataset/train/subject_train.txt', header = FALSE)
  colnames(subject_train) <- "subject_id"
  
  subject_test <- read.table('Data/UCI HAR Dataset/test/subject_test.txt',header=FALSE)
  colnames(subject_test) <- "subject_id"
  
  x_train <- read.table('Data/UCI HAR Dataset/train/x_train.txt', header = FALSE)
  colnames(x_train) <- features[,2]
  
  x_test <- read.table('Data/UCI HAR Dataset/test/x_test.txt', header = FALSE)
  colnames(x_test) <- features[,2]
  
  y_train <- read.table('Data/UCI HAR Dataset/train/y_train.txt', header = FALSE) 
  colnames(y_train) <- "activity_id"
  
  y_test <- read.table('Data/UCI HAR Dataset/test/y_test.txt', header = FALSE)
  colnames(y_test) <- "activity_id"
  
  training_data <- cbind(y_train, subject_train, x_train)
  test_data <- cbind(y_test, subject_test, x_test)
  
  data <- rbind(training_data, test_data)
  
  
  ##(Step 2) - Extract only the measurements on the mean and standard deviation for each measurement. 
  
  mean_and_std <- grep("-(mean|std)\\(\\)", features[, 2])
  data <- data[, mean_and_std]
  
  
  ##(Step 3) - Use descriptive activity names to name the activities in the data set
  
  activity_labels <- read.table('Data/UCI HAR Dataset/activity_labels.txt', header = FALSE) 
  colnames(activity_labels) <- c('activity_id', 'activity_label')
  
  data = merge(data, activity_labels, by='activity_id', all.x=TRUE)
  
  
  ##(Step 4) - Appropriately label the data set with descriptive activity names.
  
  names(data) <- gsub("^t", "Time", names(data))
  names(data) <- gsub("^f", "Frequency", names(data))
  names(data) <- gsub("mean", "Mean", names(data))
  names(data) <- gsub("-std", "StdDev", names(data))
  names(data) <- gsub("Acc", "Accelerometer", names(data))
  names(data) <- gsub("Gyro", "Gyroscope", names(data))
  names(data) <- gsub("Mag", "Magnitude", names(data))
  names(data) <- gsub("BodyBody", "Body", names(data))
  names(data) <- gsub("[-()]", "", names(data))  
  
  ##(Step 5) - Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
  
  tidy_data  = data[, names(data) != 'activity_label'];
  
  tidy_data = aggregate(tidy_data [, names(tidy_data) != c('activity_id', 'subject_id')], by = list(activity_id = tidy_data$activity_id, subject_id = tidy_data$subject_id), mean);
  
  tidy_data = merge(tidy_data, activity_labels, by='activity_id', all.x=TRUE);
  
  write.table(tidy_data, 'Data/UCI HAR Dataset/tidy_data.txt', row.names=TRUE, sep='\t')
}