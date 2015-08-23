run_analysis <- function() {

  #  Getting and Cleaning Data Course Project
  
  library(data.table)
  library(dplyr)
  
  #setwd("C:/myRrrr/3_GET_CLEAN_DATA/courseProject")
  datdir <- paste(getwd(),"UCI HAR Dataset",sep="/")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  Part 0 - Read txt files + Read training data + Read test data.
  
  raw.labels <- read.table(paste(datdir, "/activity_labels.txt", sep=""))

  raw.features <- read.table(paste(datdir, "/features.txt", sep=""))

  raw.test.subject <- read.table(paste(datdir, "/test/subject_test.txt", sep=""), col.names=c("Subject"))
  raw.test.x <- read.table(paste(datdir, "/test/X_test.txt", sep=""))
  raw.test.y <- read.table(paste(datdir, "/test/y_test.txt", sep=""), col.names=c("Activity"))

  raw.train.subject <- read.table(paste(datdir, "/train/subject_train.txt", sep=""), col.names=c("Subject"))
  raw.train.x <- read.table(paste(datdir, "/train/X_train.txt", sep=""))
  raw.train.y <- read.table(paste(datdir, "/train/y_train.txt", sep=""), col.names=c("Activity"))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  Part 1 .. Merges the training and the test sets to create one data set.
  
  raw.test <- cbind(raw.test.subject, raw.test.y, raw.test.x)

  raw.train <- cbind(raw.train.subject, raw.train.y, raw.train.x)

  raw.data <- rbind(raw.test, raw.train)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  Part 2 .. Extracts the means and SDs for each measurement.
  
  activity <- tbl_df(raw.data)
  collist <- grep("[Mm]ean\\()|[Ss]td\\()", raw.features$V2)
  colnames <- grep("[Mm]ean\\()|[Ss]td\\()", raw.features$V2, value=TRUE)
  collist <- collist + 2
  collist <- c(1, 2, collist)
  colnames <- c("Subject", "Activity", colnames)
  activity <- activity[, c(collist)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  Part 3 .. Uses descriptive activity names to name the activities in the data set.
  
  activity$Activity <- as.character(activity$Activity)
  raw.labels$V1 <- as.character(raw.labels$V1)
  raw.labels$V2 <- as.character(raw.labels$V2)
  for( i in 1:nrow(raw.labels) ) {
    activity$Activity <- gsub(raw.labels[i,1], raw.labels[i,2], activity$Activity)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  Part 4 .. Appropriately labels the data set with descriptive variable names.
  
  names(activity) <- colnames

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  Part 5 .. Using data set in step 4, creates a second, independent tidy
  #  data set with the average of each variable for each activity and each subject.

  tidy.data <- group_by(activity, Subject, Activity) %>% summarise_each(dplyr::funs(mean))
  names(tidy.data) <- colnames
  
  write.table(tidy.data, "./tidydata.txt", row.name=FALSE)

}
