## this program runs on Linux (ubuntu12.04), for Windows or Mac, modify the data download and unzip part accordingly
## or manually download and unzip to a location

## set the working directory
setwd('/home/songlin/Coursera/Get_Clean_Data')
## download the data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "UCI_Dataset.zip", method="wget")
## unzip it; result is stored in "UCI HAR Dataset"; rename it to remove the space (personal preference) 
system('unzip UCI_Dataset.zip')
system('mv "UCI HAR Dataset" UCI_Dataset')
## the data has CTRL-M; use dos2unix to remove it (if not installed, run sudo apt-get install dos2unix)
system('dos2unix UCI_Dataset/train/X_train.txt')
system('dos2unix UCI_Dataset/test/X_test.txt')

##############################################################################################
## program starts here
##############################################################################################

##########################################################
## section 0 - read the feature and activity label data ##
##########################################################
## read feature data
dst_feature <- read.csv("UCI_Dataset/features.txt", sep ="", stringsAsFactors = FALSE, header=FALSE)
feature_list <- dst_feature[,2]

## read the activit label data
dst_activity_label <- read.csv("UCI_Dataset/activity_labels.txt", sep ="", stringsAsFactors = FALSE, header=FALSE)
names(dst_activity_label) <- c("activity_label", "activity_name")

##############################################################################
## section 1 - Merges the training and the test sets to create one data set ##
##############################################################################
## training part
## first, read the main dataset
dst_x_train <- read.csv('UCI_Dataset/train/X_train.txt', sep = "", header=FALSE)
## read the label (activity)
dst_y_train <- read.csv('UCI_Dataset/train/y_train.txt', sep = "", header=FALSE)
names(dst_y_train) <- c("activity_label")
## append descriptive activity names
dst_y_train <- merge(dst_y_train, dst_activity_label, by.x="activity_label", by.y="activity_label")
## read subject data
dst_subject_train <- read.csv('UCI_Dataset/train/subject_train.txt', sep = "", header=FALSE)
names(dst_subject_train) <- c("subject")
## combine to get training data
dst_train <- cbind(dst_subject_train, dst_y_train$activity_name, dst_x_train)
names(dst_train) <- c("subject", "activity", feature_list)

## testing part
## first, read the main dataset
dst_x_test <- read.csv('UCI_Dataset/test/X_test.txt', sep = "", header=FALSE)
## read the label (activity)
dst_y_test <- read.csv('UCI_Dataset/test/y_test.txt', sep = "", header=FALSE)
names(dst_y_test) <- c("activity_label")
## append descriptive activity names
dst_y_test <- merge(dst_y_test, dst_activity_label, by.x="activity_label", by.y="activity_label")
## read subject data
dst_subject_test <- read.csv('UCI_Dataset/test/subject_test.txt', sep = "", header=FALSE)
names(dst_subject_test) <- c("subject")
## combine to get testing data
dst_test <- cbind(dst_subject_test, dst_y_test$activity_name, dst_x_test)
names(dst_test) <- c("subject", "activity", feature_list)

## now combine training and testing dataset
## please note not all variables/columns are named nicely
dst_all <- rbind(dst_train, dst_test)

########################################################################################################
## section 2 - Extracts only the measurements on the mean and standard deviation for each measurement ##
########################################################################################################
## use regular expression, only keep variables ended with mean() or std()
## per TA David Hood
## https://class.coursera.org/getdata-004/forum/thread?thread_id=106
## what columns are measurements on the mean and standard deviation
feature_selected <- grep('(mean|std)\\(\\)$', feature_list, ignore.case = TRUE)
## need to +2 since first two columns are subject and activity
feature_selected <- c(1,2,feature_selected+2)
dst_selected <- dst_all[, feature_selected]

########################################################################################
## section 3 - Uses descriptive activity names to name the activities in the data set ##
########################################################################################
## this part is done in section 1
## validate
head(dst_selected)

###################################################################################
## section 4 - Appropriately labels the data set with descriptive variable names ##
###################################################################################
## remove brackets "()" and replace dash "-" with underscore "_"
names(dst_selected) <- sub('-(mean|std)\\(\\)', '_\\1', names(dst_selected))

##########################################################################
## Section 5 - Creates a second, independent tidy data set              ##
## with the average of each variable for each activity and each subject ##
##########################################################################
## per David Hood (TA) 
## both wide and narrow form are fine
## https://class.coursera.org/getdata-004/forum/thread?thread_id=106
## (Is the wide or narrow form of the data tidy?)

## make a narrow dataset using melt
library(reshape2)
dst_narrow <- melt(dst_selected, id=c(1,2), measure.vars=3:ncol(dst_selected))
## calcuate the mean by subject, activity, variable
library(plyr)
dst_tidy <- ddply(dst_narrow, .(subject, activity, variable), summarize, mean=mean(value))
## save the data
save(dst_tidy, file = 'tidy_data.RData')