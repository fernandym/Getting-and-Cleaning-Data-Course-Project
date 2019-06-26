#Load the library
library(dplyr)

#Set the URL dan file name of data
URL      = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile = "UCI_HAR_DS.zip"

#Checking the file if archieve already exists or our WD 
URLFile  =  paste0(paste0(getwd(),"/"),destfile)
if (!file.exists(URLFile)){
  download.file(URL, destfile)
}  

#Unzip file to WD
ZipFile = 'UCI_HAR_DS'
if (!file.exists(ZipFile)){
  unzip(URLFile) 
} 

#Get data from file and put populate it into data frame
df_activities_labels    = read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("act_code", "activity"))
head(df_activities_labels)
df_features             = read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
head(df_features)
df_subject_train        = read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
head(df_subject_train)
df_x_train              = read.table("UCI HAR Dataset/train/X_train.txt",  col.names = df_features$functions)
head(df_x_train)
df_y_train              = read.table("UCI HAR Dataset/train/y_train.txt", col.names="act_code")
head(df_y_train)
df_subject_test         = read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
head(df_subject_test)
df_x_test               = read.table("UCI HAR Dataset/test/X_test.txt", col.names = df_features$functions)
head(df_x_test)
df_y_test               = read.table("UCI HAR Dataset/test/y_test.txt", col.names = "act_code")
head(df_y_test)

#Combine Subject data rows
print(nrow(df_subject_train))
print(nrow(df_subject_test))
df_subject = rbind(df_subject_train, df_subject_test)
print(nrow(df_subject)) #10299 rows

#Combine x data rows
print(nrow(df_x_train))
print(nrow(df_x_test))
df_x = rbind(df_x_train, df_x_test)
print(nrow(df_x)) #10299 rows

#Combine y data rows
print(nrow(df_y_train))
print(nrow(df_y_test))
df_y = rbind(df_y_train, df_y_test)
print(nrow(df_y)) #10299 rows

#1. Merges the training and the test sets to create one data set
df_merge_all = cbind(df_subject, df_x, df_y)
print(nrow(df_merge_all))

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
df_merge_all_mean_std = df_merge_all %>% select(subject, act_code, contains("mean"), contains("std"))
print(nrow(df_merge_all_mean_std))

#3. Use descriptive activity names to name the activities in the data set and remove old act_code
df_merge_all_act_desc = merge(df_merge_all_mean_std, df_activities_labels, by='act_code', all.x=TRUE) 
df_merge_all_act_desc$subject
df_merge_all_act_desc_tidy = df_merge_all_act_desc[, !names(df_merge_all_act_desc) %in% c("act_code")] 

#4. Appropriately labels the data set with descriptive variable names.
names(df_merge_all_act_desc_tidy) = gsub("tBody", "TimeBody", names(df_merge_all_act_desc_tidy))
names(df_merge_all_act_desc_tidy) = gsub("-mean()", "Mean", names(df_merge_all_act_desc_tidy), ignore.case = TRUE)
names(df_merge_all_act_desc_tidy) = gsub("-std()", "STD", names(df_merge_all_act_desc_tidy), ignore.case = TRUE)
names(df_merge_all_act_desc_tidy) = gsub("-freq()", "Frequency", names(df_merge_all_act_desc_tidy), ignore.case = TRUE)
names(df_merge_all_act_desc_tidy) = gsub("angle", "Angle", names(df_merge_all_act_desc_tidy))
names(df_merge_all_act_desc_tidy) = gsub("gravity", "Gravity", names(df_merge_all_act_desc_tidy))
names(df_merge_all_act_desc_tidy) = gsub("Acc", "Accelerometer", names(df_merge_all_act_desc_tidy))
names(df_merge_all_act_desc_tidy) = gsub("Gyro", "Gyroscope", names(df_merge_all_act_desc_tidy))
names(df_merge_all_act_desc_tidy) = gsub("BodyBody", "Body", names(df_merge_all_act_desc_tidy))
names(df_merge_all_act_desc_tidy) = gsub("Mag", "Magnitude", names(df_merge_all_act_desc_tidy))
names(df_merge_all_act_desc_tidy) = gsub("^t", "Time", names(df_merge_all_act_desc_tidy))
names(df_merge_all_act_desc_tidy) = gsub("^f", "Frequency", names(df_merge_all_act_desc_tidy))

head(df_merge_all_act_desc_tidy)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
df_merge_all_avg <- aggregate(. ~subject + activity , df_merge_all_act_desc_tidy, mean)
df_merge_all_avg_tidy <- df_merge_all_avg[order(df_merge_all_avg$subject, df_merge_all_avg$activity),]

df_merge_all_avg_tidy

str(df_merge_all_agg_tidy)
# Save the data
write.table(df_merge_all_agg_tidy, "df_merge_all_agg_tidy.txt", row.name=FALSE)
