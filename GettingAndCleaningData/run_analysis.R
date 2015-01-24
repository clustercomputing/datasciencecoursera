library(data.table)
library(plyr)

# set your working directory, so the files you want to read are in path
# setwd("")

# Read training data
training.data <- read.table("UCI HAR Dataset/train/X_train.txt"); training.data <- as.data.table(training.data)
        ## training labels        
        training.labels <- read.table("UCI HAR Dataset/train/y_train.txt"); training.labels <- as.data.table(training.labels)
                ## changing column labels
                setnames(training.labels,"V1","labels")
# Merging training data with training labels
training.data <- cbind(training.data,training.labels)
rm(training.labels)
# Merging subject data with training data
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt"); subject.train <- as.data.table(subject.train)
setnames(subject.train,"V1","subjects")
training.data <- cbind(training.data,subject.train)
rm(subject.train)

# Reading Test data
test.data <- read.table("UCI HAR Dataset/test/X_test.txt"); test.data <- as.data.table(test.data)
        ## test labels
        test.labels <- read.table("UCI HAR Dataset/test/y_test.txt"); test.labels <- as.data.table(test.labels)
        ## changing column labels
        setnames(test.labels,"V1","labels")
# Merging test data with test labels
test.data <- cbind(test.data,test.labels)
rm(test.labels)
# Merging subject data with test data
subject.test <- read.table("UCI HAR Dataset/test//subject_test.txt"); subject.test <- as.data.table(subject.test)
setnames(subject.test,"V1","subjects")
test.data <- cbind(test.data,subject.test)
rm(subject.test)

# check if the columns and col order match between training & test data
a <- colnames(training.data)
b <- colnames(test.data)
if (table(a==b)) (print("Cols and order match")); rm(list=c("a","b"))  
        
# Merge training & test data sets, since they have same col names & order 
training.test.data <- rbind(training.data,test.data)
        ## verify if the dimensions (col & rows) match up
        dim(training.test.data)[1] == dim(training.data)[1]+dim(test.data)[1]
        rm(list=c("training.data","test.data"))

# Adds features labels to data
        ## get the column values
features <- read.table("UCI HAR Dataset/features.txt"); features <- as.data.table(features)
        colnam <- grep("V",colnames(training.test.data))
        ## validate data sets col var are equal to features V1 col
        table(colnam==features$V1)
        ## get features description, which will be used as column names for data
        features.description <- features$V2
        rm(colnam)
        ## remove "V" from data columns
        setnames(training.test.data,grep("V",colnames(training.test.data),value=T),gsub("V","",grep("V",colnames(training.test.data),value=T)))
        ## insert features description into data column names
setnames(training.test.data,grep("^[0-9]",colnames(training.test.data),value=T),as.character(features.description))
        rm(features.description); rm(features)
        
# Add activit labels to data
activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt"); activity.labels <- as.data.table(activity.labels)
        setnames(activity.labels,c("V1","V2"),c("labels","activity_labels"))
        # validata if in data all activity labels have activity description in activity labels
        table(training.test.data$labels%in%activity.labels$labels)
training.test.data <- join(training.test.data,activity.labels,by="labels",match="all")
        training.test.data[,"labels":=NULL]
rm(activity.labels)

# Getting mean and standard deviation measurements for each treatment
data <- subset(training.test.data, select=c("subjects","activity_labels",grep("-mean()",colnames(training.test.data),value=T,fixed=T),grep("-std()",colnames(training.test.data),value=T,fixed=T)))
data <- data[order(activity_labels,subjects)]
rm(training.test.data)

# Tidy data set with average of each variable for each activity, each subject
mean.by.subject.activity <- data[,lapply(.SD,mean),by=c("subjects","activity_labels")]

write.table(mean.by.subject.activity,"output/mean_by_subject_activity.txt",row.names=F)

