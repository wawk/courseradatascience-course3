## This script is assuming the working directory is at the level where test and train directories
## are visible. After the download and upnzip of data this is effectively the "UCI HAR Dataset"
## directory.

library(utils)

## Read in each X_test/X_train.txt into a data object
xtest <- read.table("./test/X_test.txt")
xtrain <- read.table("./train/X_train.txt")

## We want to rename the "V1" column label in these two files so first we need to read them into a temp
## data object (this is done for both train and test)
ytestsub <- read.table("./test/subject_test.txt")
ytest <- read.table("./test/y_test.txt")
ytrainsub <- read.table("./train/subject_train.txt")
ytrain <- read.table("./train/y_train.txt")


## The labels file (y_*.txt) and the activity file (subject_*.txt) are both labeled V1
## This code renames those columns appropriately
subjectLabel <- c("Subject")
activityLabel <- c("Activity")
colnames(ytestsub) <- subjectLabel
colnames(ytrainsub) <- subjectLabel
colnames(ytest) <- activityLabel
colnames(ytrain) <- activityLabel

## Create a matrix in each (test/train) combining all appropriate (test or train) data objects using cbind
testDT <- cbind(xtest,ytestsub,ytest)
trainDT <- cbind(xtrain,ytrainsub,ytrain)
combineDT <- rbind(testDT,trainDT)

## get a list of the features
feat <- read.table("./features.txt")
feat_list <- feat[,2]
feat_vect <- as.character(unlist(feat_list))
feat_names <- c(feat_vect,c("Subject","Activity"))

## set the column names for the combineDT
colnames(combineDT) <- feat_names

## Change the names in each row for the Activity to something more meaningful
combineDT$Activity <- as.character(combineDT$Activity)
combineDT$Activity <- sub("1","WALKING", combineDT$Activity)
combineDT$Activity <- sub("2","WALKING_UPSTAIRS", combineDT$Activity)
combineDT$Activity <- sub("3","WALKING_DOWNSTAIRS", combineDT$Activity)
combineDT$Activity <- sub("4","SITTING", combineDT$Activity)
combineDT$Activity <- sub("5","STANDING", combineDT$Activity)
combineDT$Activity <- sub("6","LAYING", combineDT$Activity)

## split out the data referencing the mean() and std() variables
mean_regex <- grep("mean()-", feat_names, value=T, fixed=T)
std_regex <- grep("std()-", feat_names, value=T, fixed=T)
meanDT <- subset(combineDT, select=c(mean_regex,c("Subject","Activity")))
stdDT <- subset(combineDT, select=c(std_regex, c("Subject", "Activity")))

## create a simple vector of just the sensor measurement names
meanDTnames <- as.vector(names(meanDT)[1:24])
td <- function(x){strsplit(x,"-")[[1]][1]}
str <- sapply(meanDTnames,td)

strvec <- unique(as.vector(str[1:24]))

df <- data.frame(
  sensor=rep(NA,1),
  mean_std=rep(NA,1),
  subject_id=rep(NA,1),
  activity=rep(NA,1),
  x_measure=rep(NA,1),
  y_measure=rep(NA,1),
  z_measure=rep(NA,1),
  stringsAsFactors = F)

t <- function(x,y) {
  subMeanDT <- subset(meanDT, meanDT["Subject"] == y)
  a <- split(subMeanDT[x],subMeanDT["Activity"])
  a
}

tstd <- function(x,y) {
  subStdDT <- subset(stdDT, stdDT["Subject"] == y)
  a <- split(subStdDT[x],subStdDT["Activity"])
}

dfrows <- nrow(df)

mean_std <- ""

r <- function(x) {
  for (sensr in x) {
    valx <- paste0(sensr,"-",mean_std,"()-X")
    valy <- paste0(sensr,"-",mean_std,"()-Y")
    valz <- paste0(sensr,"-",mean_std,"()-Z")
    for(subj in 1:30) {
      subject <- as.character(subj)
      if(mean_std == "mean") {
        a <- t(valx,subject)
        b <- t(valy,subject)
        c <- t(valz,subject)
      } else {
        a <- tstd(valx,as.character(subj))
        b <- tstd(valy,as.character(subj))
        c <- tstd(valz,as.character(subj))
      }
      sensor <- sensr
      for(index in 1:6) {
        activity <- names(a[index])
        xaxis <- mean(unlist(a[index]))
        yaxis <- mean(unlist(b[index]))
        zaxis <- mean(unlist(c[index]))
        df[dfrows,] <- c(sensor,mean_std,subject,activity,xaxis,yaxis,zaxis)
        dfrows <- dfrows + 1
      }
    }
  }
  df
}  

mean_std <- "mean"
df3 <- r(strvec)
mean_std <- "std"
df4 <- r(strvec)
finalDF <- rbind(df3,df4)

## Uncomment out the line below to create a txt file of the final tidy data set.
## write.table(finalDF,"tidy_human_activity_recognition.txt", row.names = F)