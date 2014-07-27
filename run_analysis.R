
setwd("~/Coursera/Data Cleaning")
##Step 1. Download the data from the website into readable format

##Checks to see if the directory already exists, otherwise creates
##a directory projdata.  Then download the file from fileurl
##and save it as rawfile.zip. 
if(!file.exists("./projdata")){
                              dir.create("./projdata")
                              }

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dfile <- "./projdata/rawfile.zip"
download.file(fileUrl, destfile=dfile)
dateDownloaded <- date()
##Unzip file
unzip(dfile, overwrite=TRUE, exdir="./projdata")

##Step 2. Merge x training and test data sets to create one data set
##into merged dataframe: mergeddata

##read features.txt file data into dataframe - these are the column headers for train & test x data
featuresdata <- read.table("./projdata/UCI HAR Dataset/features.txt")

#####create dataframe with all x data######
##read x_test.txt file into dataframe
testxdata <- read.table("./projdata/UCI HAR Dataset/test/X_test.txt")

##create new factor column that identifies x data in test data frame as being test data
nrowtestxdata <- nrow(testxdata)
test <-factor(rep(c("test"), times = nrowtestxdata))
testxdata$group <- test

##read x_train.txt file into dataframe
trainxdata <- read.table("./projdata/UCI HAR Dataset/train/X_train.txt")

##create new factor column that identifies x data in train data frame as being train data
nrowtrainxdata <- nrow(trainxdata)
train <-factor(rep(c("train"), times = nrowtrainxdata))
trainxdata$group <- train

##bind rows from train and test data to create merged data set
mergeddata <- rbind(testxdata,trainxdata)

##name the columns in mergeddata
colnames(mergeddata) <- featuresdata$V2
names(mergeddata)[562] <- "group"

####Step 3 extracts only the columns relating to mean and std deviation
####and re-label columns with R friendly readable names
subsetmeanstd<- mergeddata[,grep("[Mm]ean|[Ss]td|group", colnames(mergeddata))]
names(subsetmeanstd) <- gsub("-","",names(subsetmeanstd), fixed = TRUE)
names(subsetmeanstd) <- sub(",","",names(subsetmeanstd), fixed = TRUE)
names(subsetmeanstd) <- sub("()","",names(subsetmeanstd), fixed = TRUE)
names(subsetmeanstd) <- sub("(","",names(subsetmeanstd), fixed = TRUE)
names(subsetmeanstd) <- sub(")","",names(subsetmeanstd), fixed = TRUE)
names(subsetmeanstd) <- tolower(names(subsetmeanstd))

####Step 4 binds y test and train data and labels column activitylabel
##read y_test.txt and y_train.txt files into dataframes and bind
testydata <- read.table("./projdata/UCI HAR Dataset/test/y_test.txt")
trainydata <- read.table("./projdata/UCI HAR Dataset/train/y_train.txt")
ydatabind <- rbind(testydata,trainydata)

##add new column to merged data frame with factors identifying activity
activitylabels <- read.table("./projdata/UCI HAR Dataset/activity_labels.txt")
subsetmeanstd$activitylabel <- ydatabind
subsetmeanstd$activitylabel <- factor(ydatabind,levels = activitylabels$V1,labels = activitylabels$V2)

####Step 5 binds subject ID's 
##read subject_test.txt and subject_train.txt files into dataframes and bind
testsubjectdata <- read.table("./projdata/UCI HAR Dataset/test/subject_test.txt")
trainsubjectdata <- read.table("./projdata/UCI HAR Dataset/train/subject_train.txt")
subjectdatabind <- rbind(testsubjectdata, trainsubjectdata)

##new column with factors identifying subject id
subsetmeanstd$subject <- subjectdatabind

####Step 6 Create a new data frame with the average of each variable 
##for each activity and each subject
attach(subsetmeanstd)
aggdata <- aggregate(subsetmeanstd, by=list(subsetmeanstd$subject, subsetmeanstd$activitylabel), FUN=mean, na.rm=TRUE)
print(aggdata)
detach(subsetmeanstd)
