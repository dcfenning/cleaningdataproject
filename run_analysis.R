
##Step 1. Downloads the data from the website into readable format

##Checks to see if the file already exists, otherwise creates
##a directory projdata.  Then downloads the file from fileurl
##and save it as rawfile.zip. 
if(!file.exists("./projdata/rawfile.zip")){
                              dir.create("./projdata")
                              fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                              dfile <- "./projdata/rawfile.zip"
                              download.file(fileUrl, destfile=dfile)
                              dateDownloaded <- date()                          
                              }

##Unzip file
unzip(dfile, overwrite=TRUE, exdir="./projdata")

##Step 2. Read all required .txt files into dataframes ready for tidying and bind together 
##label column variables

##read features.txt file data into dataframe - these are the column headers for train & test x data
featuresdata <- read.table("./projdata/UCI HAR Dataset/features.txt") 

##read test files into dataframes
testsubjectdata <- read.table("./projdata/UCI HAR Dataset/test/subject_test.txt")
testydata <- read.table("./projdata/UCI HAR Dataset/test/y_test.txt")
testxdata <- read.table("./projdata/UCI HAR Dataset/test/X_test.txt")

##read train files into dataframes
trainsubjectdata <- read.table("./projdata/UCI HAR Dataset/train/subject_train.txt")
trainydata <- read.table("./projdata/UCI HAR Dataset/train/y_train.txt")
trainxdata <- read.table("./projdata/UCI HAR Dataset/train/X_train.txt")

##read activity labels
activitylabels <- read.table("./projdata/UCI HAR Dataset/activity_labels.txt")

# Bind the test dataset to add subject and label
testdata <- cbind(testsubjectdata,testydata,testxdata)
# Bind the training dataset to add subject and label
traindata <- cbind(trainsubjectdata,trainydata,trainxdata)
# Merge the test dataset and the training dataset
mergeddata <- rbind(traindata,testdata)

##name the columns in mergeddata
featuredata <- as.character(featuresdata$V2)
colnames(mergeddata) <- c("subject", "activity",featuredata)

####Step 3 extracts only the columns relating to mean and std deviation
####and re-label columns with R friendly readable names

##Extract only columns relating to mean and st deviation
indexMeanStd <- grep("[Mm]ean|[Ss]td|group",featuredata)
tidydata <- mergeddata[,c(1,2,2+indexMeanStd)] # Add 2 to indexMeanStd because of the additional Subject & Activity columns

##Tidy up column names
names(tidydata) <- gsub("-","",names(tidydata), fixed = TRUE)
names(tidydata) <- sub(",","",names(tidydata), fixed = TRUE)
names(tidydata) <- sub("()","",names(tidydata), fixed = TRUE)
names(tidydata) <- sub("(","",names(tidydata), fixed = TRUE)
names(tidydata) <- sub(")","",names(tidydata), fixed = TRUE)
names(tidydata) <- tolower(names(tidydata))

####Step 4 convert Activity variable to a factor variable using activitylabels table column 2
tidydata$activity <- factor(tidydata$activity, labels = activitylabels$V2)

####Step 5 Create a new second tidy data frame with the average for each variable 
##for each activity and each subject
tidydata2 <- aggregate(tidydata[3:ncol(tidydata)],by=list(tidydata$subject,tidydata$activity),mean, na.rm=TRUE)

##relabels column names to include the fact that the columns are now Means
tempName <- colnames(tidydata)
tempName <- sub("mean","AverageMean",tempName)
tempName <- sub("std","AverageStd",tempName)
colnames(tidydata2) <- tempName

####Step 6 export tidydata2 to .txt file

##write tidy data frame 2 to .txt file
write.table(tidydata2,"tidydataset2.txt", row.names = FALSE)

