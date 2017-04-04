###This code is a part of the programming assignment of "Getting and cleaning data"

# Set path for the data.
path <- getwd()
path

# Download the data and put the data in a folder.
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, data))

# Use 7-zip or winrar to unzip the file, run the below code for the same
# check to see which folder of "C" has the 7-zip or winrar
# Generally it is present in  Program Files or Program Files (x86) (Windows)
executable <- file.path("C:", "Program Files", "7-Zip", "7z.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, data), "\""))
system(cmd)

# Use the folder named UCI HAR Dataset to put the files
# Use this folder as the input path for the code. 
# List the files of the folder here.
pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive=TRUE)

# Use the below code to load the packages.
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# Reading the subject files.
dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(pathIn, "test" , "subject_test.txt" ))

# Reading the activity files.
dtActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest  <- fread(file.path(pathIn, "test" , "Y_test.txt" ))

# Reading the data files. 
fileToDT <- function (data) {
        df <- read.table(data)
        dt <- data.table(df)
}
dtTrain <- fileToDT(file.path(pathIn, "train", "X_train.txt"))
dtTest  <- fileToDT(file.path(pathIn, "test" , "X_test.txt" ))

# Concatenating the above data tables as per requirement.
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

# Merging the data
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

# use Set key to reorder (or sorts) the rows of a dt by the columns provided.
setkey(dt, subject, activityNum)


### Extract only the mean and standard deviation

# Read the features.txt file.
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

# Subsetting the measurements for the mean and standard deviation.
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

# Convert the column numbers to a vector of variable names matching columns in dt.
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]

# Subset these variables using variable names.
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]


### Using the descriptive activity names
# Read activity_labels.txt file to add descriptive names to the activities.

dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

# Labeling descriptive activity names and Merging.
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

# Add activityName as a key.
setkey(dt, subject, activityNum, activityName)

# Reshaping from a short and wide format to a tall and narrow format.
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

# Merging the activity name.
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

# Creating a new variable, activity.
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

# Use the helper function grepthis to Seperate features from featureName.
grepthis <- function (regex) {
        grepl(regex, dt$feature)
}
# Features: 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

# Features: 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

# Features: 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

# Creating a tidy data set
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

# Saving the data table objects to a text file named DS_human_activity_recognition_using_smartphones.txt.

data <- file.path(path, "DS_human_activity_recognition_using_smartphones.txt")
write.table(dtTidy, data, quote=FALSE, sep="\t", row.names=FALSE)