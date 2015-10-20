##Getting and Cleaning Data_Project _October 2015

##This script produces a tidy dataset from activity data obtained using smartphones. 
##The final script is an average of sensor data by activity by subject. 
##Dataset name is: sensor_avg_by_act_sub

##Full details http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 


library(plyr)

# Get files from here:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


## Unpack the dataset:

uci_data <- "UCI\ HAR\ Dataset"
feature_file <- paste(uci_data, "/features.txt", sep = "")
activity_labels_file <- paste(uci_data, "/activity_labels.txt", sep = "")
x_train_file <- paste(uci_data, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_data, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_data, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uci_data, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_data, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_data, "/test/subject_test.txt", sep = "")


# Load raw data
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)


# 1. Merge training and test sets to create one data set

# Binding sensor data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels


# 2. Extracts only the mean and standard deviation for each measurement
sensor_data_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]


# 3. Uses descriptive activity names to name the activities in the data set
sensor_data_mean_std <- join(sensor_data_mean_std, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean_std <- sensor_data_mean_std[,-1]


# 4. Appropriately labels the data set with descriptive names
# Take out parentheses
names(sensor_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)
# Make valid names
names(sensor_data_mean_std) <- make.names(names(sensor_data_mean_std))
# Make clear names
names(sensor_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Gyro',"AngularVelocity",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.mean',".Mean",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_std))


# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
sensor_avg_by_act_sub = ddply(sensor_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "sensor_avg_by_act_sub.txt", row.names = FALSE)
