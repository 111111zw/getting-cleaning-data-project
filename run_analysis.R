# run_analysis.R
# Getting and Cleaning Data Course Project
# This script processes the Human Activity Recognition Using Smartphones Dataset

# Load required packages
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

cat("Starting data processing...\n")

# 1. Download and extract data (if not already done)
if (!file.exists("UCI HAR Dataset")) {
  cat("Downloading dataset...\n")
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, destfile = "Dataset.zip", method = "curl")
  unzip("Dataset.zip")
  cat("Dataset downloaded and extracted successfully.\n")
} else {
  cat("Dataset already exists. Proceeding with processing...\n")
}

# 2. Read all data files
cat("Reading data files...\n")

# Read feature names (variable names)
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("index", "feature"))

# Read activity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", 
                              col.names = c("code", "activity"))

# Read training data
train_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
train_activities <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity_code")
train_data <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature)

# Read test data
test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
test_activities <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity_code")
test_data <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature)

cat("Data files read successfully.\n")

# 3. Step 1: Merge training and test sets
cat("Merging training and test sets...\n")
merged_data <- rbind(
  cbind(train_subjects, train_activities, train_data),
  cbind(test_subjects, test_activities, test_data)
)

# 4. Step 2: Extract only mean and standard deviation measurements
cat("Extracting mean and standard deviation measurements...\n")
mean_std_data <- merged_data %>%
  select(subject, activity_code, contains("mean"), contains("std"))

# 5. Step 3: Use descriptive activity names
cat("Applying descriptive activity names...\n")
mean_std_data <- mean_std_data %>%
  mutate(activity = factor(activity_code, 
                           levels = activity_labels$code, 
                           labels = activity_labels$activity)) %>%
  select(-activity_code)  # Remove numeric activity code column

# 6. Step 4: Appropriately label with descriptive variable names
cat("Cleaning variable names...\n")
# Get current column names
current_names <- names(mean_std_data)

# Clean up variable names for better readability
cleaned_names <- current_names %>%
  gsub("^t", "Time", .) %>%           # Replace 't' at start with 'Time'
  gsub("^f", "Frequency", .) %>%      # Replace 'f' at start with 'Frequency'
  gsub("Acc", "Accelerometer", .) %>% # Expand 'Acc' to 'Accelerometer'
  gsub("Gyro", "Gyroscope", .) %>%    # Expand 'Gyro' to 'Gyroscope'
  gsub("Mag", "Magnitude", .) %>%     # Expand 'Mag' to 'Magnitude'
  gsub("BodyBody", "Body", .) %>%     # Correct duplicate 'Body'
  gsub("\\.+", "", .)                 # Remove dots

# Apply cleaned names
names(mean_std_data) <- cleaned_names

# 7. Step 5: Create independent tidy data set with averages
cat("Creating tidy data set with averages...\n")
tidy_averages <- mean_std_data %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")

# 8. Save the final tidy data set
cat("Saving tidy data set...\n")
write.table(tidy_averages, "tidy_data.txt", row.names = FALSE)

# 9. Print summary information
cat("\n=== PROCESSING COMPLETE ===\n")
cat("Final dataset dimensions:", dim(tidy_averages), "\n")
cat("Number of subjects:", n_distinct(tidy_averages$subject), "\n")
cat("Number of activities:", n_distinct(tidy_averages$activity), "\n")
cat("File saved as: tidy_data.txt\n")

# View the first few rows of the result
cat("\nFirst few rows of the tidy dataset:\n")
print(head(tidy_averages))