
# for train data
titles <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/features.txt')[,2]
subject1 <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/train/subject_train.txt', col.names = "subject")
x1 <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/train/X_train.txt', col.names = titles)
y1 <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/train/y_train.txt', col.names = "activity")

# for test data
subject2 <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/test/subject_test.txt', col.names = "subject")
x2 <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/test/X_test.txt', col.names = titles)
y2 <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/test/y_test.txt', col.names = "activity")

### merge subject together
subject <- rbind(subject1, subject2)
x <- rbind(x1, x2)
y <- rbind(y1, y2)
data <- cbind(subject, x, y)


### extract only mean and sd values
ind <- grep("mean|std", names(data))
mean_sd <- data[, ind]

### step3: replace activity data in the last column by sequence
head(data[, ncol(data)]) # we can first look at data of the last column
# extract names of this column, and rename it!
last <- data[, ncol(data)]
lv <- read.table('/Users/hongmaggie/Downloads/UCI HAR Dataset/activity_labels.txt')[, 2]
factor(last, labels = lv)
data[, ncol(data)] <- last # put it back

### step4: rename data labels
# look at first character
unique(substr(names(data), 1, 1))
# rename them
patterns = c("^t", "^f", "Acc", "Mag")
replacements = c("Time", "Frequency", "Accelerometer", "Magnitude")
nam <- names(data)
for (i in seq_along(patterns)){
  nam <- gsub(patterns[i], replacements[i], nam)
}
names(data) <- nam

### a new tidy form of data
# preparations
library(tidyverse)
# group by "activity" and "subject"
# activity the last row and subject the first row
DA <- data |> select(contains(c("mean", "std")), activity, subject) |> 
  group_by(subject, activity) |>
  summarise(across(where(is.numeric), mean))

### output data back
write.table(DA, file = '/Users/hongmaggie/Downloads/UCI HAR Dataset/output.txt', row.name = FALSE)
