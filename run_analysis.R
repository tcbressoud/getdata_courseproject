# Thomas C. Bressoud
# 14 February 2016
# Getting and Cleaning Data: Course Project

library(dplyr)
library(tidyr)
library(stringr)

# Function: transformname, where name is a character() and 
# result is a character()

# Helper function that takes a variable name and transforms it into something
# more descriptive for the non-domain expert.  In particular, a 't' for a 
# time reference is expanded to 'time', an 'f' to 'freq', variables are
# converted to lower case, acc maps to accel, parenthesis are removed,
# and the remaining hyphens separating attribute from mean/std and x/y/z
# are converted to underscores.
transformname <- function(name) {
  newname <- tolower(sub("\\(\\)", "", name))
  newname <- sub("^t", "time", newname)
  newname <- gsub("acc", "accel", newname)
  newname <- sub("^f", "freq", newname)
  newname <- gsub("-", "_", newname)
  newname
}

# Function: read_har_subset
# Gathers common steps for 'train' and 'test' into a single function to read
# the file, select only a set of the columns (for this assignment, the ones
# with 'mean' or 'std' in the name), and label the columns.
# returns a tbl (which inherits from data.frame)
read_har_subset <- function(subdir = "train", featurenames, relavantindices = 1:561) {
  if (!(subdir == "train" | subdir == "test")) {
    stop("invalid subdirector for dataset")
  }
  if (length(featurenames) != length(relavantindices)) {
    stop("featurenames vector of incorrect length")
  }
  filename <- sprintf("%s/X_%s.txt", subdir, subdir)
  features <- tbl_df(read.table(filename, header = FALSE, stringsAsFactors = FALSE)) %>%
    select(relavantindices)
  names(features) <- featurenames
  
  filename <- sprintf("%s/subject_%s.txt", subdir, subdir)
  subject <- tbl_df(read.table(filename, header = FALSE, stringsAsFactors = FALSE))
  names(subject) <- c("subject")
  
  filename <- sprintf("%s/y_%s.txt", subdir, subdir)
  activity <- tbl_df(read.table(filename, header = FALSE, stringsAsFactors = FALSE))
  names(activity) <- c("activity")
  
  df <- tbl_df(cbind(subject, activity, features))
  df
}

# Function: run_analysis
# Main driver function to take assignment through the required five steps.
# Precondition: current directory is the root of the supplied data set.
# Returns: the tidy data set arrived at through the combination of 
# multiple files, selecting of columns, merging training and test
# data sets, and aggregating averages across keys of subject/activity
# pairs.
run_analysis <- function() {
  
  # Defensive programming to ensure we are in the right directory
  neededcontents <- c("activity_labels.txt", "features.txt", "test", "train")
  dircontents <- dir()
  for (name in neededcontents) {
    matches <- grep(name, dircontents)
    if (length(matches) == 0) {
      stop("needed files not found")
    }
  }

  # Create features as a data table with columns 'index' and 'name' for the 561 features
  # Note the absence of a header line, and a space to separate index from feature name in
  # the text file.
  featurenames <- tbl_df(read.delim("features.txt", sep = " ", header = FALSE, stringsAsFactors = FALSE))
  names(featurenames) <- c("index", "name")
  
  # Determine the indices for the desired columns and their corresponding names
  meanstdindices <- grep("mean\\(|std\\(", featurenames$name)
  namevec <- sapply(featurenames$name[meanstdindices], transformname)
  N <<- namevec
  # Retrieve data frames for the train dataset and test dataset
  train <- read_har_subset("train", namevec, meanstdindices)
  test <- read_har_subset("test", namevec, meanstdindices)
  # ... and combine
  df <- tbl_df(rbind(train, test))

  # Next steps are to map the numbers representing activities into descriptive names
  # as given in a file in the hierarchy
  activities <- tbl_df(read.table("activity_labels.txt", header = FALSE, stringsAsFactors = FALSE))
  names(activities) = c("index", "label")

  df <- df %>% mutate(activity = tolower(activities$label[activity]))
  df <- df %>% arrange(subject, activity)
  
  # Finally, use tbl/dplyr facilities to aggregate variables into means based on
  # subject and activity.  This code is general, so that means are generated for
  # _all_ the remaining columns exclusive of subject and activity
  by_sa <- group_by(df, subject, activity)
  cols <- names(by_sa[-(1:2)])
  dots <- sapply(cols, function(x) substitute(mean(x), list(x=as.name(x))))
  sa_summary <- do.call(summarize, c(list(.data=by_sa), dots))
  sa_summary
}
