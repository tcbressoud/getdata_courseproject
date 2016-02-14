
# README for Course Project Assignment

Author: Thomas C. Bressoud
Date: 14 Feb 2016

This README accompanies the three other deliverables for the Course Project for the Getting and Cleaning Data Coursera course.  The other three deliverables are:

1. **run_analysis.R**: The R script that, when executed with the current working directory being the root of the UCI HAR dataset, will create, clean, merge, and transform the dataset, per the assignment specification, into a dataset keyed by subject and activity, with aggregated columns for each of the means of the mean and std variables in the original dataset.
The main processing function is named <tt>run\_analysis()</tt>, has no arguments, and is simply invoked when in the proper directory.

2. **CodeBook.md**: A markdown file describing the resultant dataset.

3. **dataset.txt**: A text file containing the resultant dataset, and obtained through a write.table invocation.