######################################################################################
# File Name: GetMetadata.R                                                           #
# Purpose: Summarise metadata from EEG data files                                    #
#                                                                                    #
# Author: Ian Watson                                                                 #
# Email1: d13128934@mydit.ie                                                         #
# Email2: iantwatson@gmail.com                                                       #
#                                                                                    #
# Institution: Dublin Institute of Technology                                        #
# Course Code: DT228B                                                                #
# Course Title: MSc. Data Analytics                                                  # 
# Date of Dissertation Commencement: September 2017                                  #
# Title: A Comparison of SVM and Neural Network Classifiers for Epileptic Seizure    #
# Prediction                                                                         #
#                                                                                    #
# R code for implementing a machine learning classification experiment to compare    #
# the performance of SVM and neural network classifiers used for epileptic seizure   #
# prediction                                                                         #
#                                                                                     #
######################################################################################

# This file should be called from the main code file SeizurePrediction.R

# Get metadata for all files and summarise

# Initialise row counter
inum <- 1

# Set up logical array with 8000 elements ~ total number of data files
log.array <- rep(NA, 8000)
# Create results data frame
meta.data.results <- data.frame(filename = log.array, seconds = log.array,
                                frequency = log.array, channels = log.array,
                                labels = log.array, sequence = log.array,
                                samples = log.array)

# Loop for all patients' folders
for (folder in folder.list) {
  
  # Set working directory here
  metadata.dir <- paste0(data.dir, folder)
  setwd(metadata.dir)
  
  # Get list of files for processing
  list.of.files <- dir(metadata.dir, "*.mat")
  
  # Remove existing log file
 # file.remove(paste0(folder, "_metadata_summary.txt"))
  
  # Create log file
  logFile = paste0(folder, "_metadata_summary.txt")
  
  # Loop for all files in the patient folder
  for (filename in list.of.files) {

    # Message to console
    cat(paste0("Getting metadata ", filename, " ", Sys.time()), "\n")
    # Read in .mat file
    EEG.file <- read.EEG.file(filename)
    # Get length of file segment in seconds
    EEG.data <- EEG.file[["mat"]]
    seconds <- EEG.file[["seconds"]]
    # Get sample frequency
    frequency <- EEG.file[["frequency"]]
    # Get number of channels
    chan <- EEG.file[["channel"]]
    # Get sequence number of file
    sequence <- EEG.file[["sequence"]]
    # Get electrode labels
    labels <- EEG.file[["labels"]]
    # Get electrode labels
    labels <- EEG.file[["labels"]]
    # Write to result data frame
    meta.data.results[inum, ] <- c(filename,
                                   EEG.file[["seconds"]],
                                   EEG.file[["frequency"]],
                                   EEG.file[["channel"]],  # No. channels
                                   EEG.file[["labels"]][1],
                                   EEG.file[["sequence"]],
                                   ncol(EEG.file[[1]]))  # No. samples
    # Increment row counter
    inum=inum+1
    # Write to log file
    cat(paste0("Checked ", filename, " ", Sys.time()),  # Write to log
        file = logFile, append = TRUE, sep = "\n")
  }
}

# Remove any rows not used
meta.data.results <- meta.data.results[complete.cases(meta.data.results), ]

# Convert to factor
coltofact <- c("seconds", "frequency", "channels", "labels", "sequence", "samples")
meta.data.results[coltofact] <- lapply(meta.data.results[coltofact], factor)

# Reset working directory
setwd(data.dir)

# Save results to a .rda file
save(meta.data.results, file = "metadata.rda")

######################################################################################