######################################################################################
# File Name: ResampleData.R                                                          #
# Purpose: Resample EEG files                                                        #
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
#                                                                                    #
######################################################################################

# List of folders
folder.list = c("Patient_1", "Patient_2")

#folder = folder.list[1]
#filename = list.of.files[1]
#channels = 1

for (folder in folder.list){
  # Set working directory to source folder 
  setwd(paste0(data.dir, folder))
  patient.dir <- paste0(data.dir, folder)
  list.of.files <- dir(patient.dir, "*.mat")
  
  for (filename in list.of.files) {
    # Set directory for source file
    setwd(paste0(data.dir, folder))
    # Message to console
    cat(paste0("Reading ", filename, " ", Sys.time()), "\n")
    # Read in .mat file
    EEG.file <- read.EEG.file(filename)
    # Get time series data
    EEG.data <- EEG.file[["mat"]]
    # Get number of channels
    chan <- EEG.file[["channel"]]
    
    EEG.data.new <- matrix(NA, nrow = chan, ncol = 240000)
   
    cat(paste0("Processing ", filename, " ", Sys.time(), "\n"))
    for (channels in 1:chan) {
      # Get channel
      EEG.channel <- EEG.data[channels,]  # Get row from matrix
      # Downsample and write to new matrix
      EEG.data.new[channels,] <- resample(EEG.channel, p=1, q=12.5)
    }
    
    # Insert new frequency
    EEG.file[["frequency"]] <- 400
    # Overwrite old data
    EEG.file[["mat"]] <- EEG.data.new
  
    cat(paste0("Writing ", filename, " ", Sys.time()), "\n")
    #Set working directory for output file
    setwd(paste0(data.dir,"/Resample/",folder))
    writeMat(filename, A = EEG.file)
    
  }
  }