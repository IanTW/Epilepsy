######################################################################################
# File Name: IntegrityCheck.R                                                        #
# Purpose: Integrity check EEG data files                                            #
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

# This file should be called from the main code file SeizurePrediction.R

# Loop for all patients' folders
for (folder in folder.list){   
  
  # Set working directory here
  patient.dir <- paste0(data.dir, folder)
  setwd(patient.dir)
  
  # Remove existing log file
  file.remove(paste0(folder, "_integtrity_check.txt"))
  
  # Create log file
  logFile = paste0(folder, "_integtrity_check.txt")
  
  # Get list of files for processing
  list.of.files <- dir(data.dir, "*.mat")
  
  # Loop for all files in the patient folder
  for (filename in list.of.files){
    tryCatch({
      a <- readMat(filename)  # Read in the file
      cat("Integrity checking", filename, "\n")  # Message to console
      cat(paste0("Checked ", filename, " ", Sys.time()),  # Write to log
          file = logFile, append = TRUE, sep = "\n")
      }, error=function(e){  # Error function  
        cat("ERROR :", conditionMessage(e), filename, "\n")  # Message to console
        cat(paste0("Error in ", filename, " ", Sys.time()),  # Write to log
            file = logFile, append = TRUE, sep = "\n")
        })
  }
  # Write to log
  cat(paste0("Checked ", length(list.of.files), " files. Finished at ", Sys.time()),
      file = logFile, append = TRUE, sep = "\n")
}

######################################################################################