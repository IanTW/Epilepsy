######################################################################################
# File Name: GetMetaData.R                                                           #
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
#                                                                                    #
# Parts of code adapted from Wei Wu & ESAI, Universidad CEU Cardenal Herrera,        #
# (F. Zamora-Martínez,    #F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo).        # 
#                                                                                    #
######################################################################################
# Copyright (c) 2014, Wei Wu                                                         #
#                                                                                    #
# Permission is hereby granted, free of charge, to any person obtaining a copy       #
# of this software and associated documentation files (the "Software"), to deal      #
# in the Software without restriction, including without limitation the rights       #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell          #
# copies of the Software, and to permit persons to whom the Software is              #
# furnished to do so, subject to the following conditions:                           #
#                                                                                    #
# The above copyright notice and this permission notice shall be included in all     #
# copies or substantial portions of the Software.                                    #
#                                                                                    #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR         #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,           #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE        #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER             #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,      #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS             #
# IN THE SOFTWARE.                                                                   #
#                                                                                    # 
# See: https://github.com/wei-wu-nyc/Kaggle-SeizureDetection-Official                #
#                                                                                    #
######################################################################################
# Copyright (c) 2014, ESAI, Universidad CEU Cardenal Herrera,                        #
# (F. Zamora-Martínez, F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo)             #
#                                                                                    #
# Permission is hereby granted, free of charge, to any person obtaining a copy       #
# of this software and associated documentation files (the "Software"), to deal      #
# in the Software without restriction, including without limitation the rights       #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell          #
# copies of the Software, and to permit persons to whom the Software is              #
# furnished to do so, subject to the following conditions:                           #
#                                                                                    #
# The above copyright notice and this permission notice shall be included in all     #
# copies or substantial portions of the Software.                                    #
#                                                                                    #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR         #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,           #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE        #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER             #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,      #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS             #
# IN THE SOFTWARE.                                                                   #
#                                                                                    # 
# See https://github.com/ESAI-CEU-UCH/kaggle-epilepsy                                #
#                                                                                    #
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
for (folder in patient.name) {
  
  # Set working directory here
  # This can be used to check the full dataset...
  data.dir <- paste0(full.dataset.dir, folder)
  # ...or it can be used to check the sample data
  # data.dir <- paste0(sample.dataset.dir, folder)
  setwd(data.dir)
  
  # Get list of files for processing
  list.of.files <- dir(data.dir, "*.mat")
  
  # Remove existing log file
  file.remove(paste0(folder, "_metadata_summary.txt"))
  
  # Create log file
  logFile = paste0(folder, "_metadata_summary.txt")
  
  # Loop for all files in the patient folder
  for (filename in list.of.files) {

    # Message to console
    cat(paste0("Getting metadata ", filename, " ", Sys.time()), "\n")
    # Read in .mat file
    EEG.file <- read.EEG.file(filename)
    # Get length of file segment in seconds
    seconds <- EEG.file[["seconds"]]
    # Get sample frequency
    freq <- EEG.file[["frequency"]]
    # Get sequence number of file
    seq <- EEG.file[["sequence"]]
    # Get electrode labels
    labels <- EEG.file[["labels"]]
    # Write to result data frame
    meta.data.results[inum, ] <- c(filename,
                                   EEG.file[["seconds"]],
                                   EEG.file[["freq"]],
                                   length(EEG.file[["labels"]]),  # No. channels
                                   EEG.file[["labels"]][1],
                                   EEG.file[["seq"]],
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

# Reset working directory
setwd(parent.dir)

# Save results to a .rda file
save(meta.data.results, file = "metadata.rda")

######################################################################################