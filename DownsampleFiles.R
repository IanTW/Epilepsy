######################################################################################
# File Name: DownsampleFiles.R                                                       #
# Purpose: Resample to 400Hz and get 16 channels per file                            #
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

######################################################################################

# Function to get a standard number of EEG channels
standard.EEG.data <- function (EEG.data, chan){
  # Standardise the number of channels to 16. Some files have 15 or 24 channels.
  #
  # Args:
  #   EEG.data: Multichannel EEG data in a matrix; each row is an EEG channel
  #   chan: The number of EEG channels in the file
  #   
  # Returns:
  #   EEG.data: Multichannel EEG data in a matrix with 16 channels total
  
  # For the case of 15 channels, insert a row of zeroes
  if (chan == 15){
    EEG.data <- rbind(EEG.data, 0)
    
    # In the case of 24 channels, randomly select 16    
  } else if (chan == 24){
    set.seed(1256)  # For reproducability
    EEG.data <- EEG.data[sample(nrow(EEG.data), 16), ]
  }
  
  # Return
  EEG.data
}

######################################################################################

# Loop for all patients' folders
for (folder in folder.list){   
  
  # Set working directory here
  downsample.dir <- paste0(data.dir, folder)
  setwd(downsample.dir)
  
  # Get list of files for processing
  list.of.files <- dir(downsample.dir, "*.mat")
  
  # Loop for all files in the patient folder
  for (filename in list.of.files) {
    # Message to console
    cat(paste0("Processing ", filename, " ", Sys.time()), "\n")
    # Read in .mat file
    EEG.file <- read.EEG.file(filename)
    # Get time series data
    EEG.data <- EEG.file[["mat"]]
    # Get length of file segment in seconds
    seconds <- EEG.file[["seconds"]]
    # Get sample frequency
    frequency <- EEG.file[["frequency"]]
    # Get number of channels
    chan <- EEG.file[["channel"]]
    # Get sequence number of file
    sequence <- EEG.file[["sequence"]]
    # Get electrode labels
    labels <- EEG.file[["labels"]]
    
    # Fix number of channels
    EEG.data <- standard.EEG.data(EEG.data, chan)
    
    
}
}