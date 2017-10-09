######################################################################################
# File Name: SeizurePrediction.R                                                     #
# Purpose: Main code sequence                                                        #
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

################################# SET UP AND LOAD PACKAGES ###########################

# Install and load required R packages 
# List of required packages
list.of.packages <- c("R.matlab",  # Handling *.mat files
                      "eegkit",    # Visualising EEG
                      "TSA",       # Time series tools
                      "moments",   # Statistical moments
                      "caret")     # Data partitioning

# Create list of required new packages
new.packages <- list.of.packages[!(list.of.packages %in% 
                                   installed.packages()[,"Package"])]
# If non-empty list is returned, install packages
if(length(new.packages)) install.packages(new.packages)

# Load packages
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})

################################ REQUIRED FUNCTIONS ##################################

# Function to read in EEG file
read.EEG.file <- function (filename) {
  # Reads in a single EEG .mat file 
  #
  # Args:
  #   filename: The name of the input EEG data file in .mat format
  # 
  # Returns:
  #   EEG.file - a list of lists, with five elements. Element 1 is the electrode 
  #   data and the others contain metadata (length; sample rate; electrode labels
  #   and sample sequence)
  
  # Initiliase list for EEG data structure
  EEG.file <- list()
  # Read in the .mat file
  a <- readMat(filename)
  # Data matrix - element 1 
  # 16 rows by ~24,000 columns = 16 by (200 x 600) or
  # 16 electrode by (sample rate X length seconds) 
  EEG.file[["mat"]] <- a[[1]][[1]]
  # EEG length - element 2
  EEG.file[["seconds"]] <- as.numeric(a[[1]][[2]])
  # Sample rate - element 3
  EEG.file[["frequency"]] <- as.numeric(a[[1]][[3]])
  # Number of EEG channels
  EEG.file[["channel"]] <- as.numeric(nrow(a[[1]][[1]]))
  # EEG electrode labels 
  EEG.file[["labels"]] <- unlist(a[[1]][[4]])
  # EEG sequence number if available or set to -1
  if (length(a[[1]]) > 4) {
    EEG.file[["sequence"]] <- as.numeric(a[[1]][[5]])
  } else {
    EEG.file[["sequence"]] <- -1
  }
  # Remove object
  rm(a)
  # Garbage collection to reallocate memory
  gc()
  # Return
  EEG.file
}

############################### SET PROJECT OPTIONS ##################################

# Folder structures with patient EEG data files
folder.list = c('Dog_1','Dog_2','Dog_3','Dog_4','Dog_5','Patient_1','Patient_2')
# Endode data file categories into a numerical annotation
patient.num <- c('Dog_1' = 1, 'Dog_2' = 2, 'Dog_3' = 3,
                 'Dog_4' = 4, 'Dog_5' = 5, 'Patient_1' = 6,
                 'Patient_2' = 7)

# Set this to choose which set of data to work on
# Sample data, set = 1; full data, set = 0
sample.data <- 1

# Code is developed on several machines and location of data files may vary
# Get working directory for code and data samples
parent.dir <- getwd()

# For labels for feature vectors
feature.folder <- "Set_1"
# Set labels for experimental run
results.folder <- "Run_1"

if (sample.data == 1){
  # Set subdirectory for feature vector results
  features.dir <- paste0(parent.dir, '/Features/', feature.folder)
  # Create folder
  dir.create(path = features.dir, showWarnings = TRUE)
  # Set working directory for sample dataset
  data.dir <- paste0(parent.dir, '/Sample Data/')
} else  { 
  # Set subdirectory for feature vector results
  features.dir <- paste0('H:', '/Features/')  # Change drive letter as needed
  # Set working directory for full dataset (Drive letter may vary across machines)
  data.dir <- paste0('H:', '/Data/')  # Change drive letter as needed
}

# Set this to choose overlapping or non-overlapping windows
# Warning!! Almost doubles processing time: time*(2n-1)
# Overlapping window, set = 1; non-overlapping, set = 0
overlap <- 1

#Set window size for file splitting (seconds), preferably factor of 600.
windowsize <- 60

########################### PREPROCESSING - LABELLING FILES ##########################

# Label the test segments with the correct labels 
# Required initially and when rebuilding the dataset

# source ("RenameData.R")

######################## PREPROCESSING - INTEGRITY CHECK FILES #######################

# Check each data file by reading the data into a matrix
# Recommended after any data file migrations

# source ("IntegrityCheck.R")

############################ PREPROCESSING - GET METADATA ############################

# Read in the data files and create a summary table of metadata
# Required initially and when rebuilding the dataset

# source ("GetMetadata.R")

# Results are saved to metadata.rda

################################ FEATURE ENGINEERING #################################

# Construct EEG features and output a feature vector for the classifiers
# Will be run each time features are generated or optimised

 source ("MakeFeature.R")

# Results are saved to a 'Features' folder, one matrix per patient

################################ DATA PARTITIONING ###################################

# Get all feature vectors and combine (generalised model)
# Set path to feature set
feature.folder <- "Set_1"
feature.folder.path <- paste0(parent.dir, '/Features/', feature.folder)

# Get list of files
list.of.feature.files <- dir(feature.folder.path, "*.rda")

# Initialise object
combined.feature.vector <- c()

# Loop for all files containing feature vectors 
for (feature.file in list.of.feature.files){
  load(feature.file)  # Load file
  
  if(is.null(combined.feature.vector)) {
    combined.feature.vector <- feature.vector.matrix  # First file do not rowbind
  } else { combined.feature.vector <- rbind(combined.feature.vector,
                                            feature.vector.matrix)
  }
}










