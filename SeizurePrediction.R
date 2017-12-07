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
######################################################################################

############################## SET UP AND LOAD PACKAGES ##############################

# Install and load required R packages 
# List of required packages
list.of.packages <- c("R.matlab",    # Handling *.mat files
                      "eegkit",      # Visualising EEG
                      "TSA",         # Time series tools
                      "moments",     # Statistical moments
                      "caret",       # Data partitioning
                      "e1071",       # SVM modeling
                      "data.table",  # Summarise data
                      "ROCR",        # Performance evaluation
                      "DMwR",        # SMOTE
                      "nnet")        # Neural network modeling

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
  #   data and the others contain metadata (length; sample rate; 
  #   number of channels; electrode labels

  # Initiliase list for EEG data structure
  EEG.file <- list()
  # Read in the .mat file
  a <- readMat(filename)
  # Data matrix - element 1 
  # E.g. 16 rows by ~24,000 columns = 16 by (200 x 600) or
  # 16 electrodes by (sample rate in Hz X length in seconds) 
  EEG.file[["mat"]] <- a[[1]][[1]]
  # EEG length - element 2
  EEG.file[["seconds"]] <- as.numeric(a[[1]][[2]])
  # Sample rate - element 3
  EEG.file[["frequency"]] <- as.numeric(a[[1]][[3]])
  # Number of EEG channels
  EEG.file[["channel"]] <- as.numeric(nrow(a[[1]][[1]]))
  # EEG electrode labels - element 4
  EEG.file[["labels"]] <- unlist(a[[1]][[4]])
  # Remove object
  rm(a)
  # Garbage collection to reallocate memory
  gc()
  # Return
  EEG.file
}

############################### SET PROJECT OPTIONS ##################################

# Folder structures with patient EEG data files
folder.list = c('Dog_1', 'Dog_2', 'Dog_3', 'Dog_4', 'Dog_5', 'Patient_1', 'Patient_2')
# Encode data file categories into a numerical annotation
#patient.num <- c('Dog_1' = 1, 'Dog_2' = 2, 'Dog_3' = 3,
#                 'Dog_4' = 4, 'Dog_5' = 5, 'Patient_1' = 6,
#                 'Patient_2' = 7)

# Code is developed on several machines and location of data files may vary
# Get working directory for code and data samples
parent.dir <- getwd()

# Set drive letter for portable data store
portable <- "I:/"

# Location for feature vectors if generating features
# ARE YOU OVERWRITING ANY EXISTING FEATURES?
feature.folder <- "Set_4"

# Location for test results
# ARE YOU OVERWRITING ANY EXISTING RESULTS?
#results.folder <- paste0(portable, 'Results/Normal')
#or
results.folder <- paste0(portable, 'Results/Correlation')

# Set directories for the data for modelling
#partition.folder <- paste0(portable, 'Partitions/Normal')
#or
partition.folder <- paste0(portable, 'Partitions/Correlation')

# Set subdirectory for feature vector results
features.dir <- paste0(portable, 'Features/',feature.folder)  # Change drive letter as needed
# Create folder
#dir.create(path = features.dir, showWarnings = TRUE)
# Set working directory for full dataset
data.dir <- paste0(portable, 'Data/')  

# Set this to choose overlapping or non-overlapping windows
# Hard coded to 50% overlap between windows
# Warning!! For n windows almost doubles processing time: time*(2n-1)
# Overlapping window, set = 1; non-overlapping, set = 0
overlap <- 1

# Set window size for file segmentation (seconds), preferably factor of 600.
windowsize <- 30

# For 50% overlapping windows
# Number of slices = ((time/windowsize) * 2) - 1
slice.num <- (600/windowsize)*2-1

# Set training/testing split
split <- 0.70

# Skip files that do not have exactly 16 channels
# Skip, set = 1; do not skip, set = 0
# Warning!! There are many files that will be removed if skip=1
skip.files <- 0

# Make statistical features
# Make, set = 1; do not make, set = 0
make.stat <- 1

# Make spectral density features
# Make, set = 1; do not make, set = 0
make.fft <- 0

########################### PREPROCESSING - LABELLING FILES ##########################

# Label the test segments with the correct labels 
# Required initially and if rebuilding the dataset

#source ("RenameData.R")

######################## PREPROCESSING - INTEGRITY CHECK FILES #######################

# Check each data file by reading the data into a matrix
# Recommended after any data file migrations

#source ("IntegrityCheck.R")

############################ PREPROCESSING - GET METADATA ############################

# Read in the data files and create a summary table of metadata

#source ("GetMetadata.R")

# Results are saved to metadata.rda

################################ FEATURE ENGINEERING #################################

# Construct EEG features and output a feature vector for the classifiers

#source ("MakeFeature.R")

# Results are saved to 'Features' folder

################################ DATA PARTITIONING ###################################

# Merge feature vectors for patients and feature types
# Create test/train partitions for modelling

#source ("PartitionData.R")

# Results are saved to 'Partitions' folder

################################## DATA MODELING #####################################

# Create models with training data and run predictions on test data

#source ("ModelData.R")

# Models and predictions are saved to 'Results' folder
# There are subfolders for 'Model' and 'Predict'

################################## SUMMARISE RESULTS #################################

# Evaluate results (AUC, specificity...) and summarise

#source ("EvaluateResults.R")

# Summary table is saved to the 'Results' folder

################################## FEATURE REDUCTION #################################

# Examine features for feature selection/reduction

#source ("SelectFeature.R")

# The results are saved to the 'Results' folder

#################################### END CODE ########################################


