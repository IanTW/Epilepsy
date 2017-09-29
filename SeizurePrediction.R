##############################################################################################
# File Name: SeizurePrediction.R                                                             #
# Purpose: Contains the main code sequence for the project                                   #
#                                                                                            #
# Author: Ian Watson                                                                         #
# Email1: d13128934@mydit.ie                                                                 #
# Email2: iantwatson@gmail.com                                                               #
#                                                                                            #
# Institution: Dublin Institute of Technology                                                #
# Course Code: DT228B                                                                        #
# Course Title: MSc. Data Analytics                                                          # 
# Date of Dissertation Commencement: September 2017                                          #
# Title: A Comparison of SVM and Neural Network Classifiers for Epileptic Seizure Prediction #
#                                                                                            #
# R code for implementing a machine learning classification experiment to compare the        #
# performance of SVM and neural network classifiers used for epileptic seizure prediction    #
#                                                                                            #
# Parts of code adapted from Wei Wu & ESAI, Universidad CEU Cardenal Herrera,                #
# (F. Zamora-Martínez,    #F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo).                # 
#                                                                                            #
##############################################################################################
# Copyright (c) 2014, Wei Wu                                                                 #
#                                                                                            #
# Permission is hereby granted, free of charge, to any person obtaining a copy               #
# of this software and associated documentation files (the "Software"), to deal              #
# in the Software without restriction, including without limitation the rights               #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell                  #
# copies of the Software, and to permit persons to whom the Software is                      #
# furnished to do so, subject to the following conditions:                                   #
#                                                                                            #
# The above copyright notice and this permission notice shall be included in all             #
# copies or substantial portions of the Software.                                            #
#                                                                                            #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR                 #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                   #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                     #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,              #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS                     #
# IN THE SOFTWARE.                                                                           #
#                                                                                            # 
# See: https://github.com/wei-wu-nyc/Kaggle-SeizureDetection-Official                        #
#                                                                                            #
##############################################################################################
# Copyright (c) 2014, ESAI, Universidad CEU Cardenal Herrera,                                #
# (F. Zamora-Martínez, F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo)                     #
#                                                                                            #
# Permission is hereby granted, free of charge, to any person obtaining a copy               #
# of this software and associated documentation files (the "Software"), to deal              #
# in the Software without restriction, including without limitation the rights               #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell                  #
# copies of the Software, and to permit persons to whom the Software is                      #
# furnished to do so, subject to the following conditions:                                   #
#                                                                                            #
# The above copyright notice and this permission notice shall be included in all             #
# copies or substantial portions of the Software.                                            #
#                                                                                            #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR                 #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                   #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                     #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,              #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS                     #
# IN THE SOFTWARE.                                                                           #
#                                                                                            # 
# See https://github.com/ESAI-CEU-UCH/kaggle-epilepsy                                        #
#                                                                                            #
##############################################################################################

################################# SET UP AND LOAD PACKAGES ###################################

# Install and load required R packages 
# List of required packages
list.of.packages <- c("R.matlab",
                      "dplyr",
                      "eegkit")

# Create list of required new packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# If non-empty list is returned, install packages
if(length(new.packages)) install.packages(new.packages)

# Load packages
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})

################################# FUNCTION TO READ EEG FILE ##################################

# Read in EEG file
read.EEG.file <- function (filename) {
  # Reads in a single EEG .mat file 
  #
  # Args:
  #   filename: The name of the EEG data file
  # 
  # Returns:
  #  a matrix ??????????????????????????????
  
  # Initiliase list for EEG data structure
  retval=list()
  # Read in the .mat file
  a=readMat(filename)
  # Data matrix - element 1 
  # 16 rows by ~24,000 columns = 16 by (200 x 600) = electrode by (sample rate X length seconds) 
  retval[["mat"]]=a[[1]][[1]]
  # EEG length - element 2
  retval[["seconds"]]=as.numeric(a[[1]][[2]])
  # Sample rate - element 3
  retval[["freq"]]=as.numeric(a[[1]][[3]])
  # EEG electrode labels 
  retval[["labels"]]=unlist(a[[1]][[4]])
  # EEG sequence number if available or set to -1
  if (length(a[[1]]) > 4) {
    retval[["seq"]]=as.numeric(a[[1]][[5]])
  } else {
    retval[["seq"]]=-1
  }
  #Remove object and garbage collection to reallocate memory
  rm(a)
  gc()
  #Return retval
  retval
}

################################# SET WORKING DIRECTORY ######################################

# Code is developed on several machines and location of data file may vary
# Set working directory to point to data files according to which machine is used

# For R install on Work PC
parent.dir <- "C:/Users/ian_wa.IMAGINE/OneDrive/Programming/LearnR/Epilepsy"
# For R install on Work Laptop
parent.dir <- ('/home/ianw/Epilepsy')
# For R install on Home PC
parent.dir <- ('/home/ianw/Epilepsy')
# For R server install on Dr. Watson
parent.dir <- ('/home/ianw/Epilepsy')

# Set working directory
setwd(parent.dir)

################################# SET INITIAL VARIABLES ######################################

# Folder structures with patient EEG data files
patient.name = c('Dog_1','Dog_2','Dog_3','Dog_4','Dog_5','Patient_1','Patient_2')
# Endode data file categories into a numerical annotation
patient.num <- c('Dog_1' = 1, 'Dog_2' = 2, 'Dog_3' = 3,
                 'Dog_4' = 4, 'Dog_5' = 5, 'Patient_1' = 6,
                 'Patient_2' = 7)

############################# PREPROCESSING - LABELLING FILES ################################

# Label the test segments with the correct labels 
# Required initially and when rebuilding the dataset

source ("RenameTestData.R")

########################## PREPROCESSING - INTEGRITY CHECK FILES #############################

# Check each data file by reading the data into a matrix
# Recommended after any data file migrations

source ("ErrorCheckData.R")

############################## PREPROCESSING - GET METADATA ##################################

# Read in the data files and create a summary table of metadata
# Required initially and when rebuilding the dataset

source ("GetMetaData.R")

############################### PREPROCESSING OF DATA FILES ##################################

