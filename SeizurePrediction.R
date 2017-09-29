##############################################################################################
# Author: Ian Watson                                                                         
# Student Number: D13128934                                                                  
# Email1: d13128934@mydit.ie                                                                 
# Email2: iantwatson@gmail.com                                                               
#                                                                                             
# Institution: Dublin Institute of Technology                                                
# Course Code: DT228B                                                                        
# Course Title: MSc. Data Analytics                                                            
# Date of Dissertation Commencement: September 2017                                          
# Title: A Comparison of SVM and Neural Network Classifiers for Epileptic Seizure Prediction 
#                                                                                            
# R code for implementing a machine learning classification experiment to compare the        
# performance of SVM and neural network classifiers used for epileptic seizure prediction   
#
# Code adapted from Wei Wu & ESAI, Universidad CEU Cardenal Herrera, (F. Zamora-Martínez,
# F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo).
#  
##############################################################################################
# Copyright (c) 2014, Wei Wu
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy 
# of this software and associated documentation files (the "Software"), to deal 
# in the Software without restriction, including without limitation the rights 
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
# copies of the Software, and to permit persons to whom the Software is 
# furnished to do so, subject to the following conditions: 
# 
# The above copyright notice and this permission notice shall be included in all 
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
# IN THE SOFTWARE. 
#
# See: https://github.com/wei-wu-nyc/Kaggle-SeizureDetection-Official
#
###############################################################################################
# Copyright (c) 2014, ESAI, Universidad CEU Cardenal Herrera,
# (F. Zamora-Martínez, F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.
#
# See https://github.com/ESAI-CEU-UCH/kaggle-epilepsy
#                                                                                            
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

################################# SET WORKING DIRECTORY ######################################
# Code is developed on several machines and location of data files varies
# Set working directory to point to data files

# Set working directory
setwd('/home/ianw/Epilepsy')

################################# INTEGRITY CHECK FILES ######################################
# Checks each data file by reading the data into a matrix
# Recommended after any data file migrations

source ("??????????.R")

############################# PREPROCESSING - LABELLING FILES ################################
# Labelling the test segments with the correct labels 
# Required initially and when rebuilding the dataset

source ("RenameTestData.R")

############################## PREPROCESSING - GATHER METADATA ###############################
# Reads in the data files and creates a summary table of metadata
# Required initially and when rebuilding the dataset

source ("GetMetaData.R")

############################### PREPROCESSING OF DATA FILES ##################################

# from discussion on Kaggle....
pat <- readMat('\\Data\Dog_1_interictal_segment_0018.mat')
#pat[[1]][[2]] == 600
#pat[[1]][[3]] == 5000
df <- data.frame(t(pat[[1]][[1]]))
names(df) <- unlist(pat[[1]][[4]])
head(df)
