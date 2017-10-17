######################################################################################
# File Name: ModelData.R                                                             #
# Purpose: Data modeling with SVM, Neural Networks and baseline GLM                                                            #
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

# Set path to feature vectors
feature.folder.path <- paste0(parent.dir, '/Features/', partition.feature.folder)
setwd(feature.folder.path)

# Load training data
load("TrainPartition.rda")

# Define 10-fold cross validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,  # Number of cross-folds
                           repeats = 10) # Repeats of 10-fold CV

#Model training ()
######################################################################################



