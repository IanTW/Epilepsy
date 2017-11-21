######################################################################################
# File Name: SelectFeature.R                                                         #
# Purpose: Feature reduction                                                         #
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

# Load feature data
setwd(features.dir)
#load("Combined_stat_features.rda")
#load("Combined_FFT_features.rda")
#load("Combined_stat_plus_FFT_features.rda")

# Calculate correlation matrix
# Ignore first two columns containing filename and class labels
correlationMatrix <- cor(combined.feature[,3:ncol(combined.feature)])
# Indices of features that are highly corrected
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)

# Save reduced feature set indexes
#save(highlyCorrelated, file = "Corr_reduced_stat_index.rda")
#save(highlyCorrelated, file = "Corr_reduced_FFT_index.rda")
#save(highlyCorrelated, file = "Corr_reduced_stat_plus_FFT_index.rda")

