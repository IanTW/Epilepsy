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

###########################

# Calculate correlation matrix
# Ignore first two columns containing filename and class labels
correlationMatrix <- cor(combined.feature[,3:ncol(combined.feature)])
# Indices of features that are highly corrected
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)

# Save reduced feature set indexes
#save(highlyCorrelated, file = "Corr_stat.rda")
#save(highlyCorrelated, file = "Corr_FFT.rda")
#save(highlyCorrelated, file = "Corr_stat_plus_FFT.rda")

###########################

# Calculate Recursive Feature Elimenation (RFE)
# Number of columns in feature vector
control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

results <- rfe(combined.feature[,3:ncol(combined.feature)], # Features
               combined.feature[,2], # Class labels
               sizes=c(1:224), # Set to required number of features
               rfeControl=control)

# Save RFE results
#save(results, file = "RFE_stat.rda")
#save(results, file = "RFE_FFT.rda")
#save(results, file = "RFE_stat_plus_FFT.rda")

# Plot results
#print(results)
#print(results$optVariables)
#plot(results, type=c("g", "o"))

# Filter
combined.feature <- combined.feature[,results$optVariables]


