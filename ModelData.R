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

# For simple partitioning with x:y split
# Load training data
setwd(partition.dir)
load("Simple_Train_Partition_70_30.rda")
# Load test data
load("Simple_Test_Partition_70_30.rda")

# For reduced partitioning with x:y split
# Load training data
setwd(partition.dir)
load("Reduced_Train_Partition_70_30.rda")
# Load test data
load("Reduced_Test_Partition_70_30.rda")

#SVM with E1071
N <- ncol(train.partition)

# Training

system.time(svmModel <- svm(train.partition[,c(3:N)],  # Choose columns for features
                train.partition$CLASS,  # Class labels
                probability = TRUE))  # Calculate probabilities

#Testing
system.time(svmPredict <- predict(svmModel,  # Trained model
                      test.partition[,c(3:N)],  # Choose culumns for features
                      probability = TRUE))  # Calculate probabilities




# # Define 10-fold cross validation
# fitControl <- trainControl(method = "repeatedcv",
#                            number = 10,  # Number of cross-folds
#                            repeats = 10) # Repeats of 10-fold CV
# 
# # SVM with caret
# 
# control <- trainControl(...)
# 
# 
# svmModel <- train(train.partition, 
#                   train.partition$CLASS,
#                   method = "svmRadial",
#                   metric = "ROC")