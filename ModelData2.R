######################################################################################
# File Name: ModelData.R                                                             #
# Purpose: Data modeling with SVM & Neural Networks                                  #
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

##################################### FUNCTIONS ######################################

svm.model <- function (filename){
  
  setwd(train.folder)
  load(filename)
  # Number columns
  N <- ncol(train.partition)
  
  # Training with e1071 package
  system.time(svmModel <- svm(train.partition[,c(2:N)],  # Choose columns for features
                              train.partition$CLASS,  # Class labels
                              probability = TRUE))  # Calculate probabilities
  
  # Labels for saving results
  label <- paste0("SVM_Model_", filename)
  setwd(paste0(results.folder, "/Model/SVM"))
  save(svmModel, file = label)

  # Get prefix from training file name
  pre <- substring(filename,1,1)
  
  # Set directory to test files
  test.dir <- paste0(partition.folder, "/Test")
  
  # Get test file that matches training file
  test.file <- list.files(test.dir, pattern = paste0(pre,"_"))

  # Load test file
  setwd(test.dir)
  load(test.file)
  
  # Number columns
  N <- ncol(test.partition)
  
  # Predicting
  system.time(svmPredict <- predict(svmModel,  # Trained model
                                    test.partition[,c(3:N)],  # Choose columns for features
                                    probability = TRUE))  # Calculate probabilities    
  
  # Labels for saving results
  label <- paste0("SVM_Predict_", filename)
  setwd(paste0(results.folder, "/Predict/SVM"))
  save(svmPredict, file = label)
  
}

neural.model <- function (filename){
  
  setwd(train.folder)
  load(filename)
  
  # Number columns
  N <- ncol(train.partition)
  
  # Training with nnet package
  system.time(neuralModel <- nnet(CLASS ~ .,
                                  data = train.partition,
                                  size = 10,
                                  rang = 0.5,
                                  decay = 0.01,
                                  maxit = 3000,
                                  MaxNWts = 10000))
  
  # Training with caret and nnet
  # Set grid for tuning parameters
  #nnetGrid <- expand.grid(.size=c(1,2),.decay=c(0,0.1))
  # Maximum number of neurons
  #maxSize <- max(nnetGrid$.size)
  # Number of weights - WRONG I THINK
  #numWts <- 1*(maxSize * (N-1) + 1)
  # Training
  #system.time(neuralModel <- train(train.partition[,c(2:N)],
  #                                 train.partition$CLASS,
  #                                 method = "nnet",
  #                                 tuneGrid = nnetGrid,
  #                                 maxit = 1000,
  #                                 MaxNWts = numWts))
  
  # Labels for saving results
  label <- paste0("Neural_Model_", filename)
  setwd(paste0(results.folder, "/Model/Neural"))
  save(neuralModel, file = label)

  # Get prefix from training file name
  pre <- substring(filename,1,1)
  # Set directory to test files
  test.dir <- paste0(partition.folder, "/Test")
  # Get test file that matches training file
  test.file <- list.files(test.dir, pattern = paste0(pre,"_"))
  # Load test file
  setwd(test.dir)
  load(test.file)
  
  # Number columns
  N <- ncol(test.partition)
  
  # system.time(neuralPredict <- predict(neuralModel,  # Trained model
  #                                      test.partition[,c(3:N)],  # Choose features
  #                                      type = "class"))  # Calculate probabilities    
  
  system.time(neuralPredict <- predict(neuralModel,  # Trained model
                                       test.partition[,c(3:N)],  # Choose features
                                       type = "raw"))  # Calculate probabilities        
  
  # Labels for saving results
  label <- paste0("Neural_Predict_", filename)
  setwd(paste0(results.folder, "/Predict/Neural"))
  save(neuralPredict, file = label)
}

################################## RUN MODELING ######################################

# Set directory
train.folder <- paste0(partition.folder, "/Train/")
list.of.files <- dir(train.folder)

for (filename in list.of.files){
  #svm.model(filename)
  neural.model(filename)
}

######################################################################################
