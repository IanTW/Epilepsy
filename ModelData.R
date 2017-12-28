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

neural.model <- function (filename){
  
  setwd(train.folder)
  load(filename)
  cat("Training with Neural and", filename, "\n")
  
  # Number columns
  N <- ncol(train.partition)
  
  ########################################################
  # Training with nnet package
  # neuralModel <- nnet(CLASS ~ .,
  #                                data = train.partition,
  #                                size = 10,
  #                                rang = 0.5,
  #                                decay = 0.01,
  #                                maxit = 3000,
  #                                MaxNWts = 10000)
  #
  #######################################################
  # Training with caret and nnet
  fitControl <- trainControl(## k-fold CV
                             method = "repeatedcv",
                             number = 2, ## k fold
                             repeats = 1) ## repeated x times
  
  # Set grid for tuning parameters
  nnetGrid <- expand.grid(.size=c(1,10,20,50),.decay=seq(0,1,0.1))
  # Maximum number of neurons
  maxSize <- max(nnetGrid$.size)
  # Number of weights
  numWts <- maxSize * N + maxSize + 1
  # Training with grid tune
  neuralModel <- train(train.partition[,c(2:N)],
                       train.partition$CLASS,
                       method = "nnet",
                       trControl = fitControl,
                       tuneGrid = nnetGrid,
                       maxit = 100,
                       MaxNWts = numWts,
                       verboseIter = TRUE)
  
  # Training with random hyperparameter search
  fitControl <- trainControl(## k-fold CV
    method = "repeatedcv",
    number = 2, ## k fold
    repeats = 1, ## repeated x times
    search = "random")
  
  neuralModel <- train(train.partition[,c(2:N)],
                       train.partition$CLASS,
                       method = "nnet",
                       metric = "ROC",
                       trControl = fitControl,
                       tuneLength = 30,
                       verboseIter = TRUE)
  
  
  
  
  
  
  
  
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
  
  cat("Testing with Neural and", filename, "\n")
  
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
  setwd(paste0(results.folder, "/Predict/"))
  save(neuralPredict, file = label)
}

svm.model <- function (filename){
  
  setwd(train.folder)
  load(filename)
  cat("Training with SVM and", filename, "\n")
  
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
  pre <- substring(filename,1,2)
  

  
  # Get test file that matches training file
  test.file <- list.files(test.dir, pattern = paste0(pre,"_"))

  # Set directory to test files
  test.dir <- paste0(partition.folder, "/Test")  
  setwd(test.dir)
  # Load test file
  load(test.file)
  
  cat("Testing with SVM and", filename, "\n")
  
  # Number columns
  N <- ncol(test.partition)
  
  # Predicting
  system.time(svmPredict <- predict(svmModel,  # Trained model
                                    test.partition[,c(3:N)],  # Choose columns for features
                                    probability = TRUE))  # Calculate probabilities    
  
  # Labels for saving results
  label <- paste0("SVM_Predict_", filename)
  setwd(paste0(results.folder, "/Predict"))
  save(svmPredict, file = label)
  
}

################################## RUN MODELING ######################################

# Set directory
train.folder <- paste0(partition.folder, "/Train/")
list.of.files <- dir(train.folder)

for (filename in list.of.files){
  neural.model(filename)
  svm.model(filename)
}

######################################################################################
