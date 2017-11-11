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

# Visualisation
# install.packages("devtools")
# library(devtools)
# source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
# plot.nnet(outdata)

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

################################### PROCESS RESULTS ##################################

# Set directory
predict.folder <- paste0(results.folder, "/Predict/")
list.of.files <- dir(predict.folder)
setwd(predict.folder)

# Make results object
summary.results <- data.frame(matrix(ncol = 6, nrow = 0))

for (filename in list.of.files){
  
  # Console output
  cat("Processing ", filename, "\n")
  
  # Load prediction
  load(filename)
  
  if(grepl("SVM", filename)){
    
    # Get prefix from prediction file name
    pre <- substring(filename,13,13)
    # Set directory to test files
    test.dir <- paste0(partition.folder, "/Test/")
    # Get test file that matches training file
    test.file <- list.files(test.dir, pattern = paste0(pre,"_"))
    # Load test file
    load(paste0(test.dir,test.file))
    
    # Get probabilities from prediction
    probs <- attr(svmPredict, "probabilities")
    # Bind with test data to get original file identities
    test.partition.prob <- cbind(probs, test.partition)
    
    # Remove slice number from ID
    test.partition.prob$ID <- paste0(read.table(text = test.partition.prob$ID,
                                                sep = ".",
                                                as.is = TRUE)$V1,
                                     ".mat")
    
    # Convert to data.table
    test.partition.prob <- as.data.table(test.partition.prob)
    
    # Summarise to get total probability across all slices for each data file
    results <- test.partition.prob[,.(PreictalProb = sum(Preictal)/slice.num,
                                      InterictalProb = sum(Interictal)/slice.num),
                                   by =.(ID)]
    
    # Create predicted label based on highest probability
    results$Prediction <- factor(ifelse(results$PreictalProb > results$InterictalProb,
                                        "Preictal", "Interictal"))
    
    # Create truth label based on the filename
    results$Truth <- factor(ifelse(grepl("inter", results$ID), "Interictal", "Preictal"))
    
  } else{
    
    # Get prefix from prediction file name
    pre <- substring(filename,16,16)
    # Set directory to test files
    test.dir <- paste0(partition.folder, "/Test/")
    # Get test file that matches training file
    test.file <- list.files(test.dir, pattern = paste0(pre,"_"))
    # Load test file
    load(paste0(test.dir,test.file))
    
    # Get probabilities from prediction
    probs <- as.data.frame(neuralPredict)
    # Bind with test data to get original file identities
    test.partition.prob <- cbind(probs, test.partition)
    
    # Remove slice number from ID
    test.partition.prob$ID <- paste0(read.table(text = test.partition.prob$ID,
                                                sep = ".",
                                                as.is = TRUE)$V1, ".mat")
    
    # Convert to data.table
    test.partition.prob <- as.data.table(test.partition.prob)
    
    # Summarise to get total probability across all slices for each data file
    results <- test.partition.prob[,.(PreictalProb = sum(V1/slice.num)),
                                   by =.(ID)]
    
    # Create predicted label based on highest probability
    results$Prediction <- factor(ifelse(results$PreictalProb > 0.5,
                                        "Preictal", "Interictal"))
    
    # Create truth label based on the filename
    results$Truth <- factor(ifelse(grepl("inter", results$ID), "Interictal", "Preictal"))

  }

  # Get confusion matrix
  confusion <- table(results$Prediction, results$Truth)
  
  # Initialise object
  df <- c()
  # Get probabilities
  df$predictions <- results$PreictalProb
  # Get class labels
  df$label <- ifelse(results$Truth == "Preictal", 1, 0)
  # Create prediction object
  pred <- prediction(df$predictions, df$label)
  # Get performance measures
  # AUC
  perf <- performance(pred,"auc")
  auc.score <- round(unlist(attr(perf, "y.values")),2)

  # Compile results
  results.list <- c(filename, confusion[1], confusion[2], confusion[3], confusion[4], auc.score)
  
  # Bind to output 
  summary.results <- rbind(summary.results, results.list, stringsAsFactors = FALSE)
  
  #rm(results.list)
}

# Create column names
x <- c("Filename", "TNR", "FPR", "FNR", "TPR", "AUC")
colnames(summary.results) <- x
# Set directory for output
setwd(results.folder)
save(summary.results, file = "Summary_Results.rda")
summary.results
