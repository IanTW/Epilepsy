######################################################################################
# File Name: EvaluateResults.R                                                       #
# Purpose: Summarise results of predictions with evaluation criteria                 #
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

# Set directory
predict.folder <- paste0(results.folder, "/Predict/")
list.of.files <- dir(predict.folder)
setwd(predict.folder)

# Make results object
summary.results <- data.frame(matrix(ncol = 8, nrow = 0))

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
  # Specificity
  spec.score <- round(confusion[1]/(confusion[1]+confusion[2]),2)
  # Sensitivity
  sens.score <- round(confusion[4]/(confusion[3]+confusion[4]),2)
  
  
  # Compile results
  results.list <- c(filename,
                    confusion[1],
                    confusion[2],
                    confusion[3],
                    confusion[4],
                    spec.score,
                    sens.score,
                    auc.score)
  
  # Bind to output 
  summary.results <- rbind(summary.results, results.list, stringsAsFactors = FALSE)
  
  #rm(results.list)
}

# Create column names
x <- c("Filename", "TN", "FP", "FN", "TP", "Specificity", "Sensitivity", "AUC")
colnames(summary.results) <- x
# Set directory for output
setwd(results.folder)
save(summary.results, file = "Summary_Results.rda")
write.csv(summary.results, "Summary_Results.csv", row.names = FALSE)

summary.results
