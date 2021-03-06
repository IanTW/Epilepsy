######################################################################################
# File Name: RandomPredictor.R                                                       #
# Purpose: Random classification of test data                                        #
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

library(ROCR)

# Random Lucky Predictor - !!!Not working as intended!!!
partition.folder <- "E:/Partitions"
test.dir <- paste0(partition.folder, "/Test/")
setwd(test.dir)
# Get list of all test files
list.of.files <- dir(test.dir) 

# Set directory
results.folder <- "E:/Results"
label <- "Binomial_Random"

# Make results object
summary.results <- data.frame(matrix(ncol = 9, nrow = 0))

for (filename in list.of.files){
  
  load(filename)
  cat("Predicting", filename, "\n")
  # Drop the slice number from the ID
  test.partition$ID <- paste0(read.table(text = test.partition$ID,
                                         sep = ".",
                                         as.is = TRUE)$V1,
                                         ".mat")
  # Convert to data.table
  test.partition <- as.data.table(test.partition)
  
  # Get unique file list
  test.partition <- unique(test.partition[,c("ID", "CLASS")])

  # Shuffle list of files 10 times
  count <- 1
  while (count <= 10){
  test.partition <- test.partition[sample(nrow(test.partition)),]
  count = count + 1
  }
  
  # Get class frequencies
  classes <- table(test.partition$CLASS)
  
  # Number of interictal instances
  N <- as.numeric(classes["Interictal"])
  # Number of preictal instances
  M <- as.numeric(classes["Preictal"])
  # Total number of instances
  X <- N+M
  
  # Set threshold for preictal/interictal
  thresh <- (1/X)*M
  
  # Inititalise variable to count number of random preictals
  P <- 0
  
  # Loop while frequency of random does not match frequency of test file
  while (abs(M-P) >= 1){
    # Generate a sequence of random numbers
    # Length is X, the number of instances in the test file
    # Range of random values is from 0 to 1
    rand <- as.data.frame(runif(X))
    # Rename column
    colnames(rand) <- c("Prob")
    # Allocate label based on prediction
    rand$Prediction <- ifelse(rand$Prob <= thresh, "Preictal", "Interictal")
  
    # Check class frequencies of resulting prediction
    classes <- table(rand$Prediction)
    # Number of preictal instances
    P <- as.numeric(classes["Preictal"])
  }  

  # Bind result with test partition
  results <- cbind(test.partition, rand)

  # Add truth value based on file name
  results$Truth <- factor(ifelse(grepl("inter", results$ID), "Interictal", "Preictal"))
  
  # Get confusion matrix
  confusion <- table(results$Prediction, results$Truth)
  
  # Initialise object
  df <- c()
  # Get probabilities
  df$predictions <- results$Prob
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
  # S1 score
  s1.score <- round(2*(sens.score*spec.score)/(sens.score+spec.score),2)
  
  # Compile results
  results.list <- c(filename,
                    confusion[1],
                    confusion[2],
                    confusion[3],
                    confusion[4],
                    spec.score,
                    sens.score,
                    auc.score,
                    s1.score)
  
  # Bind to output 
  summary.results <- rbind(summary.results, results.list, stringsAsFactors = FALSE)
  
}

# Create column names
x <- c("Filename", "TN", "FP", "FN", "TP", "Spec", "Sens", "AUC", "S1")
colnames(summary.results) <- x

# Set directory for output
setwd(results.folder)
file.label.rda = paste0(label, "_", "Summary_Results.rda")
file.label.csv = paste0(label, "_", "Summary_Results.csv")
save(summary.results, file = file.label.rda)
write.csv(summary.results, file = file.label.csv, row.names = FALSE)

# Graphics
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

########################################################################################

library(ROCR)
# Random Binomial Predictor

# Set directory to test files
partition.folder <- "~/Epilepsy/Partitions"
test.dir <- paste0(partition.folder, "/Test/")
setwd(test.dir)
# Get list of all test files
list.of.files <- dir(test.dir) 

# Make results object
summary.results <- data.frame(matrix(ncol = 9, nrow = 0))

for (filename in list.of.files){
  
  load(filename)
  cat("Predicting", filename, "\n")
  # Drop the slice number from the ID
  test.partition$ID <- paste0(read.table(text = test.partition$ID,
                                         sep = ".",
                                         as.is = TRUE)$V1,
                              ".mat")
  # Convert to data.table
  test.partition <- as.data.table(test.partition)
  
  # Get unique file list
  test.partition <- unique(test.partition[,c("ID", "CLASS")])
  
  # Shuffle list of files 10 times
  count <- 1
  while (count <= 10){
    test.partition <- test.partition[sample(nrow(test.partition)),]
    count = count + 1
  }
  
  # Get class frequencies
  classes <- table(test.partition$CLASS)
  
  # Total number of instances
  X <- nrow(test.partition)
  
  # Set threshold for preictal/interictal
  thresh <- 0.5
  
  # Generate a sequence of random numbers
  # Length is X, the number of instances in the test file
  # Range of random values is from 0 to 1
  rand <- as.data.frame(runif(X))
  # Rename column
  colnames(rand) <- c("Prob")
  # Allocate label based on prediction
  rand$Prediction <- ifelse(rand$Prob <= thresh, "Preictal", "Interictal")
  
  # Check class frequencies of resulting prediction
  classes <- table(rand$Prediction)
  
  # Bind result with test partition
  results <- cbind(test.partition, rand)
  
  # Add truth value based on file name
  results$Truth <- factor(ifelse(grepl("inter", results$ID), "Interictal", "Preictal"))
  
  # Get confusion matrix
  confusion <- table(results$Prediction, results$Truth)
  
  # Initialise object
  df <- c()
  # Get probabilities
  df$predictions <- results$Prob
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
  # S1 score
  s1.score <- round(2*(sens.score*spec.score)/(sens.score+spec.score),2)
  
  # Compile results
  results.list <- c(filename,
                    confusion[1],
                    confusion[2],
                    confusion[3],
                    confusion[4],
                    spec.score,
                    sens.score,
                    auc.score,
                    s1.score)
  
  # Bind to output 
  summary.results <- rbind(summary.results, results.list, stringsAsFactors = FALSE)
  
}

# Create column names
x <- c("Filename", "TN", "FP", "FN", "TP", "Spec", "Sens", "AUC", "S1")
colnames(summary.results) <- x

# Set directory for output
setwd(results.folder)
file.label.rda = paste0(label, "_", "Summary_Results.rda")
file.label.csv = paste0(label, "_", "Summary_Results.csv")
save(summary.results, file = file.label.rda)
write.csv(summary.results, file = file.label.csv, row.names = FALSE)

# Graphics
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

################################################

# Set directory for output
setwd(results.folder)
save(summary.results, file = "Summary_Results.rda")
write.csv(summary.results, "Summary_Results.csv", row.names = FALSE)
