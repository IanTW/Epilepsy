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

library(data.table)
library(ROCR)
# Set directory
results.folder <- "E:/Results"
partition.folder <- "E:/Partitions"
predict.folder <- paste0(results.folder, "/Predict/Neural")
list.of.files <- dir(predict.folder)
setwd(predict.folder)

# Make results object
summary.results <- data.frame(matrix(ncol = 8, nrow = 0))

for (filename in list.of.files){
  
  # Console output
  cat("Processing prediction", filename, "\n")
  
  # Load prediction
  load(filename)
  
  # For SVM predictions
  if(grepl("SVM", filename)){
    
    # Get all digits from string
    matches <- regmatches(filename, gregexpr("[[:digit:]]+", filename))
    # Unlist the matches
    matches <- as.numeric(unlist(matches))
    # The first match is the file number
    pre <- matches[1]
    if (pre <= 26){
        slice.num = 19
    } else if (pre > 26 && pre <= 53){
        slice.num = 10
    } else if (pre > 53 && pre <= 80){
        slice.num = 39
    } else if (pre > 80){
        slice.num = 20
    }
    
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
  
  
  # For Neural Net predictions    
  } else{
    
    # Get all digits from string
    matches <- regmatches(filename, gregexpr("[[:digit:]]+", filename))
    # Unlist the matches
    matches <- as.numeric(unlist(matches))
    # The first match is the file number
    pre <- matches[1]
    
    if (pre < 10){
      pre = paste("0", pre, sep = "")}
    
    if (pre <= 26){
      slice.num = 19
    } else if (pre > 26 && pre <= 53){
      slice.num = 10
    } else if (pre > 53 && pre <= 80){
      slice.num = 39
    } else if (pre > 80){
      slice.num = 20
    }
      
    cat("There are", slice.num, "slices", "\n")
    
    # Set directory to test files
    test.dir <- paste0(partition.folder, "/Test/")
    # Get test file that matches training file
    test.file <- list.files(test.dir, pattern = paste0("^", pre, "_"))
    # Load test file
    cat("Loading test file", test.file, "\n")
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

######################################################
# For summary plots

file.names <- read.table(text = summary.results$Filename, sep = "_", colClasses = "character")
summary.results <- cbind(file.names, summary.results)
summary.results$V1 <- NULL
summary.results$V2 <- NULL
summary.results$V8 <- NULL
summary.results$V9 <- NULL
summary.results$Filename <- NULL
colnames(summary.results) <- c("Number",
                               "Window",
                               "Feature",
                               "Selection",
                               "Sampling",
                               "TN",
                               "FP",
                               "FN",
                               "TP",
                               "Specificity",
                               "Sensitivity",
                               "AUC")

library(plotly)
library(ggplot2)

summary.results$Window <- as.factor(summary.results$Window)
summary.results$Feature <- as.factor(summary.results$Feature)
summary.results$Selection <- as.factor(summary.results$Selection)
summary.results$Sampling <- as.factor(summary.results$Sampling)
summary.results$Specificity <- as.numeric(summary.results$Specificity)
summary.results$Sensitivity <- as.numeric(summary.results$Sensitivity)

#Ordering
summary.results$Feature <- factor(summary.results$Feature, levels = c("Stat", "FFT", "Both"))


p <- ggplot(summary.results, aes(x=Window, y=Specificity, fill=Window)) + geom_boxplot()

# Plot of specificity vs features and selection
ggplot(summary.results, aes(x=Feature, y=Specificity, fill=Selection)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())

# Plot of sensitvity vs features and selection
ggplot(summary.results, aes(x=Feature, y=Sensitivity, fill=Selection)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())





# Plot of sensitivity vs windows and Feature
ggplot(summary.results, aes(x=Window, y=Sensitivity, fill = Feature)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())


###############KEEPERS#####################
#Ordering
summary.results$Window <- factor(summary.results$Window, levels = c("30-00", "60-50", "30-50","60-00"))
# Plot of sensitivity vs windows
ggplot(summary.results, aes(x=Window, y=Sensitivity, fill = Window)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21)
###############KEEPERS#####################
# Plot of specificity vs features
ggplot(summary.results, aes(x=Feature, y=Specificity, fill=Feature)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())
###############KEEPERS#####################
#Ordering
summary.results$Selection<- factor(summary.results$Selection, levels = c("Non", "Rfe", "Lvq"))
# Plot of specificity vs selection
ggplot(summary.results, aes(x=Selection, y=Specificity, fill=Selection)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())









######################################################
# For AUC plots

# Graphics
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

#####################################################