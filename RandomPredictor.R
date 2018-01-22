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

# Random Predictor

# Set directory to test files
test.dir <- paste0(partition.folder, "/Test/")
setwd(test.dir)
list.of.files <- dir(test.dir) 

for (testfiles in list.of.files){
  
  load(testfiles)
  test.partition$ID <- paste0(read.table(text = test.partition$ID,
                                         sep = ".",
                                         as.is = TRUE)$V1,
                                         ".mat")
  # Convert to data.table
  test.partition <- as.data.table(test.partition)
  
  # Get unique files
  test.partition <- unique(test.partition[,c("ID", "CLASS")])
  
  classes <- table(test.partition$CLASS)
  
  N <- as.numeric(classes["Interictal"])
  M <- as.numeric(classes["Preictal"])
  X <- N+M
  
  thresh <- (1/X)*M
  
  rand <- as.data.frame(runif(X))
  colnames(rand) <- c("Prob")
  rand$Prediction <- ifelse(rand$Prob <= thresh, "Preictal", "Interictal")
  
  table(rand$Prediction)
  
  
  }


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
