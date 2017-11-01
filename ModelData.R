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

################################### LOAD PARTITION ###################################

# For normal partitioning with x:y split
# Load training data
setwd(partition.folder)
load("Normal_Train_Partition_70_30.rda")
# Load test data
load("Normal_Test_Partition_70_30.rda")

# or

# For downsampled majority partitioning with x:y split
# Load training data
load("Downsample_Majority_Train_Partition_70_30.rda")
# Load test data
load("Downsample_Majority_Test_Partition_70_30.rda")

# or

# For upsampled minority partitioning with x:y split
# etc
# etc

##################################### SVM MODELING ###################################

# Number columns
N <- ncol(train.partition)

# SVM modelling
# Training with e1071 package
system.time(svmModel <- svm(train.partition[,c(3:N)],  # Choose columns for features
                train.partition$CLASS,  # Class labels
                probability = TRUE))  # Calculate probabilities

# Predicting
system.time(svmPredict <- predict(svmModel,  # Trained model
                      test.partition[,c(3:N)],  # Choose culumns for features
                      probability = TRUE))  # Calculate probabilities                                                                                                                     ui95

# Save model and prediction results
setwd(results.folder)
save(svmModel, file = "SVM_model_normal_70_30_stat_plus_fft.rda")
save(svmPredict, file = "SVM_predict_normal_70_30_stat_plus_fft.rda")

#################################### GET RESULTS ####################################

# Load training data
setwd(partition.folder)
load("Normal_Train_Partition_70_30.rda")
# Load test data
load("Normal_Test_Partition_70_30.rda")

# Load models
setwd(results.folder)
load("SVM_model_normal_70_30_stat_plus_fft.rda")
load("SVM_predict_normal_70_30_stat_plus_fft.rda")

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

# Get confusion matrix
confusion <- table(results$Prediction, results$Truth)

# Uses ROCR package
# Initialise object
auc <- c()
# Get probabilities
auc$predictions <- results$PreictalProb
# Get labels
auc$label <- ifelse(results$Truth == "Preictal", 1, 0)
# Create prediction object
pred <- prediction(auc$predictions, auc$label)
# Get performance measure TPR and FPR 
perf <- performance(pred,"tpr","fpr")
# Plot measures
#plot(perf)

