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

################################## LOAD PARTITION ####################################

setwd(partition.folder)
train.filename <- "Stat_plus_FFT_Boosted_Minority_Train_Partition_70_30.rda" 
test.filename <- "Stat_plus_FFT_Boosted_Minority_Test_Partition_70_30.rda"
load(train.filename)  # Load training data
load(test.filename) # Load test data

# Labels for saving results
label <- "Stat_plus_FFT_Boosted_Minority_70_30.rda"

# [1] "FFT_Boosted_Minority_Test_Partition_70_30.rda"              
# [2] "FFT_Boosted_Minority_Train_Partition_70_30.rda"             
# [3] "FFT_Downsample_Majority_Test_Partition_70_30.rda"           
# [4] "FFT_Downsample_Majority_Train_Partition_70_30.rda"          
# [5] "FFT_Normal_Test_Partition_70_30.rda"                        
# [6] "FFT_Normal_Train_Partition_70_30.rda"                       
# [7] "Stat_Boosted_Minority_Test_Partition_70_30.rda"             
# [8] "Stat_Boosted_Minority_Train_Partition_70_30.rda"            
# [9] "Stat_Downsample_Majority_Test_Partition_70_30.rda"          
# [10] "Stat_Downsample_Majority_Train_Partition_70_30.rda"         
# [11] "Stat_Normal_Test_Partition_70_30.rda"                       
# [12] "Stat_Normal_Train_Partition_70_30.rda"                      
# [13] "Stat_plus_FFT_Boosted_Minority_Test_Partition_70_30.rda"    
# [14] "Stat_plus_FFT_Boosted_Minority_Train_Partition_70_30.rda"   
# [15] "Stat_plus_FFT_Downsample_Majority_Test_Partition_70_30.rda" 
# [16] "Stat_plus_FFT_Downsample_Majority_Train_Partition_70_30.rda"
# [17] "Stat_plus_FFT_Normal_Test_Partition_70_30.rda"              
# [18] "Stat_plus_FFT_Normal_Train_Partition_70_30.rda"   

##################################### SVM MODELING ###################################

# Number columns
N <- ncol(train.partition)

# SVM modelling
# Training with e1071 package
system.time(svmModel <- svm(train.partition[,c(2:N)],  # Choose columns for features
                train.partition$CLASS,  # Class labels
                probability = TRUE))  # Calculate probabilities

# Number columns
N <- ncol(test.partition)

# Predicting
system.time(svmPredict <- predict(svmModel,  # Trained model
                      test.partition[,c(3:N)],  # Choose columns for features
                      probability = TRUE))  # Calculate probabilities

# Save model and prediction results
setwd(results.folder)
save(svmModel, file = paste0("SVM_model_", label))
save(svmPredict, file = paste0("SVM_predict_", label))

#################################### GET RESULTS ####################################

# Load training data
setwd(partition.folder)
load("Normal_Train_Partition_70_30.rda")
# Load test data
load("Normal_Test_Partition_70_30.rda")

# Load models
setwd(results.folder)
load("SVM_model_normal_70_30_fft.rda")
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

# Show result
confusion

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

################################### NEURAL MODELING ###################################

# Drop ID
# Not in fft boosted minority - is this the case for all????
train.partition$ID <- NULL

N <- ncol(train.partition)

# Neural Network modelling
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
nnetGrid <- expand.grid(.size=c(1,2),.decay=c(0,0.1))

# Maximum number of neurons
maxSize <- max(nnetGrid$.size)

# Number of weights - WRONG I THINK
numWts <- 1*(maxSize * (N-1) + 1)

# Training
system.time(neuralModel <- train(train.partition[,c(2:N)],
                                 train.partition$CLASS,
                                 method = "nnet",
                                 tuneGrid = nnetGrid,
                                 maxit = 1000,
                                 MaxNWts = numWts))

# summary(neuralModel)

# Number columns
N <- ncol(test.partition)

# Predicting
#system.time(neuralPredict <- predict(neuralModel,  # Trained model
#                                      test.partition[,c(3:N)],  # Choose columns for features
#                                      type = "class"))  # Calculate probabilities    
#
# or

system.time(neuralPredict <- predict(neuralModel,  # Trained model
                                      test.partition[,c(3:N)],  # Choose columns for features
                                      type = "raw"))  # Calculate probabilities        

# Save model and prediction results
setwd(results.folder)
save(neuralModel, file = paste0("Neural_model_", label))
save(neuralPredict, file = paste0("Neural_predict_", label))

#################################### GET RESULTS ####################################

# Load training data
setwd(partition.folder)
load("Normal_Train_Partition_70_30.rda")
# Load test data
load("Normal_Test_Partition_70_30.rda")

# Load models
setwd(results.folder)
load("SVM_model_normal_70_30_fft.rda")
load("SVM_predict_normal_70_30_stat_plus_fft.rda")

# Get probabilities from prediction
probs <- as.data.frame(neuralPredict)
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
results <- test.partition.prob[,.(PreictalProb = sum(V1/slice.num)),
                               by =.(ID)]

# Create predicted label based on highest probability
results$Prediction <- factor(ifelse(results$PreictalProb > 0.5,
                                    "Preictal", "Interictal"))

# Create truth label based on the filename
results$Truth <- factor(ifelse(grepl("inter", results$ID), "Interictal", "Preictal"))

# Get confusion matrix
confusion <- table(results$Prediction, results$Truth)

# Show result
confusion

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