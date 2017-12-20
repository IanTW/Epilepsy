######################################################################################
# File Name: SelectFeature.R                                                         #
# Purpose: Feature reduction                                                         #
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

# Load feature data
setwd(features.dir)
#load("Combined_stat_features.rda")
#load("Combined_FFT_features.rda")
#load("Combined_stat_plus_FFT_features.rda")

###########################
# Calculate correlation matrix

# Ignore first two columns containing filename and class labels
correlationMatrix <- cor(combined.feature[,3:ncol(combined.feature)])
# Indices of features that are highly corrected
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)

# Save reduced feature set indexes
#save(highlyCorrelated, file = "Corr_stat.rda")
#save(highlyCorrelated, file = "Corr_FFT.rda")
#save(highlyCorrelated, file = "Corr_stat_plus_FFT.rda")

###########################
# Subset
temp <- combined.feature[combined.feature$CLASS == "Preictal",]
temp1 <- combined.feature[combined.feature$CLASS == "Interictal",]
temp1 <- temp1[sample(nrow(temp1),30000),]
combined.feature <-rbind(temp, temp1)
table(combined.feature$CLASS)

###########################
# RFE Method
library(caret)

# Calculate Recursive Feature Elimenation (RFE)
# Number of columns in feature vector
control <- rfeControl(functions = rfFuncs, method = "cv", number = 2, verbose = TRUE)

results <- rfe(combined.feature[,3:ncol(combined.feature)], # Features
               combined.feature[,2], # Class labels
               sizes=c(1:224), # Set to required number of features - /80/224/304
               rfeControl=control)

# Save RFE results
#save(results, file = "RFE_Stat_40k.rda")
save(results, file = "RFE_FFT_40k.rda")
#save(results, file = "RFE_Both_40k.rda")

###########################
#LVQ method
library(caret)

control <- trainControl(method="cv", number = 2, verboseIter = TRUE)

#grid <- expand.grid(size=c(33,66), k=c(67,99, 121))

# Train the model
model <- train(combined.feature[,3:ncol(combined.feature)], # Features
               combined.feature[,2], # Class labels
               method = "lvq", 
               trControl=control)

# Train the model with grid tune
#model <- train(combined.feature[,3:ncol(combined.feature)], # Features
#               combined.feature[,2], # Class labels
#               method = "lvq", 
#               trControl=control,
#               tuneGrid = grid)


# Save LVQ results
#save(model, file = "LVQ_Stat_40k.rda")
save(importance, file = "LVQ_FFT_40k.rda")
#save(importance, file = "LVQ_Both.rda")

#################################################################
# Graphics/Options for RFE

# Plot results
print(results)
print(results$optVariables)
plot(results, main = "Title Here",type=c("g", "o"))

# Filter 
combined.feature <- combined.feature[,results$optVariables]

#################################################################
# Graphics/Options for LVQ

# estimate variable importance
importance <- varImp(model, scale=FALSE)
plot(varImp(model, scale = FALSE), top = 40)
# summarize importance
print(importance)
# plot model (show k size and codebook size)
plot(model)

# Top variables
LVQ <- data.frame(varImp(model)[1])
LVQ <- LVQ[order(-LVQ$importance.Interictal),]
LVQ <- LVQ[c(1:20),] # Top 20
# Get var names
LVQ <- rownames(LVQ)
#################################################################
