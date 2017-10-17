######################################################################################
# File Name: PartitionFeature.R                                                      #
# Purpose: Partition feature vectors into Test/Train partitions                      #
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
#                                                                                     #
######################################################################################

# This file should be called from the main code file SeizurePrediction.R

# Set path to feature vectors
feature.folder.path <- paste0(parent.dir, '/Features/', partition.feature.folder)

# Get list of files
list.of.feature.files <- dir(feature.folder.path, "*features.rda")
# Set working directory
setwd(feature.folder.path)

# Initialise object
combined.feature.vector <- c()

# Get all feature vectors and combine (generalised model)
# Loop for all files containing feature vectors 
for (feature.file in list.of.feature.files){
  load(feature.file)  # Load file
  
  if(is.null(combined.feature.vector)) {
    combined.feature.vector <- feature.vector.matrix  # First file do not rowbind
  } else { combined.feature.vector <- rbind(combined.feature.vector,
                                            feature.vector.matrix)
  }
}

# Save results to a .rda file
save(combined.feature.vector, file = "Combined.rda")

# Set up partitions
# Set random seed for reproducability
set.seed(893)

# Convert to data frame
combined.feature.vector <- as.data.frame(combined.feature.vector)

# Create random data split
train.index <- createDataPartition (combined.feature.vector$CLASS,
                                        p = split,  # Set training size
                                        list = FALSE)

# Subset for training partition
train.partition <- combined.feature.vector[train.index, ]
# Save to file
save(train.partition, file = "TrainPartition.rda")

# Subset for test partition
test.partition <- combined.feature.vector[-train.index, ]
# Save to file
save(test.partition, file = "TestPartition.rda")

######################################################################################



