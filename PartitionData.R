######################################################################################
# File Name: PartitionData.R                                                        #
# Purpose: Combine feature vectors & create test/train partitions                    #
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

######################################################################################
# For stat and FFT features
# This code is specfic for the folders and number of features
# It cannot be run generically

# Make sure there is no existing copy already !!!
# Combine all feature vectors into one dataframe
setwd(paste0(portable, "Features/60s-00p/Stat"))
# Get list of feature files
listoffiles <- list.files()
# Initialise object
combined.feature <- c()

# Loop for all feature files
for (filename in listoffiles){
  # Load file
  load(filename)
  # Row bind
  combined.feature <- rbind(combined.feature, feature.vector.matrix)
}

combined.feature$CLASS <- as.factor(combined.feature$CLASS)
save(combined.feature, file = "Combined_features.rda")

# Check
# Number of preictal rows
rows_preictal <- nrow(combined.feature[combined.feature$CLASS == "Preictal",])
# Number of interictal rows
rows_interictal <- nrow(combined.feature[combined.feature$CLASS == "Interictal",])
# Columns of data
cols <- ncol(combined.feature)

#######################################################################################
# For merge of stat and FFT
# Combine Statistics and FFT features per patient into a single object
# This code is specfic for the folders and number of features
# It cannot be run generically

for (folder in folder.list){
  
  # Get Statistics features
  setwd(paste0(portable, "Features/60s-50p/Stat"))
  load (paste0(folder,"_features.rda"))
  df1 <- feature.vector.matrix
  # Get FFT features
  setwd(paste0(portable, "Features/60s-50p/FFT"))
  load (paste0(folder,"_features.rda"))
  df2 <- feature.vector.matrix
  
  # Bind
  df3 <- cbind(df1,df2)
  # Drop duplicate header columns with filename and class label
  df <- df3[, c(1:82,85:308)]
  
  #Set output directory - does the directory exist?
  setwd(paste0(portable, "Features/60s-50p/Both"))
  # Save to file
  save(df, file = paste0(folder,"_both_features.rda"))
}

# Make sure there is no existing copy already !!!
# Combine all feature vectors into one dataframe
setwd(paste0(portable, "Features/60s-50p/Both"))
# Get list of feature files
listoffiles <- list.files()
# Initialise object
combined.feature <- c()

# Loop for all feature files
for (filename in listoffiles){
  # Load file
  load(filename)
  # Row bind
  combined.feature <- rbind(combined.feature, df)
}

combined.feature$CLASS <- as.factor(combined.feature$CLASS)
save(combined.feature, file = "Combined_both_features.rda")

# Number of preictal rows
rows_preictal <- nrow(combined.feature[combined.feature$CLASS == "Preictal",])
# Number of interictal rows
rows_interictal <- nrow(combined.feature[combined.feature$CLASS == "Interictal",])
# Columns of data
cols <- ncol(combined.feature)

#######################################################################################

# Set up normal partition based on split ratio WITH ALL DATA FILES

# Load combined feature vector
#setwd(paste0(portable,"/Features/Set_1"))
#load("Combined_stat_features.rda")
#setwd(paste0(portable,"/Features/Set_2"))
#load("Combined_FFT_features.rda")
#setwd(paste0(portable,"/Features/Set_3"))
#load("Combined_stat_plus_FFT_features.rda")

# Function to create normal data partition
normal.partition <- function (filename,type,feat,featsel) {
  # Reads in a feature vector file 
  #
  # Args:
  #   filename: The name of the input feature vector
  #   type: The type of feature (window and overlap size)
  #   feat: The type of feature (Stat, FFT, or both)
  #   featsel: The type of feature selection method (LVQ, RFE)
  # 
  # Returns:
  #   Saves a test and training partition

  # Load metadata file
  setwd(data.dir)
  load("metadata.rda")
  
  # List of files
  preictal.files <- meta.data.results[grep("preictal", meta.data.results$filename),]
  interictal.files <- meta.data.results[grep("interictal", meta.data.results$filename),]
  
  # Calculate number of preictal files in each partition
  n.train.preictal <- round(nrow(preictal.files)*split,0)  # Total files * split
  n.test.preictal <- nrow(preictal.files) - n.train.preictal # The remaining files
  # Calculate number of interictal files in each partition
  n.train.interictal <- round(nrow(interictal.files)*split,0)  # Total files * split
  n.test.interictal <- nrow(interictal.files) - n.train.interictal # The remaining files
  
  # Set random seed for reproducability
  set.seed(893)
  # Create random indices for preictal
  train.preictal.indices <- sample(nrow(preictal.files), n.train.preictal)
  # Get preictal training files
  train.preictal.files <- preictal.files[train.preictal.indices,]
  train.preictal.files <- train.preictal.files$filename # Training file names
  # Get preictal testing files
  test.preictal.files <- preictal.files[-train.preictal.indices,]
  test.preictal.files <- test.preictal.files$filename # Testing file names
  
  # Set random seed for reproducability
  set.seed(313)
  # Create random indices for interictal
  train.interictal.indices <- sample(nrow(interictal.files), n.train.interictal)
  # Get interictal training files
  train.interictal.files <- interictal.files[train.interictal.indices,]
  train.interictal.files <- train.interictal.files$filename # Training file names
  # Get interictal testing files
  test.interictal.files <- interictal.files[-train.interictal.indices,]
  test.interictal.files <- test.interictal.files$filename # Testing file names
  
  # Combined list of training files
  training.files <- c(train.preictal.files, train.interictal.files)
  # Combined list of testing files
  testing.files <- c(test.preictal.files, test.interictal.files)
  
  # Make training data
  train.partition <-c()
  i <- 1  # For console output
  
  for (files in training.files){
    cat(files," ", i , "\n")
    matchstring <- paste0("^",files,"_" )
    train.partition <- rbind(train.partition, combined.feature[grep(matchstring, combined.feature$ID),])
    i = i + 1
  }
  
  # Make testing data
  test.partition <-c()
  i <- 1  # For console output
  
  for (files in testing.files){
    cat(files," ", i , "\n")
    matchstring <- paste0("^",files,"_" )
    test.partition <- rbind(test.partition, combined.feature[grep(matchstring, combined.feature$ID),])
    i = i + 1  
  }
  
  # Drop ID
  train.partition$ID <- NULL
  
  # Set up labels for files
  labty <- type  # Type
  labfe <- feat  # Feature
  labse <- featsel  # Selection method
  labtr <- "Normal_Train"
  labte <- "Normal_Test"
  
  # Save to file
  setwd(paste0(portable, "Partitions"))
  save(train.partition, file = paste0(labty,
                                      labfe,
                                      labse,
                                      labtr,
                                      ".rda",
                                      sep = "_"))
  save(test.partition, file = paste0(labty,
                                     labfe,
                                     labse,
                                     labte,
                                     ".rda",
                                     sep = "_"))
  }

######################################################################################

# Set up partition based on split ratio WITH REDUCED MAJORITY

# Load combined feature vector
#setwd(paste0(portable,"/Features/Set_1"))
#load("Combined_stat_features.rda")
setwd(paste0(portable,"/Features/Set_2"))
load("Combined_FFT_features.rda")
#setwd(paste0(portable,"/Features/Set_3"))
#load("Combined_stat_plus_FFT_features.rda")

# Load metadata file
setwd(data.dir)
load("metadata.rda")

# List of preictal and interictal files
preictal.files <- meta.data.results[grep("preictal", meta.data.results$filename),]
interictal.files <- meta.data.results[grep("interictal", meta.data.results$filename),]

# Number of preictal files
n_preictal <- nrow(preictal.files)

# Set random seed for reproducability
set.seed(379)

# Reduce number of interictal files to match preictal
interictal.files <- interictal.files[sample(nrow(interictal.files), n_preictal),]

# Split files according to desired ratio
n.train.preictal <- round(nrow(preictal.files)*split,0)  # Total files * split
n.test.preictal <- nrow(preictal.files) - n.train.preictal # The remaining files

n.train.interictal <- round(nrow(interictal.files)*split,0)  # Total files * split
n.test.interictal <- nrow(interictal.files) - n.train.interictal # The remaining files

# Set random seed for reproducability
set.seed(893)
# Create random data split for preictal
train.preictal.indices <- sample(nrow(preictal.files), n.train.preictal)
train.preictal.files <- preictal.files[train.preictal.indices,]
train.preictal.files <- train.preictal.files$filename # Training file names

test.preictal.files <- preictal.files[-train.preictal.indices,]
test.preictal.files <- test.preictal.files$filename # Testing file names

# Set random seed for reproducability
set.seed(313)
# Create random data split for interictal
train.interictal.indices <- sample(nrow(interictal.files), n.train.interictal)
train.interictal.files <- interictal.files[train.interictal.indices,]
train.interictal.files <- train.interictal.files$filename # Training file names

test.interictal.files <- interictal.files[-train.interictal.indices,]
test.interictal.files <- test.interictal.files$filename # Testing file names

# List of training files
training.files <- c(train.preictal.files, train.interictal.files)
# List of testing files
testing.files <- c(test.preictal.files, test.interictal.files)

# Training data
train.partition <-c()
i <- 1

for (files in training.files){
  cat(files," ", i , "\n")
  matchstring <- paste0("^",files,"_" )
  train.partition <- rbind(train.partition, combined.feature[grep(matchstring, combined.feature$ID),])
  i = i + 1
}

# Testing data
test.partition <-c()
i <- 1

for (files in testing.files){
  cat(files," ", i , "\n")
  matchstring <- paste0("^",files,"_" )
  test.partition <- rbind(test.partition, combined.feature[grep(matchstring, combined.feature$ID),])
  i = i + 1  
}

# Drop ID
train.partition$ID <- NULL

# Set up labels for files
trainsize <- split*100
testsize <- (1-split)*100
labsp <- paste(trainsize, "_", testsize, sep = "")
labfe <- "FFT_"
labtr <- "Reduced_Train_Partition_"
labte <- "Reduced_Test_Partition_"

# Save to file
setwd(paste0(portable, "Partitions"))
save(train.partition, file = paste0(labfe, labtr, labsp, ".rda"))
save(test.partition, file = paste0(labfe, labte, labsp, ".rda"))

######################################################################################

# Set up partition based on split ratio WITH BOOSTED MINORITY

# Load normal training partition
setwd(partition.folder)

#train.filename <- "Stat_Normal_Train_Partition_70_30.rda"                       
#train.filename <- "FFT_Normal_Train_Partition_70_30.rda"                    
train.filename <-"Stat_plus_FFT_Normal_Train_Partition_70_30.rda"
load(train.filename)

#test.filename <- "Stat_Normal_Test_Partition_70_30.rda"                       
#test.filename <- "FFT_Normal_Test_Partition_70_30.rda"                    
test.filename <-"Stat_plus_FFT_Normal_Test_Partition_70_30.rda"
load(test.filename)

# Drop ID
train.partition$ID <- NULL

# SMOTE
train.partition$CLASS <- as.factor(train.partition$CLASS)
train.partition <- SMOTE(CLASS ~ ., train.partition, perc.over = 1250, perc.under=108)

#Check distribution
table(train.partition$CLASS)

# Set up labels for files
trainsize <- split*100
testsize <- (1-split)*100
labsp <- paste(trainsize, "_", testsize, sep = "")
labfe <- "Stat_plus_FFT_"
labtr <- "Boosted_Minority_Train_Partition_"
labte <- "Boosted_Minority_Test_Partition_"

# Save to file
setwd(paste0(portable, "Partitions"))
save(train.partition, file = paste0(labfe, labtr, labsp, ".rda"))
save(test.partition, file = paste0(labfe, labte, labsp, ".rda"))

######################################################################################

# Create partitions using reduced features

# Portable
portable <- "D:/"

# Stat features
feature.folder <- "Set_1"
features.dir <- paste0(portable, 'Features/',feature.folder)
setwd(features.dir)
load("Corr_reduced_stat_index.rda")

# Adjust indices for number of columns in original data frame
# E.g. index 12 in the test data is index 14 in the original
test.index <- highlyCorrelated + 2
# E.g. index 13 in the train data is index 14 in the original
train.index <- highlyCorrelated + 1

# Test/Stat
a <- "1_Stat_Normal_Test_Partition_70_30.rda"
b <- "2_Stat_Downsample_Majority_Test_Partition_70_30.rda"
c <- "3_Stat_Boosted_Minority_Test_Partition_70_30.rda"
# Load data, filter and save
setwd(paste0(portable,'Partitions/Normal/Test'))
filename <- c
load(filename)
test.partition <- test.partition[,-test.index]
setwd(paste0(portable, "Partitions/Correlation/Test"))
save(test.partition, file = filename)

# Train/Stat
a <- "1_Stat_Normal_Train_Partition_70_30.rda"
b <- "2_Stat_Downsample_Majority_Train_Partition_70_30.rda"
c <- "3_Stat_Boosted_Minority_Train_Partition_70_30.rda"
# Load data, filter and save
setwd(paste0(portable,'Partitions/Normal/Train'))
filename <- a
load(filename)
train.partition <- train.partition[,-train.index]
setwd(paste0(portable, "Partitions/Correlation/Train"))
save(train.partition, file = filename)

################################

# FFT features
feature.folder <- "Set_2"
features.dir <- paste0(portable, 'Features/',feature.folder)
setwd(features.dir)
load("Corr_reduced_fft_index.rda")

# Adjust indices for number of columns in original data frame
# E.g. index 12 in the test data is index 14 in the original
test.index <- highlyCorrelated + 2
# E.g. index 13 in the train data is index 14 in the original
train.index <- highlyCorrelated + 1

# Test/Stat
a <- "4_FFT_Normal_Test_Partition_70_30.rda"
b <- "5_FFT_Downsample_Majority_Test_Partition_70_30.rda"
c <- "6_FFT_Boosted_Minority_Test_Partition_70_30.rda"

# Load data, filter and save
setwd(paste0(portable,'Partitions/Normal/Test'))
filename <- a
load(filename)
test.partition <- test.partition[,-test.index]
setwd(paste0(portable, "Partitions/Correlation/Test"))
save(test.partition, file = filename)

# Train/Stat
a <- "4_FFT_Normal_Train_Partition_70_30.rda"
b <- "5_FFT_Downsample_Majority_Train_Partition_70_30.rda"
c <- "6_FFT_Boosted_Minority_Train_Partition_70_30.rda"

# Load data, filter and save
setwd(paste0(portable,'Partitions/Normal/Train'))
filename <- a
load(filename)
train.partition <- train.partition[,-train.index]
setwd(paste0(portable, "Partitions/Correlation/Train"))
save(train.partition, file = filename)

################################

# Stat Plus FFT features
feature.folder <- "Set_3"
features.dir <- paste0(portable, 'Features/',feature.folder)
setwd(features.dir)
load("Corr_reduced_stat_plus_fft_index.rda")

# Adjust indices for number of columns in original data frame
# E.g. index 12 in the test data is index 14 in the original
test.index <- highlyCorrelated + 2
# E.g. index 13 in the train data is index 14 in the original
train.index <- highlyCorrelated + 1

# Test/Stat
a <- "7_Stat_plus_FFT_Normal_Test_Partition_70_30.rda"
b <- "8_Stat_plus_FFT_Downsample_Majority_Test_Partition_70_30.rda"
c <- "9_Stat_plus_FFT_Boosted_Minority_Test_Partition_70_30.rda"

# Load data, filter and save
setwd(paste0(portable,'Partitions/Normal/Test'))
filename <- a
load(filename)
test.partition <- test.partition[,-test.index]
setwd(paste0(portable, "Partitions/Correlation/Test"))
save(test.partition, file = filename)

# Train/Stat
a <- "7_Stat_plus_FFT_Normal_Train_Partition_70_30.rda"
b <- "8_Stat_plus_FFT_Downsample_Majority_Train_Partition_70_30.rda"
c <- "9_Stat_plus_FFT_Boosted_Minority_Train_Partition_70_30.rda"

# Load data, filter and save
setwd(paste0(portable,'Partitions/Normal/Train'))
filename <- a
load(filename)
train.partition <- train.partition[,-train.index]
setwd(paste0(portable, "Partitions/Correlation/Train"))
save(train.partition, file = filename)
