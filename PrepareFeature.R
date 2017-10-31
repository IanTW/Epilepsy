######################################################################################
# File Name: PrepareFeature.R                                                        #
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

# Combine Statistics and FFT features per patient
# This code is specfic for the folders and number of features
# It cannot be run generically

for (folder in folder.list){

setwd('E:/Features/Set_1_A')
load (paste0(folder,"_features.rda"))
df1 <- feature.vector.matrix
setwd('E:/Features/Set_2_A')
load (paste0(folder,"_features.rda"))
df2 <- feature.vector.matrix

# Bind
df3 <- cbind(df1,df2)
# Drop duplicate header columns
df <- df3[, c(1:84,89:312)]

#Set output directory - does the directory exist?
setwd('E:/Features/Set_3_A')
# Save to file
save(df, file = paste0(folder,"_stat_plus_FFT_features.rda"))
}

# Make sure there is no existing copy already !!!
# Combine all feature vectors into one dataframe
setwd('E:/Features/Set_3_A')
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
save(combined.feature, file = "Combined_stat_plus_FFT_features.rda")

# Number of preictal rows
n_preictal <- nrow(combined.feature[combined.feature$CLASS == "Preictal",])
# Number of interictal rows
n_interictal <- nrow(combined.feature[combined.feature$CLASS == "Interictal",])

#######################################################################################

# Set up simple partition based on split ratio WITH ALL DATA FILES

# Load combined feature vector
setwd(paste0(portable,"/Features/Set_3_A"))
load("Combined_stat_plus_FFT_features.rda")

# Load metadata file
setwd(data.dir)
load("metadata.rda")

#List of preictal files
preictal.files <- meta.data.results[grep("preictal", meta.data.results$filename),]
interictal.files <- meta.data.results[grep("interictal", meta.data.results$filename),]

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

# Add numeric ID
train.partition$NID <- seq.int(nrow(train.partition))
train.partition <- train.partition[,c(307,1,2,3:306)]
test.partition$NID <- seq.int(nrow(test.partition))
test.partition <- test.partition[,c(307,1,2,3:306)]

# Set up labels for files
trainsize <- split*100
testsize <- (1-split)*100
file.label <- paste(trainsize, "_", testsize, sep = "")
labtr <- "Simple_Train_Partition_"
labte <- "Simple_Test_Partition_"

# Save to file
setwd(paste0(portable, "/Partitions"))
save(train.partition, file = paste0(labtr, file.label, ".rda"))
save(test.partition, file = paste0(labte, file.label, ".rda"))

######################################################################################

# Set up simple partition based on split ratio WITH REDUCED DATA FILES

# Load combined feature vector
setwd(paste0(portable,"/Features/Set_3_A"))
load("Combined_stat_plus_FFT_features.rda")

# Load metadata file
setwd(data.dir)
load("metadata.rda")

#List of preictal and interictal files
preictal.files <- meta.data.results[grep("preictal", meta.data.results$filename),]
interictal.files <- meta.data.results[grep("interictal", meta.data.results$filename),]

n_preictal <- nrow(preictal.files)

# Set random seed for reproducability
set.seed(379)

#Reduce number of interictal files to match preictal
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

# Add numeric ID
train.partition$NID <- seq.int(nrow(train.partition))
train.partition <- train.partition[,c(307,1,2,3:306)]
test.partition$NID <- seq.int(nrow(test.partition))
test.partition <- test.partition[,c(307,1,2,3:306)]

# Set up labels for files
trainsize <- split*100
testsize <- (1-split)*100
file.label <- paste(trainsize, "_", testsize, sep = "")
labtr <- "Reduced_Train_Partition_"
labte <- "Reduced_Test_Partition_"

# Save to file
setwd(paste0(portable, "/Partitions"))
save(train.partition, file = paste0(labtr, file.label, ".rda"))
save(test.partition, file = paste0(labte, file.label, ".rda"))

######################################################################################

# Check correct numbers of records

setwd(paste0(portable,"/Features/Set_3_A"))
load("Combined_stat_plus_FFT_features.rda")
nrow(combined.feature[combined.feature$CLASS == 'Preictal',])
nrow(combined.feature[combined.feature$CLASS == 'Interictal',])

setwd(paste0(portable, "/Partitions"))
load("Reduced_Test_Partition_70_30.rda")
load("Reduced_Train_Partition_70_30.rda")

nrow(test.partition[test.partition$CLASS == 'Preictal',])
nrow(train.partition[train.partition$CLASS == 'Preictal',])
nrow(test.partition[test.partition$CLASS == 'Interictal',])
nrow(train.partition[train.partition$CLASS == 'Interictal',])

load("Simple_Test_Partition_70_30.rda")
load("Simple_Train_Partition_70_30.rda")

nrow(test.partition[test.partition$CLASS == 'Interictal',])
nrow(train.partition[train.partition$CLASS == 'Interictal',])
nrow(test.partition[test.partition$CLASS == 'Preictal',])
nrow(train.partition[train.partition$CLASS == 'Preictal',])


######################################################################################