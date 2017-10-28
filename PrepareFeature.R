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
# Drop header columns
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
# Set up simple partition based on Split variable
# Set random seed for reproducability
set.seed(893)

# Create random data split
train.index <- createDataPartition (combined.feature$CLASS,
                                        p = split,  # Set training size
                                        list = FALSE)
# Set up label for files
trainsize <- split*100
testsize <- (1-split)*100
file.label <- paste(trainsize, "_", testsize, sep = "")

# Subset for training partition
train.partition <- combined.feature[train.index, ]
# Save to file
setwd('E:/Partitions')
save(train.partition, file = paste0("Simple_Train_Partition_",file.label,".rda"))

# Subset for test partition
test.partition <- combined.feature[-train.index, ]
# Save to file
save(test.partition, file = paste0("Simple_Test_Partition_",file.label,".rda"))

######################################################################################
# Check correct numbers of records

nrow(combined.feature[combined.feature$CLASS == 'Preictal',])
nrow(test.partition[test.partition$CLASS == 'Preictal',])
nrow(train.partition[train.partition$CLASS == 'Preictal',])

nrow(combined.feature[combined.feature$CLASS == 'Interictal',])
nrow(test.partition[test.partition$CLASS == 'Interictal',])
nrow(train.partition[train.partition$CLASS == 'Interictal',])

