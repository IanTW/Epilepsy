##############################################################################################
# Author: Ian Watson                                                                         #
# Student Number: D13128934                                                                  #
# Email1: d13128934@mydit.ie                                                                 #
# Email2: iantwatson@gmail.com                                                               #
#                                                                                            # 
# Institution: Dublin Institute of Technology                                                #
# Course Code: DT228B                                                                        #
# Course Title: MSc. Data Analytics                                                          #  
# Date of Dissertation Commencement: September 2017                                          #
# Title: A Comparison of SVM and Neural Network Classifiers for Epileptic Seizure Prediction #
#                                                                                            #
# R code for implementing a machine learning classification experiment to compare the        #
# performance of SVM and neural network classifiers used for epileptic seizure prediction    #
#                                                                                            #
############################# SET UP AND LOAD PACKAGES #######################################

# Install R packages if required
# List of required packages
list.of.packages <- c("R.matlab",
                      "dplyr",
                      "eegkit")

# Create list of required new packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# If non-empty list is returned, install packages
if(length(new.packages)) install.packages(new.packages)

# Load packages
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})

############################### LINK TO OTHER SOURCE FILES ###################################
# Links to other R files
# source("otherRfile.R")
# etc

############################# CHECK INPUT FILES FOR ERRORS ###################################

# Set working directory
setwd('/home/ianw/Epilepsy')

# Make list of all data files
# Creates a dataframe with all file details - see ?file.info
listfiles <- file.info(list.files(path = "./Data", pattern = NULL, all.files = FALSE,
                                  full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))

# Create list using rownames
listfiles <- as.list(rownames(listfiles))

# Set working directory for data directory
setwd('./Data')

# Loop to check all files for errors
# Reads in files and gives error if encountered
for (filename in listfiles){
  tryCatch({
    a <- readMat(filename)
  }, error=function(e){cat("ERROR :",conditionMessage(e), filename, "\n")})
}

############################### PREPROCESSING OF DATA FILES ##################################




# Open file






# from discussion on Kaggle....
pat <- readMat('\\Data\Dog_1_interictal_segment_0018.mat')
#pat[[1]][[2]] == 600
#pat[[1]][[3]] == 5000
df <- data.frame(t(pat[[1]][[1]]))
names(df) <- unlist(pat[[1]][[4]])
head(df)
