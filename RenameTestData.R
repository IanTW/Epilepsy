######################################################################################
# File Name: RenameTestData.R                                                        #
# Purpose: Relabel 'test' data files                                                  #
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
# Parts of code adapted from Wei Wu & ESAI, Universidad CEU Cardenal Herrera,        #
# (F. Zamora-Martínez,    #F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo).        # 
#                                                                                    #
######################################################################################
# Copyright (c) 2014, Wei Wu                                                         #
#                                                                                    #
# Permission is hereby granted, free of charge, to any person obtaining a copy       #
# of this software and associated documentation files (the "Software"), to deal      #
# in the Software without restriction, including without limitation the rights       #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell          #
# copies of the Software, and to permit persons to whom the Software is              #
# furnished to do so, subject to the following conditions:                           #
#                                                                                    #
# The above copyright notice and this permission notice shall be included in all     #
# copies or substantial portions of the Software.                                    #
#                                                                                    #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR         #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,           #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE        #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER             #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,      #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS             #
# IN THE SOFTWARE.                                                                   #
#                                                                                    # 
# See: https://github.com/wei-wu-nyc/Kaggle-SeizureDetection-Official                #
#                                                                                    #
######################################################################################
# Copyright (c) 2014, ESAI, Universidad CEU Cardenal Herrera,                        #
# (F. Zamora-Martínez, F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo)             #
#                                                                                    #
# Permission is hereby granted, free of charge, to any person obtaining a copy       #
# of this software and associated documentation files (the "Software"), to deal      #
# in the Software without restriction, including without limitation the rights       #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell          #
# copies of the Software, and to permit persons to whom the Software is              #
# furnished to do so, subject to the following conditions:                           #
#                                                                                    #
# The above copyright notice and this permission notice shall be included in all     #
# copies or substantial portions of the Software.                                    #
#                                                                                    #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR         #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,           #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE        #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER             #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,      #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS             #
# IN THE SOFTWARE.                                                                   #
#                                                                                    # 
# See https://github.com/ESAI-CEU-UCH/kaggle-epilepsy                                #
#                                                                                    #
######################################################################################
# Data is from the Kaggle Epileptic Seizure Prediction Competion 2014. A large number#
# of EEG files are labelled as test segments.These files were to be used for testing #
# and to derive a private score in the competition. For the purposes of this study,  #
# the test segments will be included with the training segments to increase the size #
# of the dataset. All test files are anonymised and labelled as "*_test_segment.mat".#
# The data labels were provided by the competition host once the competion finished. #
# The data labels are contained in SzPrediction_answer_key.csv which is available    #
# from https://www.kaggle.com/c/seizure-prediction/discussion/10955                  #
#                                                                                    #
# Data files are nested according to patient. Preictal and interictal segments are   #
# filed under their relevant folder. Test segments are filed in a sub-folder         #
# ./Test_Data                                                                        #
#                                                                                    #
# ./Data/Dog_1/Test_Data                                                             #
#    |     |        |--Dog_1_test_segment_0001.mat                                   #
#    |     |        |--etc.                                                          #
#    |     |                                                                         #
#    |     |--Dog_1_interictal_segment_0001.mat                                      #
#    |     |--etc.                                                                   #
#    |                                                                               #
#    |-- /Dog_2/Test_Data                                                            #
#    |-- /Dog_3/Test_Data                                                            #
#    |-- /Dog_4/Test_Data                                                            #
#    |-- /Dog_5/Test_Data                                                            #
#    |-- /Patient_1/Test_Data                                                        #
#    |-- /Patient_2/Test_Data                                                        #
#                                                                                    #
# The csv file with the labels is read and filtered for the preictal segments.       #
# This is used to relabel the test segments.                                         #
######################################################################################

# This file should be called from the main code file SeizurePrediction.R

# List of files that are preictal (from the .csv containing the data labels)
file.input <- read.csv("SzPrediction_answer_key.csv", header = TRUE, sep = ",")
# Filter out preictal labels only
preict.files <- file.input[file.input$preictal == 1,]
# Get the names of the clips
preict.files <- as.character(preict.files$clip)
# Convert to a list
preict.files <- as.list(preict.files)

# Loop for all patient folder 
for (folder in patient.name){   
        
        # Set working directory here
        data.dir <- paste0(parent.dir, folder, "/Test_Data")
        setwd(data.dir)
     
        # Prefix any files that are listed as preictal, based on the list provided 
        for (myfiles in preict.files){
          file.rename(myfiles, paste0("Preict_", myfiles))
          }
        # Get files with Preict prefix
        myfilelist <- list.files(data.dir, pattern = "[P][r]")
        # Files are labelled as preictal with an index of 2000 and up
        # Calculate the number of files
        numfile <- length(myfilelist) + 1999
        # Rename as preictal ('to' and 'from' in rename command must match)
        file.rename(myfilelist, paste0(mytype,
                                       "_preictal_segment_",
                                       2000:numfile, ".mat"))
        
        # Get all other files and label interictal
        myfilelist <- list.files(data.dir, pattern = "[t][e][s]")
        # Files are labelled as interictal with an index of 2000 and up
        # Calculate the number of files
        numfile <- length(myfilelist) + 1999
        # Rename as interictal ('to' and 'from' in rename command must match)
        file.rename(myfilelist, paste0(mytype,
                                       "_interictal_segment_",
                                       2000:numfile, ".mat"))
}

######################################################################################