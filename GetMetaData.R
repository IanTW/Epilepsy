##############################################################################################
# File Name: GetMetaData.R                                                                   #
# Purpose: Contains code for summarising metadata from EEG data files                        #
#                                                                                            #
# Author: Ian Watson                                                                         #
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
# Parts of code adapted from Wei Wu & ESAI, Universidad CEU Cardenal Herrera,                #
# (F. Zamora-Martínez,    #F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo).                # 
#                                                                                            #
##############################################################################################
# Copyright (c) 2014, Wei Wu                                                                 #
#                                                                                            #
# Permission is hereby granted, free of charge, to any person obtaining a copy               #
# of this software and associated documentation files (the "Software"), to deal              #
# in the Software without restriction, including without limitation the rights               #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell                  #
# copies of the Software, and to permit persons to whom the Software is                      #
# furnished to do so, subject to the following conditions:                                   #
#                                                                                            #
# The above copyright notice and this permission notice shall be included in all             #
# copies or substantial portions of the Software.                                            #
#                                                                                            #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR                 #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                   #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                     #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,              #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS                     #
# IN THE SOFTWARE.                                                                           #
#                                                                                            # 
# See: https://github.com/wei-wu-nyc/Kaggle-SeizureDetection-Official                        #
#                                                                                            #
##############################################################################################
# Copyright (c) 2014, ESAI, Universidad CEU Cardenal Herrera,                                #
# (F. Zamora-Martínez, F. Muñoz-Malmaraz, P. Botella-Rocamora, J. Pardo)                     #
#                                                                                            #
# Permission is hereby granted, free of charge, to any person obtaining a copy               #
# of this software and associated documentation files (the "Software"), to deal              #
# in the Software without restriction, including without limitation the rights               #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell                  #
# copies of the Software, and to permit persons to whom the Software is                      #
# furnished to do so, subject to the following conditions:                                   #
#                                                                                            #
# The above copyright notice and this permission notice shall be included in all             #
# copies or substantial portions of the Software.                                            #
#                                                                                            #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR                 #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                   #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                     #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,              #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS                     #
# IN THE SOFTWARE.                                                                           #
#                                                                                            # 
# See https://github.com/ESAI-CEU-UCH/kaggle-epilepsy                                        #
#                                                                                            #
##############################################################################################

# This file should be called from the main code file SeizurePrediction.R

# Get metadata for all files and summarise
metadata <- function (){
  
  # Initialise counter
  inum = 1
 
  # Set up logical array with 7000 elements; 1000 elements per patient
  # This would perhaps be the array of results eg 0 = non-seizure 1 = seizure.
  # Set up logical array with 7000 elements
  nv=rep(NA,length(types)*100)
  # Create results data frame
  summary_df=data.frame(fname=nv, second=nv, freq=nv, ch=nv,label=nv, seq=nv, datalen=nv)
  
  for (mytype in types) {
    # Set path for data files, each patient in its own folder
    datadir=paste0(parent.dir,mytype)
    
    # Get list of interictal files
    interfiles=dir(datadir, ".*_interictal_segment_.*.mat")
    # Get list of preicatal files
    prefiles=dir(datadir, ".*_preictal_segment_.*.mat")
    
    for (myfile in interfiles) {
      filename=paste0(datadir,"/",myfile)
      # Read in .mat file
      retval=read_one_matfile(filename)
      # Get length of clip
      seconds=retval[["seconds"]]
      # Get sample frequency
      freq=retval[["freq"]]
      # Get sequence number
      seq=retval[["seq"]]
      # Get electrode labels
      labels=retval[["labels"]]
      summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
                          retval[["labels"]][1], retval[["seq"]],ncol(retval[[1]]))
      inum=inum+1
    }
    
    for (myfile in prefiles) {
      filename=paste0(datadir,"/",myfile)
      # Read in .mat file
      retval=read_one_matfile(filename)
      # Get length of clip
      seconds=retval[["seconds"]]
      # Get sample frequency
      freq=retval[["freq"]]
      # Get sequence number
      seq=retval[["seq"]]
      summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
                          retval[["labels"]][1], retval[["seq"]],ncol(retval[[1]]))
      inum=inum+1
      
    }
    
    
  }
  
  return(summary_df)
  
}

#Run the following to get the summary
#a <- metadata()
