######################################################################################
# File Name: MakeFeature.R                                                           #
# Purpose: Feature Engineering                                                       #
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

# This file should be called from the main code file SeizurePrediction.R

######################################################################################

#Variables for development to be deleted
# variables for manual looping 

folder <- "Dog_1"
filename <- "Dog_1_interictal_segment_0476.mat"
slices <- 1
channels <- 1
prefix <- "Chan_1"

######################################################################################

feature.statistic <- function (EEG.channel, prefix){
  # Calculates summary statistics for a single windowed EEG channel
  #
  # Args:
  #   feature.vector: Object containing extracted features
  #   EEG.channel: EEG channel containing times series data
  #   prefix: label for identifying channel number
  # Returns:
  #   feature.vector: extracted features
  
  # Get maximum value in series
  feature.vector[paste0(prefix,"_Max")] <- max(EEG.channel)
  # Get mean value (1st moment)
  feature.vector[paste0(prefix,"_Mean")] <- moment(EEG.channel, order = 1)
  # Get variance value (2nd moment)
  feature.vector[paste0(prefix,"_Variance")] <- moment(EEG.channel, order = 2)
  # Get skewness value (3rd moment)
  feature.vector[paste0(prefix,"_Skew")] <- moment(EEG.channel, order = 3)
  # Get kurtosis value (4th moment)
  feature.vector[paste0(prefix,"_Kurtosis")] <- moment(EEG.channel, order = 4)
 
  # Return
  feature.vector
}

######################################################################################

# Function to window a multichannel EEG time series
window.matrix <- function (EEG.time.series, num.split){
  # Windows a single EEG time series matrix into a list of windowed matrices.
  #
  # Args:
  #   EEG.time.series: Multichannel EEG data in a matrix; each row is an EEG channel
  #   num.split: The number of splits to perform
  # Returns:
  #   a list of EEG matrices. The no. of elements in the list equals num.split
  #   The first element contains data from 0 to windowsize (seconds). The second
  #   element contains data from windowsize to 2*windowsize (and so on). 
  
  # Initialise a list
  window.eeg <- list()
  
  # Calculate the total samples for each windowed matrix
  # Total data samples in time series / number of splits
  number.samples <- ncol(EEG.time.series) / num.split 
  # Make a list of windowed matrices
  # Loop for each split
  for (i in 1:num.split) {  # Create a list of matrices
    window.eeg[[i]] <- EEG.time.series[,((i-1)*number.samples+1):
                                         (i*number.samples)] 
  } 
  # return
  window.eeg
}

######################################################################################

# Loop for all folders with patient data
for (folder in patient.name) {
  
  # Set working directory here
  # This can be used on the full dataset...
  data.dir <- paste0(parent.dir, folder)
  # ...or it can be used on the sample data
  # data.dir <- paste0(sample.dataset.dir, folder)
  setwd(data.dir)
  
  # Get list of files for processing
  list.of.files <- dir(data.dir, "*.mat")

  # Loop for all files in the patient folder
  for (filename in list.of.files) {
    
    # Message to console
    cat(paste0("Processing ", filename, " ", Sys.time()), "\n")
    # Read in .mat file
    EEG.file <- read.EEG.file(filename)
    # Get time series data
    EEG.data <- EEG.file[["mat"]]
    # Get length of file segment in seconds
    seconds <- EEG.file[["seconds"]]
    # Get sample frequency
    freq <- EEG.file[["frequency"]]
    # Get sequence number of file
    seq <- EEG.file[["sequence"]]
    # Get electrode labels
    labels <- EEG.file[["labels"]]
    
    # Window the time series data
    # Calculate the required number of splits
    num.split <- seconds/windowsize  # The total length in seconds / size of window
    # Create a set of windowed data matrices
    # Splits matrix into a list of matrices, each 1/nsplit of the original is size
    EEG.window <- window.matrix (EEG.data, num.split)  
    
    # Loop for all slices in the windowed data
    for (slices in 1:nsplit) { # Step through the individual smaller matrices)
    
      # Get a slice from the windowed data
      EEG.slice <- EEG.window[[slices]]
      #Get the number of EEG channels
      num.channel <- nrow(EEG.slice)
      
      # Initialise a feature vector for the slice
      feature.vector <- c()
      
      # Loop for all channels in the EEG slice
      for (channels in 1:num.channel) {
        prefix <- paste0("Chan_",channels) # Make a prefix with channel name  
        EEG.channel <- EEG.slice[channels,]  # Get row from matrix
        
        # Get summary statistics and append to feature vector
        feature.vector <- feature.statistic(EEG.channel, prefix)
        
        # Other feature functions here
        #feature.vector <- feature.xxxxx
      }
      
      
      #RESUME HERE
      # TO DO - set up labels for each vector
      #Make sure loop works for entire file/all files in folder
      #Make sure loop works for entire folder/all folders
      #Write output to a file
      
      
      #add in sequence
      feature.vector['seq']=seq
      
      #Flag for what?
      f['flag']=0
      
      # add in ID based on typenum list = Dog_1 = 1 etc, base ID (isub) and slice number (mi)
      f['id']=typenums[mytype]*100000+isub*100+mi
      #Add in slice number
      f['si']=mi
      
      # Make training matrix
      if(is.null(trainmat)) {
        trainmat=f  # copy first row if null
      } else {
        trainmat=rbind(trainmat, f) #otherwise row bind to existing
      }
    }
      
      
    }
  }
}


################################################################################################################################################

# Generate features for ONE CHANNEL
gen_features_onearray <- function (indata, pre, freqs) {
  
  #Args:  indata is a time-series of data from one channel. Can be orginal, delta1 or delta2.
  #       pre is the channel name
  #       freqs is that weird ass sequence...
  #Think this might be calculating statistical moments ???
  
  # Init object
  feats=c()
  #Summary statistics from Summary() function
  sumheads=c('Min','1stQrt','Med','Mean','3rdQrt','Max')
  sumindex=c(4,6)
  #Set mean and max
  sumheads=sumheads[sumindex]
  
  #Get summary stats for MEAN and MAX
  a=summary(indata)[sumindex] # Gets the 4th and 6th elements from the summary data (mean and max)
  #Get Mean and Max and populate
  feats[paste0(pre,"_",sumheads)] = a
  
  #Get SD and populate
  feats[paste0(pre,"_","Sd")]=sd(indata)
  
  # Get absolute value for mean and max (calculates the mean Amplitude and max Amplitude)
  a=summary(abs(indata))[sumindex]
  feats[paste0(pre, "Amp", "_",sumheads)] = a
  
  #Calculate the SD of the amplitude
  feats[paste0(pre,"Amp_Sd")]=sd(abs(indata))
  
  #Calculated this sequence again...
  freqs=freqs[1:length(indata)]
  #	cat(paste("In gen_features_onearray(), before fft size is:",paste(dim(indata),collapse=','),"\n"))
  #	flush.console()
  #	b=Sys.time()
  #FFT function follows
  
  #if (dolog == 1) { # Set in early parameter setup
  #  myfft=log(abs(fft(indata))+1) #log of FFT - what is the difference FFT vs log FFT??
  #} else {
  #  myfft=abs(fft(indata))
  #}
  #Calculate mean and max of FFT series
  #a=summary(myfft)[sumindex]
  #	cat(paste("After fft, time used:",format(Sys.time()-b),"\n"));flush.console()
  # Paste into feature vector
  #heads=paste0(pre,"FFT_",sumheads)
  #feats[heads]=a
  #Calculate SD and max Freq and paste into vector
  #feats[paste0(pre,"FFT_","Sd")]=sd(myfft)
  #feats[paste0(pre,"FFT_","MaxFreq")]=freqs[which.max(myfft)]
  
  #if (addFFT == 1 || addFFT == 2) { #set variable at beginning
  #  if (addFFT == 2) {
  #    myfft=(myfft-mean(myfft))/sd(myfft) # FFT - mean(FFT)/sd(FFT)
  #  }
  #  
  #  myfft=myfft[1:round(FFTratio*length(myfft))] # FFT ratio set at beginning
  #  myfft=down_sampling(myfft, round(length(myfft)/FFTavglen))
  #  colnames=paste0(pre,"FFT_",sprintf("%02d",1:length(myfft)))
  #  feats[colnames]=myfft
  #}
  feats
}

# SPLITS SERIES INTO THREE SERIES (ORIG. DELTA1 & DELTA2)
gen_features_oneseries <- function (indata, pre, freqs) {
  # Args: indata is a numeric sequence consisting of all the data points for a singel EEG channel 
  #       pre is channel name
  #       freqs is a numeric sequence 1/60 to 1/24000
  
  #init object
  feats=c()
  #convert to vector seems redundant - is.vector returns TRUE before this command
  indata=as.vector(indata)
  
  # Initialise numeric object
  delta1=rep(0,length(indata))
  
  # This is simply done by the R diff() function 
  # Calculates the difference between consecutive data points in indata d(n)-d(n-1)
  # for indata = (47, 41, 43, 53...) delta1 = (-6,2,10...)
  delta1[1:(length(indata)-1)]=indata[2:length(indata)]-indata[1:(length(indata)-1)]
  
  #This can be done by the diff() of the diff()
  # Calculates the difference between consecutive data points in delta1 d(n)-d(n-1)
  # for delta1 = (-6, 2, 10, -6 ...) delta2 = (8,8,-16...)
  delta2=rep(0,length(indata))
  delta2[1:(length(delta1)-2)]=delta1[2:(length(delta1)-1)]-delta1[1:(length(delta1)-2)]
  
  #Generate features for original time series (indata) and two delta series (delta1 and delta2)
  feats=c(feats, gen_features_onearray(indata, paste0(pre,""), freqs)) # 33 features
  feats=c(feats, gen_features_onearray(delta1, paste0(pre,"Del1"), freqs)) #33 features
  feats=c(feats, gen_features_onearray(delta2, paste0(pre,"Del2"), freqs)) # 33 features, total 99
  feats
}

# SPLITS FILE INTO CHANNELS
gen_features_onefile <- function (indata, f, t) {
  
  #  Args: indata = submatrix consisting of 10 seconds of data 
  #        f = sample frequency
  #        t = length of time of matrix (seconds/nsplit = 600/10 = 60 seconds)
  
  #Set timere
  #b=Sys.time()
  #Initialise object
  feats=c()
  # Get number of channels (16)
  chans=nrow(indata)
  # Get number of data points (~24000)
  nc=ncol(indata)
  # Seems redundant - at no point is the number of data points precisely = sample rate x seconds
  # Will crap out every time
  #if (round(f*t) != nc) {
  #	stop(paste("Stop in gen_features_onefile(), f*t=",f*t,"!= number of data points,",nc))
  #}
  
  freqs=(1:nc)*1/t # Takes the column index and divides by 60. WTF for ???
  
  # Loop for each channel
  for (i in 1:chans) {
    prename=paste0("chan",i) # make a channel name  = 'chan1', 'chan2' etc
    # passes a single channel to the function, the channel name and that fucking odd 'freqs' numeric sequence
    # The single channel is passed as a numeric sequence; class(indata[1,]) = 'numeric'
    feats=c(feats,gen_features_oneseries (indata[i,], pre=prename, freqs)) 
  }
  # Get the labels from the feature vector
  fnames=names(feats)
  cat(paste("Total number of features before global features",length(feats),format(Sys.time()-b),"\n"))
  #	flush.console()
  #	uniq_postfix=grep('^chan[0-9]*FFT_[0-9][0-9]',fnames,invert=T,value=T)
  uniq_postfix=fnames
  #Regex to remove chanX references
  uniq_postfix=unique(gsub('^Chan_[0-9]*','',uniq_postfix))
  # Loop through labels
  # For max, mean, skew, kurtosis or whatever feature, get the value for each channel 
  for (mypost in uniq_postfix) {
    pre=paste0("All",mypost)
    mynames=grep(paste0('^Chan_[0-9]*',mypost),fnames,value=T)
    mydata=feats[mynames]
    sumheads=c('Min','1stQrt','Med','Mean','3rdQrt','Max')
    sumindex=c(4,6) # For mean and max
    sumheads=sumheads[sumindex]
    a=summary(mydata)[sumindex] #Calculate the mean and max of whatever feature....
    heads=paste0(pre, "_",sumheads)
    feature.vector[heads]=a
    feature.vector[paste0(pre,"_","Sd")]=sd(abs(mydata)) # calculate sd of abs(value)
  }
  #	cat(paste("Total number of features after AllCh features",length(feats),format(Sys.time()-b),"\n"))
  #	flush.console()
  # Set of features based on the average across all channels - passed to make orig, delta1, delta2....
  feats=c(feats,gen_features_oneseries (colMeans(indata), pre="chanAvg", freqs))
  #	cat(paste("Total number of features after chAvg features",length(feats),format(Sys.time()-b),"\n"))
  #	flush.console()
  mydata=indata
  
  #Setting up some kind of temporary matrix. for covariance calc?
  for (del in 0:2) {
    if (del != 0) {
      mydata_tmp=matrix(rep(0,length(indata)), nrow=nrow(indata))
      mydata_tmp[,1:(ncol(mydata)-del)]=mydata[,2:(ncol(mydata)-del+1)]-mydata[,1:(ncol(mydata)-del)]
      mydata=mydata_tmp
      rm(mydata_tmp)
      pre=paste0("Del",del)
    } else {
      mydata=indata
      pre=""
    }
    
    #Covariance calculation
    #t() is transpose
    cov=cov(t(mydata))
    #		cat(paste("Size of data is:", paste(dim(mydata),collapse=','),
    #			"Size of covar is:",paste(dim(cov),collapse=','),"\n"));flush.console()
    covarray=abs(cov[upper.tri(cov)])
    feats[paste0(pre,'CovMean')]=mean(covarray)
    feats[paste0(pre,'CovSd')]=sd(covarray)
    covarray=covarray[order(covarray,decreasing=T)]
    feats[paste0(pre,'CovMax')]=covarray[1]
    feats[paste0(pre,'Cov2nd')]=covarray[2]
    feats[paste0(pre,'Cov3rd')]=covarray[3]
    #		cat(paste("Before FFT", format(Sys.time()-b),"\n"));flush.console()
    
    # Now fft again 
    myfft=matrix(rep(0,length(mydata)),nrow=nrow(mydata))
    for (i in 1:chans) {
      if (dolog == 1) {
        myfft[i,]=log(abs(fft(mydata[i,]))+1)
      } else {
        myfft[i,]=abs(fft(mydata[i,]))
      }
    }
    #		cat(paste("After FFT", format(Sys.time()-b),"\n"));flush.console()
    pre=paste0(pre,"FFT")
    
    #Now covariance of FFT
    #t() means transpose of matrix
    
    cov=cov(t(myfft))
    #		cat(paste("Size of FFT data is:", paste(dim(myfft),collapse=','),
    #			"Size of FFT covar is:",paste(dim(cov),collapse=','),"\n"));flush.console()
    covarray=abs(cov[upper.tri(cov)])
    feats[paste0(pre,'CovMean')]=mean(covarray)
    feats[paste0(pre,'CovSd')]=sd(covarray)
    covarray=covarray[order(covarray,decreasing=T)]
    feats[paste0(pre,'CovMax')]=covarray[1]
    feats[paste0(pre,'Cov2nd')]=covarray[2]
    feats[paste0(pre,'Cov3rd')]=covarray[3]
    #		gc()
    #		cat(paste("Total number of features after Del",del,"covar features",length(feats),
    #			format(Sys.time()-b),"\n"))
    #		flush.console()
  }
  #	cat(paste("Total number of features after covar features",length(feats),format(Sys.time()-b),"\n"))
  flush.console()
  feats
}

# Returns a list of values. Each element in the list is a window or segment of the data. There are x elements corresponding to y splits.
# If nsplit = 10 then there are 10 elements in the list
# Windowing function
split_mat <- function (mymat, nsplit) {  # this error function breaks every time - not sure what it achieves
  #	if (ncol(mymat) %% nsplit != 0) {  # x %% y or x modulus y
  #		stop(paste("In split_mat(), the nsplit",nsplit,"and column number",ncol(mymat),"do not match to even blocks."))
  #	}
  # Set up a list
  retdata=list()
  # length for each sub-matrix
  mysize=ncol(mymat) / nsplit # Total colums / number of splits
  for (i in 1:nsplit) {
    retdata[[i]]=mymat[,((i-1)*mysize+1):(i*mysize)] # Create a list of matrices  - column 0:23976, then 23977:47952, 
  } 
  # return
  retdata
}

# Initialise project variables
{
  # Set target sample rate for resampling (Hz)
  fixfreq=200
  # Set window size 
  nsplit=10	# original clip is 10min=600sec, nsplit=10, new length=60sec, nsplit=20, newlength=30sec, nsplit=40 newlength=15sec
  
  # FFT parameters
  # Base for log calc?
  dolog=1
  addFFT=2
  FFTratio=0.2
  FFTavglen=24
  # Set counter to identify record number 
  inum=1
  
  # Endode data file categories into a numerical annotation
  types=c('Dog_1','Dog_2','Dog_3','Dog_4','Dog_5','Patient_1','Patient_2')
  typenums=c(Dog_1=1, Dog_2=2, Dog_3=3, Dog_4=4, Dog_5=5, Patient_1=6, Patient_2=7)
  
  # Set up logical array with 7000 elements. Why 7000? Are there 1000 files per patient?
  nv=rep(NA,length(types)*1000)
  # Create results data frame
  summary_df=data.frame(fname=nv, second=nv,freq=nv,ch=nv,label=nv,seq=nv,datalen=nv, time1=nv,time2=nv)
}

# Main code sequence
# Loop for all patients
for (mytype in types) {
  # Set path for data files, each patient in its own folder
  datadir=paste0(parent.dir,data.dir,mytype)
  
  # Get list of interictal files
  interfiles=dir(datadir, ".*_interictal_segment_.*.mat")
  # Get list of preictal files
  prefiles=dir(datadir, ".*_preictal_segment_.*.mat")
  
  # Initialise object for training data
  trainmat=NULL
  # Set timer for ??
  #begTime=Sys.time()
  # Set counter for ID of feature vector
  isub=1
  
  # Loop for all interictal files
  for (myfile in interfiles) {
    # Set timer for ??
    begTime0=Sys.time()
    # Get filename
    filename=paste0(datadir,"/",myfile)
    # Read in .mat file
    retval=read_one_matfile(filename)
    # Save the electrode data component 
    orimat=retval[["mat"]]
    # Get length of clip
    seconds=retval[["seconds"]]
    # Get sample frequency
    freq=retval[["freq"]]
    # Get electrode labels
    labels=retval[["labels"]]
    # Get sequence number
    seq=retval[["seq"]]
    
    # Code for splitting or windowing
    # Try to find a built-in funtion for doing this
    newmats=split_mat(orimat, nsplit)  # Splits matrix into a list of matrices, each 1/nsplit of the original is size
    for (mi in 1:nsplit) { # Step through the individual smaller matrices
      mymat=newmats[[mi]]  # Call the matrix by referencing the list 
      
      #f is the feature vector
      f=gen_features_onefile(mymat, freq, seconds/nsplit) # Call the function to make features....
      #PHEW finally back here with a big-ass feature vector
      
      #add in sequence
      f['seq']=seq
      #Flag for what?
      f['flag']=0
      
      # add in ID based on typenum list = Dog_1 = 1 etc, base ID (isub) and slice number (mi)
      f['id']=typenums[mytype]*100000+isub*100+mi
      #Add in slice number
      f['si']=mi
      
      # Make training matrix
      if(is.null(trainmat)) {
        trainmat=f  # copy first row if null
      } else {
        trainmat=rbind(trainmat, f) #otherwise row bind to existing
      }
    }
    endTime=Sys.time()
    time1=difftime(endTime, begTime0, units="secs")
    time2=difftime(endTime, begTime, units="secs")
    cat(paste(inum, myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
              retval[["labels"]][1], retval[["seq"]],ncol(orimat), time1, time2,"\n"))
    summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
                        retval[["labels"]][1], retval[["seq"]],ncol(orimat), time1, time2)
    inum=inum+1
    isub=isub+1
    flush.console()
  }
  
  # Loop for all preictal files
  for (myfile in prefiles) {
    begTime0=Sys.time()
    filename=paste0(datadir,"/",myfile)
    retval=read_one_matfile(filename)
    orimat=retval[["mat"]]
    seconds=retval[["seconds"]]
    freq=retval[["freq"]]
    labels=retval[["labels"]]
    seq=retval[["seq"]]
    if (fixfreq > 0 && fixfreq < freq) {
      orimat=down_sampling(orimat, freq/fixfreq)
      freq=fixfreq
    }
    newmats=split_mat(orimat, nsplit)
    for (mi in 1:nsplit) {
      mymat=newmats[[mi]]
      f=gen_features_onefile(mymat, freq, seconds/nsplit)
      f['seq']=seq
      f['flag']=1
      f['id']=typenums[mytype]*100000+isub*100+mi
      f['si']=mi
      if(is.null(trainmat)) {
        trainmat=f
      } else {
        trainmat=rbind(trainmat, f)
      }
    }
    endTime=Sys.time()
    time1=difftime(endTime, begTime0, units="secs")
    time2=difftime(endTime, begTime, units="secs")
    cat(paste(inum, myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),retval[["labels"]][1],
              retval[["seq"]],ncol(orimat),time1, time2,"\n"))
    summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
                        retval[["labels"]][1], retval[["seq"]],ncol(orimat),time1, time2)
    inum=inum+1
    isub=isub+1
    flush.console()
  }
  
  testmat=NULL
  for (myfile in testfiles) {
    begTime0=Sys.time()
    filename=paste0(datadir,"/",myfile)
    retval=read_one_matfile(filename)
    orimat=retval[["mat"]]
    seconds=retval[["seconds"]]
    freq=retval[["freq"]]
    labels=retval[["labels"]]
    seq=retval[["seq"]]
    if (fixfreq > 0 && fixfreq < freq) {
      orimat=down_sampling(orimat, freq/fixfreq)
      freq=fixfreq
    }
    newmats=split_mat(orimat, nsplit)
    for (mi in 1:nsplit) {
      mymat=newmats[[mi]]
      f=gen_features_onefile(mymat, freq, seconds/nsplit)
      f['id']=typenums[mytype]*100000+isub*100+mi
      f['si']=mi
      if(is.null(testmat)) {
        testmat=f
      } else {
        testmat=rbind(testmat, f)
      }
    }
    endTime=Sys.time()
    time1=difftime(endTime, begTime0, units="secs")
    time2=difftime(endTime, begTime, units="secs")
    cat(paste(inum, myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),retval[["labels"]][1],
              retval[["seq"]],ncol(orimat),time1, time2,"\n"))
    summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
                        retval[["labels"]][1], retval[["seq"]],ncol(orimat),time1, time2)
    inum=inum+1
    isub=isub+1
    flush.console()
  }
  
  if (mytype %in% c("Dog_1","Dog_2","Dog_3","Dog_4") && do_holdout) {
    holdoutmat=NULL
    for (myfile in holdoutfiles) {
      begTime0=Sys.time()
      filename=paste0(holdoutdir,"/",myfile)
      retval=read_one_matfile(filename)
      orimat=retval[["mat"]]
      seconds=retval[["seconds"]]
      freq=retval[["freq"]]
      labels=retval[["labels"]]
      seq=retval[["seq"]]
      if (fixfreq > 0 && fixfreq < freq) {
        orimat=down_sampling(orimat, freq/fixfreq)
        freq=fixfreq
      }
      newmats=split_mat(orimat, nsplit)
      for (mi in 1:nsplit) {
        mymat=newmats[[mi]]
        f=gen_features_onefile(mymat, freq, seconds/nsplit)
        f['id']=typenums[mytype]*100000+isub*100+mi
        f['si']=mi
        if(is.null(holdoutmat)) {
          holdoutmat=f
        } else {
          holdoutmat=rbind(holdoutmat, f)
        }
      }
      endTime=Sys.time()
      time1=difftime(endTime, begTime0, units="secs")
      time2=difftime(endTime, begTime, units="secs")
      cat(paste(inum, myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),retval[["labels"]][1],
                retval[["seq"]],ncol(orimat),time1, time2,"\n"))
      summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
                          retval[["labels"]][1], retval[["seq"]],ncol(orimat),time1, time2)
      inum=inum+1
      isub=isub+1
      flush.console()
    }
  }
  # datadir=paste0("Data/",mytype)
  datadir=paste0("F:/Work/Kaggle/SeizurePrediction/Kaggle-SeizureDetection-Official/Data/",mytype)
  if (addFFT>=1) {
    freq=paste0(freq,"FFT",addFFT,"-",FFTratio)
  }
  
  #Write output to zip file eg "Dog_1_Split_10_Freq_400.RData"
  
  filename=paste0(datadir,"/",mytype,"Split",nsplit,"_Freq",freq,ifelse(dolog==1,paste0("Log",dolog),""),".RData")
  summary_df=summary_df[!is.na(summary_df$fname),]
  if (mytype %in% c("Dog_1","Dog_2","Dog_3","Dog_4") && do_holdout) {
    save(trainmat, testmat, holdoutmat, summary_df, file=filename, compress='bzip2')
  } else {
    save(trainmat, testmat, summary_df, file=filename, compress='bzip2')
  }
  endTime=Sys.time()
  cat(paste("Total time used for",mytype,":",format(endTime-begTime0),"\n"))
  cat(paste("File saved:",filename,"\n"))
  flush.console()
}

#Get rid of NA
summary_df=summary_df[!is.na(summary_df$fname),]


totaltime=(Sys.time()-begTime)
print(tail(summary_df, n=20))
print(totaltime)

