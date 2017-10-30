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

# This file should be called from the main code file SeizurePrediction.R

######################################################################################

# Variables for development to be deleted

#folder <- "Dog_1"
#filename <- "Dog_1_interictal_segment_0476.mat"
#slices <- 1
#channels <- 1
#prefix <- "Chan_1"

######################################################################################

# Function to calculate basis statistics
feature.statistic <- function (EEG.channel, prefix){
  # Calculates summary statistics for a single windowed EEG channel
  #
  # Args:
  #   EEG.channel: EEG channel containing times series data
  #   prefix: label for identifying channel number
  # Returns:
  #   feature.vector: extracted features
  
  # Get maximum value in series
  feature.vector[paste0(prefix,"_Max")] <- max(EEG.channel)
  # Get mean value (1st moment) of absolute value
  feature.vector[paste0(prefix,"_Mean")] <- moment(abs(EEG.channel), order = 1)
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

# Function to window a multichannel EEG time series (non-overlapping)
window.matrix <- function (EEG.data, num.split){
  # Windows a single EEG time series matrix into a list of windowed matrices.
  # The windows are non-overlapping.
  #
  # Args:
  #   EEG.data: Multichannel EEG data in a matrix; each row is an EEG channel
  #   num.split: The number of splits to perform
  # Returns:
  #   EEG.window: List of EEG matrices.
  #   The no. of elements in the list equals num.split
  #   The first element contains data from 0 to windowsize (seconds). The second
  #   element contains data from windowsize to 2*windowsize (and so on). 
  
  # Initialise a list
  EEG.window <- list()
  
  # Calculate the total samples for each windowed matrix
  # Total data samples in time series / number of splits
  number.samples <- ncol(EEG.data) / num.split 
  # Make a list of windowed matrices
  # Loop for each split
  for (i in 1:num.split) {  # Create a list of matrices
    EEG.window[[i]] <- EEG.data[,((i-1)*number.samples+1):
                                         (i*number.samples)] 
  } 
  # Return
  EEG.window
}

######################################################################################

# Function to window a multichannel EEG time series (overlapping)
overlap.window.matrix <- function (EEG.data, num.split){
  # Windows a single EEG time series matrix into a list of windowed matrices.
  # The windows are overlapping by 50%
  #
  # Args:
  #   EEG.data: Multichannel EEG data in a matrix; each row is an EEG channel
  #   num.split: The number of splits to perform
  #   overlap: The percentage overlap
  # Returns:
  #   EEG.window: List of EEG matrices.
  #   The no. of elements in the list equals (num.split * 2) - 1
  #   The first element contains data from 0 to windowsize (seconds). The second
  #   element contains data from windowsize to 2*windowsize (and so on). 
  
  # Initialise a list
  EEG.window <- list()
  
  # Calculate the total samples for each windowed matrix
  # Total data samples in time series / number of splits
  number.samples <- ncol(EEG.data) / num.split
  
  #Calculate total splits (50% overlap is 2N-1)
  total.splits <- num.split*2 - 1
  
  # Make a list of windowed overlapping matrices
  # Loop for each split
  for (i in 1:total.splits) {  # Create a list of matrices
    EEG.window[[i]] <- EEG.data[,((i-1)*number.samples/2+1):
                                         ((i-1)*number.samples/2 + number.samples)] 
  } 
  # Return
  EEG.window
}

######################################################################################

# Function to get a standard number of EEG channels
standard.EEG.data <- function (EEG.data, chan){
  # Standardise the number of channels to 16. Some files have 15 or 24 channels.
  #
  # Args:
  #   EEG.data: Multichannel EEG data in a matrix; each row is an EEG channel
  #   chan: The number of EEG channels in the file
  #   
  # Returns:
  #   EEG.data: Multichannel EEG data in a matrix with 16 channels total
  
  # For the case of 15 channels, insert a row based on the mean of the other ch.
  if (chan == 15){
    EEG.data <- rbind(EEG.data, colMeans(EEG.data))
  
  # In the case of 24 channels, randomly select 16    
  } else if (chan == 24){
    set.seed(1256)  # For reproducability
    EEG.data <- EEG.data[sample(nrow(EEG.data), 16), ]
  }

  # Return
  EEG.data
}

######################################################################################

# Function to calculate spectral FFT based features
feature.FFT <- function (EEG.channel, prefix, frequency){ 
  # Calculates FFT based features for a single windowed EEG channel
  #
  # Args:
  #   EEG.channel: EEG channel containing times series data
  #   prefix: label for identifying channel number
  #   rate: sample rate
  # Returns:
  #   feature.vector: extracted features
  
  # Compute the FFT of the time series
  X.k <- fft(EEG.channel)
  # Take the absolute value to get the Real component
  X.k <- abs(X.k)
  # Get length of series
  N <- length(EEG.channel)
  # Translate from FFT index to frequency
  df <- frequency/N
  sample.index <- 2:N-1
  
  # Creates a vector of frequency values
  freq <- sample.index*df

  # Drop the first element - represents DC component
  X.k <- X.k[2:N]
  # To data frame
  X.k <- data.frame(X.k)
  # Append frequency values
  X.k$Freq <- freq
  # Keep data points less than Nyquist frequency (sample frequency/2)
  # Values above this are negative frequencies
  X.k <- X.k[X.k$Freq < frequency/2,]
  
  # Calculate power spectral density
  X.k$X.k <- X.k$X.k^2
  
  # Filter for specific bands and get power per band
  band.delta.power <- round(sum(X.k[X.k$Freq >= 0.1 & X.k$Freq < 4,]),0)
  band.theta.power <- round(sum(X.k[X.k$Freq >= 4 & X.k$Freq < 8,]),0)
  band.alpha.power <- round(sum(X.k[X.k$Freq >= 8 & X.k$Freq < 12,]),0)
  band.beta.power <- round(sum(X.k[X.k$Freq >= 12 & X.k$Freq <= 30,]),0)
  band.gammalow.power <- round(sum(X.k[X.k$Freq >= 30 & X.k$Freq <= 70,]),0)
  band.gammahigh.power <- round(sum(X.k[X.k$Freq >= 70 & X.k$Freq <= 180,]),0)
  band.total.power <- round(sum(X.k[X.k$Freq <= 200,]),0)

  # Relative power per band
  rel.band.delta.power <- band.delta.power/band.total.power*100
  rel.band.theta.power <- band.theta.power/band.total.power*100
  rel.band.alpha.power <- band.alpha.power/band.total.power*100
  rel.band.beta.power <- band.beta.power/band.total.power*100
  rel.band.gammalow.power <- band.gammalow.power/band.total.power*100
  rel.band.gammahigh.power <- band.gammahigh.power/band.total.power*100

  # Get frequency with greatest power
  maximum.freq.power <- X.k$Freq[[which.max(X.k[,1])]]

  # Append values to feature vector
  feature.vector[paste0(prefix,"_Delta")] <- band.delta.power
  feature.vector[paste0(prefix,"_Theta")] <- band.theta.power
  feature.vector[paste0(prefix,"_Alpha")] <- band.alpha.power
  feature.vector[paste0(prefix,"_Beta")] <- band.beta.power
  feature.vector[paste0(prefix,"_GammaLow")] <- band.gammalow.power
  feature.vector[paste0(prefix,"_GammaHigh")] <- band.gammahigh.power
  feature.vector[paste0(prefix,"_Total_Power")] <- band.total.power
  feature.vector[paste0(prefix,"_Delta_Rel")] <- rel.band.delta.power
  feature.vector[paste0(prefix,"_Theta_Rel")] <- rel.band.theta.power
  feature.vector[paste0(prefix,"_Alpha_Rel")] <- rel.band.alpha.power
  feature.vector[paste0(prefix,"_Beta_Rel")] <- rel.band.beta.power
  feature.vector[paste0(prefix,"_GammaLow_Rel")] <- rel.band.gammalow.power
  feature.vector[paste0(prefix,"_GammaHigh_Rel")] <- rel.band.gammahigh.power
  feature.vector[paste0(prefix,"_Max_Freq")] <- maximum.freq.power
  
  # Return
  feature.vector
}

######################################################################################

# Loop for all folders with patient data
for (folder in folder.list) {

  # Create log file and log start time
  setwd(features.dir)
  logFile = paste0(folder, "_feature_extraction.txt")
  line <- paste0("Starting at ", Sys.time())
  write(line, file = logFile, append = TRUE)
  
  # Set working directory for data files
  patient.dir <- paste0(data.dir, folder)
  setwd(patient.dir)
  
  # Initialise matrix for feature vectors
  feature.vector.matrix  <- NULL
  
  # Get list of files for processing
  list.of.files <- dir(patient.dir, "*.mat")
 
  # If files are to be skipped, if the number of channels != 16
  if (skip.files == 1){
    
    # Load metadata for data files
    load(paste0(data.dir,"metadata.rda"))
    # Filter out list of files for the patient 
    patient.files <- meta.data.results[grep(folder, meta.data.results$filename),]
    # Get files that have 16 channels
    list.of.files <- patient.files[patient.files$channels == 16,]
    # Coerce to list
    list.of.files <- as.list(list.of.files$filename)
  }
  
  # Loop for all files in the patient folder
  # Proceed if list of files is not empty
  if (length(list.of.files) != 0 ){
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
      frequency <- round(EEG.file[["frequency"]], 0)
      # Get number of channels
      chan <- EEG.file[["channel"]]

      # If all files are to be included then move 15 to 16 channels
      # and reduce 24 to 16 channels
      if (skip.files == 0){
      # Function to get a total of 16 channels
      EEG.data <- standard.EEG.data (EEG.data, chan)
      }
      
      # Window the time series data
      # Calculate the required number of splits
      num.split <- seconds/windowsize  # The total length in seconds / size of window
      # Create a set of windowed data matrices
      # Splits matrix into a list of matrices, each 1/nsplit of the original in size
      if (overlap == 1){  # Overlapping windows
        EEG.window <- overlap.window.matrix (EEG.data, num.split)   
        num.split <- 2*num.split-1
      } else {  # Non overlapping windows
        EEG.window <- window.matrix (EEG.data, num.split)  
      }
  
      # Loop for all slices in the windowed data
      for (slices in 1:num.split) { # Step through the individual smaller matrices)
      
        # Get a slice from the windowed data
        EEG.slice <- EEG.window[[slices]]
  
        # Initialise a feature vector for the slice
        feature.vector <- c()
        
        # Loop for all channels in the EEG slice
        for (channels in 1:16) {
          prefix <- paste0("Chan_",channels) # Make a prefix with channel name  
          EEG.channel <- EEG.slice[channels,]  # Get row from matrix
          
          # Take the 1st differentiation function of the time series
          EEG.channel <- diff(EEG.channel)
          
          if (make.stat == 1){
            # Get summary statistics and append to feature vector
            feature.vector <- feature.statistic(EEG.channel, prefix)
          }
          if (make.fft == 1){
            # Get FFT statistics and append to feature vector
            feature.vector <- feature.FFT(EEG.channel, prefix, frequency)
          }
        }
        
        # Add in ID
        feature.vector['ID'] <- paste0(filename, "_", slices)
        # Add in class label
        if(grepl("inter", feature.vector['ID'])){
          feature.vector['CLASS'] <- "Interictal"
        } else {
          feature.vector['CLASS'] <- "Preictal"
        }
        
        # Make matrix
        if(is.null(feature.vector.matrix)) {  # Copy first row if null
          feature.vector.matrix <- feature.vector  
        } else {  # Else rowbind to existing object
          feature.vector.matrix <- rbind(feature.vector.matrix, feature.vector) 
        }
      }  # End loop for slices
    }  # End loop for files
  } 
  
  # Drop rownames from results
  rownames(feature.vector.matrix) <- c()
  
  #Convert to data frame
  feature.vector.matrix <- as.data.frame(feature.vector.matrix, stringsAsFactors = FALSE)
  
  # Number of columns
  N <- ncol(feature.vector.matrix)
  # Data columns to be coerced to numeric
  # The last column is not a data variable
  cols.num <- seq(1,N-2,1)
  # Coerce data columns to numeric
  feature.vector.matrix[cols.num] <- sapply(feature.vector.matrix[cols.num],as.numeric)
  
  # Split out data columns
  header <- feature.vector.matrix[,seq(N-1,N,1)] # Last two columns
  
  # Scale data columns to range 0 to 1
  dat <- data.frame(lapply(feature.vector.matrix[,1:(N-2)], function(x)(x-min(x))/(max(x)-min(x))))
  
  # Round all columns to 3 decimal places
  dat <-round(dat,3)
  
  # Bind back to header
  feature.vector.matrix <- cbind(header, dat)
  
  # Save results as .rda object
  # Set save location
  setwd(features.dir)
  
  # Save results for each patient to a .rda file
  save(feature.vector.matrix, file = paste0(folder,"_","features.rda"))
  
  # Update log
  line <- paste0("Ending at ", Sys.time())
  write(line, file = logFile, append = TRUE)
  
}  # End loop for folder
   
######################################################################################