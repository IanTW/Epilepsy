partition.folder = paste0("c:/users/ian_wa/desktop", '/Partitions') # Dev only
features.dir <- paste0("c:/users/ian_wa/dropbox", '/Features') # Dev only
data.dir <- "c:/users/ian_wa/dropbox"
#features.dir <- paste0(portable, '/Features')
win.type <- c("60-00", "30-50")      #  "60-50", "60-00", "30-50", "30-00"
feature.folder <- c("Stat", "FFT", "Both")
feature.selection <- c("Non", "Rfe.rda", "Lvq.rda")
prefix = 27
split = 0.7

# Function to create normal data partition
normal.partition <- function (filename, prefix, win, feature, sel) {
  # Reads in a feature vector file 
  #
  # Args:
  #   prefix: File prefix for saving
  #   filename: The name of the input feature vector
  #   win: The type of window and overlap size
  #   feature: The type of feature (Stat, FFT, or both)
  #   sel: The type of feature selection method (LVQ, RFE)
  # 
  # Returns:
  #   Saves a test and training partition
  
  cat("Starting Normal Partition\n")
  
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
    #cat(files," ", i , "\n")
    matchstring <- paste0("^",files,"_" )
    train.partition <- rbind(train.partition, combined.feature[grep(matchstring, combined.feature$ID),])
    i = i + 1
  }
  
  # Make testing data
  test.partition <-c()
  i <- 1  # For console output
  
  for (files in testing.files){
    #cat(files," ", i , "\n")
    matchstring <- paste0("^",files,"_" )
    test.partition <- rbind(test.partition, combined.feature[grep(matchstring, combined.feature$ID),])
    i = i + 1  
  }
  
  # Drop ID
  train.partition$ID <- NULL
  
  # Make double digit prefix
  if (prefix <= 9){
    prefix = paste(0, prefix, sep="")
  }
  
  # Set up labels for files
  labpre <- prefix
  labty <- win  # Type of window
  labfe <- feature  # Feature
  labse <- substr(sel,1,3)  # Selection method
  labtr <- "Normal_Train"
  labte <- "Normal_Test"
  
  # Save to file
  setwd("c:/users/ian_wa/Desktop/partitions/train") # dev only
  # setwd(paste0(portable, "Partitions"))
  save(train.partition, file = paste(prefix,
                                      labty,
                                      labfe,
                                      labse,
                                      labtr,
                                      ".rda",
                                      sep = "_"))
  setwd("c:/users/ian_wa/Desktop/partitions/test") # dev only
  save(test.partition, file = paste(prefix,
                                    labty,
                                    labfe,
                                    labse,
                                    labte,
                                    ".rda",
                                    sep = "_"))

}

# Function to create increased minority data partition
increase.partition <- function (filename, prefix, win, feature, sel) {
  # Reads in a feature vector file 
  #
  # Args:
  #   prefix: File prefix for saving
  #   filename: The name of the input feature vector
  #   win: The type of window and overlap size
  #   feature: The type of feature (Stat, FFT, or both)
  #   sel: The type of feature selection method (LVQ, RFE)
  # 
  # Returns:
  #   Saves and returns a test and training partition

  cat("Starting Increased Partition\n")
  
  # Load metadata file
  setwd(data.dir)
  
  load("metadata.rda")
  cat("Loaded metadata\n")
  
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
    #cat(files," ", i , "\n")
    matchstring <- paste0("^",files,"_" )
    train.partition <- rbind(train.partition, combined.feature[grep(matchstring, combined.feature$ID),])
    i = i + 1
  }
  
  # Make testing data
  test.partition <-c()
  i <- 1  # For console output
  
  for (files in testing.files){
    #cat(files," ", i , "\n")
    matchstring <- paste0("^",files,"_" )
    test.partition <- rbind(test.partition, combined.feature[grep(matchstring, combined.feature$ID),])
    i = i + 1  
  }
  
  # Drop ID
  train.partition$ID <- NULL
  
  # SMOTE
  train.partition$CLASS <- as.factor(train.partition$CLASS)
  train.partition <- SMOTE(CLASS ~ ., train.partition, perc.over = 1250, perc.under=108)
  
  #Check distribution
  #table(train.partition$CLASS)
  
  # Make double digit prefix
  if (prefix <= 9){
    prefix = paste(0, prefix, sep="")
  }
  
  # Set up labels for files
  labpre <- prefix
  labty <- win  # Type of window
  labfe <- feature  # Feature
  labse <- substr(sel,1,3)  # Selection method
  labtr <- "Increased_Train"
  labte <- "Increased_Test"
  
  # Save to file
  setwd("c:/users/ian_wa/Desktop/partitions/train") # dev only
  # setwd(paste0(portable, "Partitions"))
  save(train.partition, file = paste(prefix,
                                      labty,
                                      labfe,
                                      labse,
                                      labtr,
                                      ".rda",
                                      sep = "_"))
  setwd("c:/users/ian_wa/Desktop/partitions/test") # dev only
  save(test.partition, file = paste(prefix,
                                    labty,
                                    labfe,
                                    labse,
                                    labte,
                                    ".rda",
                                    sep = "_"))
}

# Function to create reduced majority data partition
reduce.partition <- function (filename, prefix, win, feature, sel) {
  # Reads in a feature vector file 
  #
  # Args:
  #   prefix: File prefix for saving
  #   filename: The name of the input feature vector
  #   win: The type of window and overlap size
  #   feature: The type of feature (Stat, FFT, or both)
  #   sel: The type of feature selection method (LVQ, RFE)
  # 
  # Returns:
  #   Saves and returns a test and training partition
  
  cat("Starting Reduced Partition\n")
  
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
  
  # Set random seed for reproducability
  set.seed(379)
  
  # Reduce number of training interictal files to match training preictal files
  train.interictal.indices <- sample(train.interictal.indices,n.train.preictal)
  train.interictal.files <- interictal.files[train.interictal.indices,]
  train.interictal.files <- train.interictal.files$filename # Training file names

  # Combined list of training files
  training.files <- c(train.preictal.files, train.interictal.files)
  # Combined list of testing files
  testing.files <- c(test.preictal.files, test.interictal.files)

  # Training data
  train.partition <-c()
  i <- 1
  
  for (files in training.files){
    #cat(files," ", i , "\n")
    matchstring <- paste0("^",files,"_" )
    train.partition <- rbind(train.partition, combined.feature[grep(matchstring, combined.feature$ID),])
    i = i + 1
  }
  
  # Testing data
  test.partition <-c()
  i <- 1
  
  for (files in testing.files){
    #cat(files," ", i , "\n")
    matchstring <- paste0("^",files,"_" )
    test.partition <- rbind(test.partition, combined.feature[grep(matchstring, combined.feature$ID),])
    i = i + 1  
  }
  
  # Make double digit prefix
  if (prefix <= 9){
    prefix = paste(0, prefix, sep="")
  }
  
  # Drop ID
  train.partition$ID <- NULL
  
  # Set up labels for files
  labpre <- prefix
  labty <- win  # Type of window
  labfe <- feature  # Feature
  labse <- substr(sel,1,3)  # Selection method
  labtr <- "Reduced_Train"
  labte <- "Reduced_Test"
  
  # Save to file
  setwd("c:/users/ian_wa/Desktop/partitions/train") # dev only
  # setwd(paste0(portable, "Partitions"))
  save(train.partition, file = paste(prefix,
                                      labty,
                                      labfe,
                                      labse,
                                      labtr,
                                      ".rda",
                                      sep = "_"))
  setwd("c:/users/ian_wa/Desktop/partitions/test") # dev only
  save(test.partition, file = paste(prefix,
                                    labty,
                                    labfe,
                                    labse,
                                    labte,
                                    ".rda",
                                    sep = "_"))

}

for (win in win.type){  # Loop for all window types
  for (feature in feature.folder){  # Loop for all feature types
    for (sel in feature.selection){  # Loop for all selection methods
      # Set directory to location for feature vector
      setwd(paste0(features.dir, "/", win,"/", feature))
      cat("Working on ", win," ",feature," ",sel,"\n")
      # Open feature vector
      load("Combined_features.rda")
      
      # Subset with selection method
      if (sel == "Non"){
      cat("No feature selection\n")  
      } else {
      # Open selection method
      load(sel)
      cat("Subsetting with",sel,"\n")  
      combined.feature <- combined.feature[,cols] 
      }
      
      # Set directory for partitions
      setwd(partition.folder)
      # Make normal partition
      normal.partition(combined.feature, prefix, win, feature, sel)
      prefix <- prefix + 1
  
      # Make increased minority class
      increase.partition(combined.feature, prefix, win, feature, sel)
      prefix <- prefix + 1
      
      # Make reduced majority class
      reduce.partition(combined.feature, prefix, win, feature, sel)
      prefix <- prefix + 1
    }
  }
}  

######################################################################################

