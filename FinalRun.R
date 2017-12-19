partition.folder = paste0("c:/users/ian/desktop", '/Partitions') # Dev only
features.dir <- paste0("c:/users/ian/desktop", '/Features/') # Dev only
features.dir <- paste0(portable, '/Features')
feature.type <- c("60s-50p", "60s-00p", "30s-50p", "30s-00p")
feature.folder <- c("Stat", "FFT", "Both")
feature.selection <- c("LVQ.rda", "RFE.rda")

#Dev only
feats = feature.type[1]
folder = feature.folder[1]
featsel = feature.selection[1]
prefix = 0


for (feats in feature.type){
  for (folder in feature.folder){ 
    for (featsel in feature.selection){
      # Set directory to location for feature vector
      setwd(paste0(features.dir, "/", feats,"/", folder))
      cat("Working on ", feats," ",folder," ",featsel,"\n")
      # Open feature vector
      load("Combined_features.rda")
      # Open selection method
      load(featsel)
      cat("Subsetting with",featsel,"\n")
      
      # Feature selection
      cols <- c(1,2,feature.select)
      combined.feature <- combined.feature[,cols]
      
      # Set directory for partitions
      setwd(partition.folder)
      # Create normal partition
      normal.partition(prefix,combined.feature,feats,folder,featsel)
      prefix = prefix + 1
    }
  }
}  
  
# Function to create normal data partition
normal.partition <- function (prefix,filename,feats,feat,featsel) {
  # Reads in a feature vector file 
  #
  # Args:
  #   prefix: File prefix for saving
  #   filename: The name of the input feature vector
  #   feats: The type of feature (window and overlap size)
  #   folder: The type of feature (Stat, FFT, or both)
  #   featsel: The type of feature selection method (LVQ, RFE)
  # 
  # Returns:
  #   Saves a test and training partition
  
  # Load metadata file
  #setwd(data.dir)
  setwd("c:/users/ian/desktop") # dev only
  
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
  
  # Set up labels for files
  labpre <- prefix
  labty <- feats  # Type
  labfe <- folder  # Feature
  labse <- substr(featsel,1,3)  # Selection method
  labtr <- "Normal_Train"
  labte <- "Normal_Test"
  
  # Save to file
  setwd("c:/users/ian/Desktop/partitions") # dev only
 # setwd(paste0(portable, "Partitions"))
  save(train.partition, file = paste0(prefix,
                                      labty,
                                      labfe,
                                      labse,
                                      labtr,
                                      ".rda",
                                      sep = "_"))
  save(test.partition, file = paste(prefix,
                                     labty,
                                     labfe,
                                     labse,
                                     labte,
        
                                     sep = "_"))
}

######################################################################################

