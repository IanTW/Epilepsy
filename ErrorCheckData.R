# Used for integrity checking the files
require (R.matlab)

# Folder structures with data files
patient.names = c('Dog_1','Dog_2','Dog_3','Dog_4','Dog_5','Patient_1','Patient_2')

# Loop for all patients' folders
for (folder in patient.names){   
  
  # Set working directory here
  datadir <- paste0("G:/OneDrive/DIT/00. Dissertation/Data/", folder)
  setwd(datadir)
  
  # Create log file - DONT FORGET TO CLEAR IT OUT FIRST IF YOU ARE RUNNING A 2nd TIME
  logFile = paste0(folder, "_integtrity_check.txt")
  
  list.of.files <- dir(datadir, "*.mat")
  
  for (filename in list.of.files){
    tryCatch({
      a <- readMat(filename)
      cat("Checking", filename, "\n")
      cat(paste0("Good ", filename), file = logFile, append = TRUE, sep = "\n")
      }, error=function(e){
        cat("ERROR :", conditionMessage(e), filename, "\n")
        cat(paste0("BAD OH SO BAD OH NO ", filename), file = logFile, append = TRUE, sep = "\n")
        })
  }
}

close(fileConn)