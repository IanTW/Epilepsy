#################################################

# Summarise metadata for all clips
# Loop for all data files

# Read in EEG file
read_one_matfile <- function (filename) {
  # Initiliase list for EEG data structure
  retval=list()
  # Read in the .mat file
  a=readMat(filename)
  # Data matrix - element 1 
  # 16 rows by ~24,000 columns = 16 by (200 x 600) = electrode by (sample rate X length seconds) 
  retval[["mat"]]=a[[1]][[1]]
  # EEG length - element 2
  retval[["seconds"]]=as.numeric(a[[1]][[2]])
  # Sample rate - element 3
  retval[["freq"]]=as.numeric(a[[1]][[3]])
  # EEG electrode labels 
  retval[["labels"]]=unlist(a[[1]][[4]])
  # EEG sequence number if available or set to -1
  if (length(a[[1]]) > 4) {
    retval[["seq"]]=as.numeric(a[[1]][[5]])
  } else {
    retval[["seq"]]=-1
  }
  #Remove object and garbage collection to reallocate memory
  rm(a)
  gc()
  #Return retval
  retval
}

# Get metadata for all files and summarise
metadata <- function (){
  
  require(R.matlab)
  
  inum=1
  
  # Endode data file categories into a numerical annotation
  types=c('Dog_1','Dog_2','Dog_3','Dog_4','Dog_5','Patient_1','Patient_2')
  typenums=c(Dog_1=1,Dog_2=2,Dog_3=3,Dog_4=4,Dog_5=5,Patient_1=6,Patient_2=7)
  
  # Set up logical array with 7000 elements; 1000 elements per patient
  # This would perhaps be the array of results eg 0 = non-seizure 1 = seizure.
  # Set up logical array with 7000 elements
  nv=rep(NA,length(types)*100)
  # Create results data frame
  summary_df=data.frame(fname=nv, second=nv, freq=nv, ch=nv,label=nv, seq=nv, datalen=nv)
  
  for (mytype in types) {
    # Set path for data files, each patient in its own folder
    datadir=paste0("C:/Users/ian_wa/Documents/Epilepsy/Data/",mytype)
    
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
