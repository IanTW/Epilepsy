features.dir <- paste0(parent.dir, '/Features')
feature.win <- c("/60-50", "/30-50")
feature.type <- c("/Stat", "/FFT", "/Stat_plus_FFT")
feature.selection <- c("LVQ.rda", "RFE.rda")

for (folder in feature.win){
  for (subfolder in feature.type){ 
    setwd(paste0(features.dir,folder,subfolder))
      for (select.method in feature.selection){
        open file
        subset combined.feature
        
        for (sample.method in sampling) {
          
          
        }
        
        
      }
    
    open feature selection LVQ
      
      
      
    }  
  
  
  }
}