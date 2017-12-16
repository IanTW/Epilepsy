features.dir <- paste0(portable, '/Features')
feature.type <- c("/60s-50p") #, "/30s-50p")
feature.folder <- c("/Stat", "/FFT", "/Both")
feature.selection <- c("LVQ.rda", "RFE.rda")

for (feats in feature.type){
  for (folder in feature.folder){ 
    setwd(paste0(features.dir,feats,folder))
      for (select.method in feature.selection){
        load("Combined_features.rda")
        cat("Loaded features\n")
        load(select.method)
        cat("Loaded",select.method,"\n")
        
        #subset combined.feature
        
        #for (sample.method in sampling) {
          
          
        
        
        
      }
    
   # open feature selection LVQ
      
  }
      
    }  
  

