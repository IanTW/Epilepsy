####################
###  Takes almost 20 hours for Subject 1 train files
###  RAM 8 gb , CPU: Intel i7 2.40 GHz
####################

libs = c("R.matlab","dplyr","zoo")

sapply(libs,require,character.only = T)

##setwd("path/to/data")

# entropy = function(x){
#     probs = table(x)/length(x)
#     -sum(sapply(x,function(x_i) 
#         probs[[as.character(x_i)]]*log2(probs[[as.character(x_i)]])))
# }

deriveSpec = function(ts,band){
    #----- AVG spectrum for this band
    ts = spectrum(ts,plot=FALSE)
    EEGspectrum = data.frame(ts$spec,ts$freq)
    names(EEGspectrum) = c("Spec","Freq")
    as.numeric(EEGspectrum %>% 
                   filter(Freq>band[1],Freq<band[2]) %>% 
                   summarise(mean = mean(Spec), var = var(Spec)))
}


spectralFeatures = function(df){
    #------ Mean and variance of Spectrum in five popular bands
    bands = list( c(0.5,3.5)/nyquist,
                 c(4,8)/nyquist,
                 c(8.5,12)/nyquist,
                 c(12.5,16)/nyquist,
                 c(16.5,30)/nyquist,
                 c(30.5,60)/nyquist)
    
    meanSpectrum = c()
    varSpectrum = c()
    for( band in bands){
        spectrum_summary = data.frame(sapply(df,deriveSpec,band=band))
        meanSpectrum = c(meanSpectrum,spectrum_summary[1,])
        varSpectrum = c(varSpectrum,spectrum_summary[2,])
    }
    c(meanSpectrum,varSpectrum)
}

rolling_features = function(ts){
    #------ Extraction of simple features in time windows
    c(mean(ts),
      sum(ts),
      var(ts),
      min(ts),
      max(ts))
  #sum(abs(fft(ts)))
  #entropy(ts)
}


#------ 400 Hz sampling
window = 400 * 60
#nyquist = half max sample
nyquist = 200

train_files = dir(path="C:/Users/ian_wa/Documents/Epilepsy/Sample Data/Dog_1",full.names=T)
for(idx in seq_along(train_files)){
    train_file = train_files[idx]
    print(idx)
    
    #read in mat
    rawData = readMat(train_file)  
    #get electrode data
    df = as.data.frame(rawData[[1]][[1]])
    
    #Class lablel
    class = as.numeric(gsub(".mat","",strsplit(train_file,"_")[[1]][4]))

    #------- Connectivity Features, Spectral Features
    features = c(class,eigen(cov(df))$values,spectralFeatures(df))
    
    #------- Time windows features
    for(i in 1:ncol(df)){
        features = c(features, matrix(rollapply(df[,i], width = window, by = window/2, 
                                FUN = rolling_features, align = "left"),nrow=1))
    }
    
    
    write(paste(features, collapse=","),file="train_first.csv",append=TRUE)
}



