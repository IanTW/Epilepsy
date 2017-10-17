#Plot series
library(eegkit)
voltage <- EEG.channel
fs <- length(EEG.channel)/60
time <- seq(0,60-1/fs, 1/fs)
