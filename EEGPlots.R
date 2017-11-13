#Plot series
library(eegkit)
voltage <- EEG.channel
fs <- length(EEG.channel)/60
time <- seq(0,60-1/fs, 1/fs)

# Visualisation for nnet
# install.packages("devtools")
# library(devtools)
# source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
# plot.nnet(outdata)

