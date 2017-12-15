#Plot series
library(eegkit)

# From a EEG file
voltage <- EEG.channel

fs <- length(EEG.channel)/600 # 60 for a slice or 600 for whole file
time <- seq(0,600-1/fs, 1/fs)

par(mfrow=c(2,2))

plot(time,voltage, "line")

plot(time,voltage, "line")

plot(time,voltage, "line")

plot(time,voltage, "line")

# Visualisation for nnet
# install.packages("devtools")
# library(devtools)
# source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
# plot.nnet(outdata)

#From eegkit - doesnt work
# data(eegdata)
# idx <- which(eegdata$group=="c")
# eegdata <- eegdata[idx,]
# # get average
# eegmean <- tapply(eegdata$voltage,list(eegdata$time,eegdata$channel),mean)
# eegse <- tapply(eegdata$voltage,list(eegdata$time,eegdata$channel),sd)/sqrt(50)
# # plot time course for all electrodes
# quartz(height=15,width=15)
# tseq <- seq(0,1000,length.out=256)
# eegtimemc(tseq,eegmean,colnames(eegmean),ylim=c(-11,14),voltSE=eegse)
