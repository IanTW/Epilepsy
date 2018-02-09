#Here’s a R function for plotting trajectories given a fourier series:

plot.fourier <- function(fourier.series, f.0, ts) {
  w <- 2*pi*f.0
  trajectory <- sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
}


f <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

#plot.frequency.spectrum() plot a frequency spectrum of a given XkXk
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  # 0 to N-1 and modulus of complex (gives magnitude I think)
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  # Double the magnitude prob. because of pos and neg values
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

#Two examples of sine waves:
xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
#which can be linearly combined into a complex wave:
wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)


#And the plotting of equation f(t)=0.5×sin(3wt)+0.25×sin(10wt)f(t)=0.5×sin(3wt)+0.25×sin(10wt):

acq.freq <- 100                    # data acquisition frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
f.0      <- 1/time                 # fundamental frequency (Hz)

dc.component       <- 0
component.freqs    <- c(3,10)      # frequency of signal components (Hz)
component.delay    <- c(0,0)       # delay of signal components (radians)
component.strength <- c(.5,.25)    # strength of signal components

plot.fourier(f,f.0,ts)   

#Here are two egs of use, a stationary and an increasing trajectory:

library(stats)
fft(c(1,1,1,1)) / 4  # to normalize
## [1] 1+0i 0+0i 0+0i 0+0i
fft(1:4) / 4  
## [1]  2.5+0.0i -0.5+0.5i -0.5+0.0i -0.5-0.5i



#Let’s try with one eg. We’ll make a trajectory given the following complex wave

#f(t)=2+0.75×sin(3wt)+0.25×sin(7wt)+0.5×sin(10wt)
#f(t)=2+0.75×sin(3wt)+0.25×sin(7wt)+0.5×sin(10wt)

acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 1                     # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time

dc.component <- 0
component.freqs <- c(3,5,7)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1) # strength of signal components

plot.fourier(f,f.0,ts=ts)


#Let’s assume that we don’t know the functional form of trajectory, we only have its contents, the period and the sampling time points:

w <- 2*pi*f.0
trajectory <- sapply(ts, function(t) f(t,w))
head(trajectory,n=30)
##  [1] 1.000000 1.162132 1.323161 1.481997 1.637568 1.788836 1.934801
##  [8] 2.074515 2.207089 2.331703 2.447610 2.554146 2.650736 2.736896
## [15] 2.812242 2.876489 2.929454 2.971057 3.001324 3.020382 3.028458
## [22] 3.025877 3.013056 2.990502 2.958803 2.918623 2.870694 2.815807
## [29] 2.754805 2.688575
#So, given that trajectory we can find where the frequency peaks are:

X.k <- fft(trajectory)                   # find all harmonics with fft()
plot.frequency.spectrum(X.k, xlimits=c(0,50))

#And if we only had the frequency peaks we could rebuild the signal:

x.n <- get.trajectory(X.k,ts,acq.freq) / acq.freq  # TODO: why the scaling?
plot(ts,x.n, type="l"); abline(h=0,lty=3)
points(ts,trajectory,col="red",type="l") # compare with original

#########################################################################
#Other example

acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 60                     # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time

dc.component <- 0
component.freqs <- c(50)        # frequency of signal components (Hz)
component.delay <- c(0)         # delay of signal components (radians)
component.strength <- c(1) # strength of signal components

plot.fourier(f,f.0,ts=ts)


#Let’s assume that we don’t know the functional form of trajectory, we only have its contents, the period and the sampling time points:

w <- 2*pi*f.0
trajectory <- sapply(ts, function(t) f(t,w))
X.k <- fft(trajectory)
plot.frequency.spectrum(X.k, xlimits=c(0,6000))


#The frequency scales are wrong though and show the FFT indexes N
#########################################################################
# Using periodogram

#manually make a 1Hz sine wave 
ts2 <- seq(0,1,1/100)
ts2 <- data.frame(ts2)
ts2$sin <- sin(2*pi*ts2$ts2)
plot(ts2$ts2, ts2$sin)
X.k2 <- fft(ts2$sin)

plot.frequency.spectrum(X.k2, xlimits=c(0,50))


#Make a sine wave with provided code
acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 1                     # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time
dc.component <- 0
component.freqs <- c(1)        # frequency of signal components (Hz)
component.delay <- c(0)         # delay of signal components (radians)
component.strength <- c(1)
w <- 2*pi*f.0
trajectory <- sapply(ts, function(t) f(t,w))

plot(ts, trajectory)
X.k <- fft(trajectory)
plot.frequency.spectrum(X.k, xlimits=c(0,50))

#The plots are the same

#########################
#Using TSA library
periodogram(trajectory)
periodogram(ts2$sin)
#Results same


data(eeg)
periodogram(eeg, xlimits = c(0,0.1))
plot(eeg)

a <- periodogram(eeg, plot = FALSE)
#Get spec
b <- a[2]


load("~/Epilepsy/Sample Data/SampleChannel1.rda")
periodogram(EEG.channel)

a <- periodogram(EEG.channel, plot = FALSE)
b <- a[2]
#limit axis
periodogram(eeg, xlim = c(0,0.1))

#Is the periodogram calculating the power?
#Compare to basic way of doing it

X.k3 <- fft(EEG.channel)
#Take abs value to get Real
X.k3 <- abs(X.k3)
#Calculate frequency resolution Delta F
fs <- 400
N = length(EEG.channel)
deltaf <- fs/N # Length is N

sampleindex <- 1:N

#axis converted to freq
f = sampleindex*deltaf

#plot with fft bin/index
plot(sampleindex, X.k3,'l')

#plot with freq - note the midpoint is Fs/2
plot(f,X.k3, 'l')

#Limit to same range as periodogram
plot(sampleindex, X.k3,'l',xlim=c(0,N/2))

#plot periodogram and compare
periodogram(EEG.channel) # periodogram has much higher values on y

#Double
X.k3 <- 2*X.k3

# Periodogram seems to have a total of four times the value