require(R.matlab)

parent.dir = getwd()
data.dir = "/Sample Data/"
mytype = "Dog_1"
myfile = "Dog_1_interictal_segment_0476.mat"
mi = 1
indata = mymat
f = 400 # for sample freq
t = 60 # for split time
i = 1 # for channel num
indata <- indata[1,] # for passing to gen_features_oneseries
pre = prename


#Code sequence

# 1) set variables
# 2) loop for all patients (set mytype)
#       loop for interictal files (set myfile)
#            read in .mat file 
#            call downsample function  
#            call the split function
#            loop for each window (set mi)
#                   call gen_features_onefile function
#                           do some stuff
#                           loop for each channel (set i)
#                                 do some stuff
#                                 call gen_features_oneseries function
#                                       do some stuff
#                                       calculate delta1 and delta2
#                                             call gen_features_onearray function
#            
#       loop for ictal files
#
#
#
#
#
#
#
#
#
#
#
#

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

# Down sampling to reduce data load and normalise features
down_sampling <- function (data, factor = 1) {
	#If freq\fixfreq < 1 then the sample rate is less than the target sample rate
  #If freq\fixfreq = 1 then the sample rate is the same
  if (factor <= 1) {
		return(data)
  }
  
  # Check the data frame is not empty
 	not_multi=is.null(nrow(data))
	# If TRUE proceed - Resampling based on the total number of ELEMENTS in matrix
 	# Suitable for a single electrode ?
	if (not_multi) {
	  # Length of data gives the number of elements in the matrix
	  # This is divided by the factor to REDUCE the number of samples
	  # Floor rounds this down
		newlen=floor(length(data)/factor)
		if (newlen > 1) {
			newdata=sapply(1:newlen, FUN=function(i, d, f) { mean(d[round((i-1)*f+1):round(i*f)]) },
				d=data, f=factor)
		}
	# Resampling based on the total number of samples per electrode
	} else {
	  # Columns of data gives the number of samples per electrode
	  # This is divided by the factor to REDUCE the number of samples
	  # Floor rounds this down
		newlen=floor(ncol(data)/factor)
		if (newlen > 1) {
			newdata=sapply(1:newlen, #For samples from 1 to new length
				FUN=function(i, d, f) {
					col_start=round((i-1)*factor+1);
					col_end=round(i*factor);
					if (col_end > col_start) {
						rowMeans(d[,col_start:col_end])
					} else {
						d[,col_start]
					}
				},
				d=data, f=factor)
		} else {
			newdata=data
		}
	}
 	#Return new data frame
	newdata
}

# Generate features for array?
gen_features_onearray <- function (indata, pre, freqs) {
  
  #Args:  indata is a time-series of data from one channel. Can be orginal, delta1 or delta2.
  #       pre is the channel name
  #       freqs is that weird ass sequence...
  
  

  feats[paste0(pre,"_","Sd")]=sd(abs(indata))
  a=summary(abs(indata))[sumindex]
  heads=paste0(pre, "Amp", "_",sumheads)
  feats[heads]=a
  feats[paste0(pre,"Amp_Sd")]=sd(abs(indata))
  freqs=freqs[1:length(indata)]
  #	cat(paste("In gen_features_onearray(), before fft size is:",paste(dim(indata),collapse=','),"\n"))
  
  # Init object
	feats=c()
	#Summary statistics from Summary() function
	sumheads=c('Min','1stQrt','Med','Mean','3rdQrt','Max')
	sumindex=c(4,6)
	#Set mean and max
	sumheads=sumheads[sumindex]
	
	#Get summary stats for MEAN and MAX
	a=summary(indata)[sumindex] # Gets the 4th and 6th elements from the summary data (mean and max)
	#Get Mean and Max and populate
	feats[paste0(pre,"_",sumheads)] = a
	
  #Get SD and populate
	feats[paste0(pre,"_","Sd")]=sd(indata)
	
	# Get absolute value for mean and max (calculates the mean Amplitude and max Amplitude)
	a=summary(abs(indata))[sumindex]
	feats[paste0(pre, "Amp", "_",sumheads)] = a
	
	#Calculate the SD of the amplitude
	feats[paste0(pre,"Amp_Sd")]=sd(abs(indata))
	
	#Calculated
	freqs=freqs[1:length(indata)]
#	cat(paste("In gen_features_onearray(), before fft size is:",paste(dim(indata),collapse=','),"\n"))
#	flush.console()
#	b=Sys.time()
	#FFT function follows
	
	if (dolog == 1) { # Set in early parameter setup
		myfft=log(abs(fft(indata))+1) #log of FFT - what is the difference FFT vs log FFT??
	} else {
		myfft=abs(fft(indata))
	}
	#Calculate mean and max of FFT series
	a=summary(myfft)[sumindex]
#	cat(paste("After fft, time used:",format(Sys.time()-b),"\n"));flush.console()
	heads=paste0(pre,"FFT_",sumheads)
	feats[heads]=a
	feats[paste0(pre,"FFT_","Sd")]=sd(myfft)
	feats[paste0(pre,"FFT_","MaxFreq")]=freqs[which.max(myfft)]
	if (addFFT == 1 || addFFT == 2) {
		if (addFFT == 2) {
			myfft=(myfft-mean(myfft))/sd(myfft)
		}
		myfft=myfft[1:round(FFTratio*length(myfft))]
		myfft=down_sampling(myfft, round(length(myfft)/FFTavglen))
		colnames=paste0(pre,"FFT_",sprintf("%02d",1:length(myfft)))
		feats[colnames]=myfft
	}
	feats
}

# Generate feature for a single windowed EEG channel
gen_features_oneseries <- function (indata, pre, freqs) {
  # Args: indata is a numeric sequence consisting of all the data points for a singel EEG channel 
  #       pre is channel name
  #       freqs is a numeric sequence 1/60 to 1/24000
  
  #init object
	feats=c()
	#convert to vector seems redundant - is.vector returns TRUE before this command
	indata=as.vector(indata)
	
	# Initialise numeric object
	delta1=rep(0,length(indata))
	
	# This is simply done by the R diff() function 
	# Calculates the difference between consecutive data points in indata d(n)-d(n-1)
	# for indata = (47, 41, 43, 53...) delta1 = (-6,2,10...)
	delta1[1:(length(indata)-1)]=indata[2:length(indata)]-indata[1:(length(indata)-1)]
	
	#This can be done by the diff() of the diff()
	# Calculates the difference between consecutive data points in delta1 d(n)-d(n-1)
	# for delta1 = (-6, 2, 10, -6 ...) delta2 = (8,8,-16...)
	delta2=rep(0,length(indata))
	delta2[1:(length(delta1)-2)]=delta1[2:(length(delta1)-1)]-delta1[1:(length(delta1)-2)]
  
	
	#Generate features for original time series (indata) and two delta series (delta1 and delta2)
	feats=c(feats, gen_features_onearray(indata, paste0(pre,""), freqs))
	feats=c(feats, gen_features_onearray(delta1, paste0(pre,"Del1"), freqs))
	feats=c(feats, gen_features_onearray(delta2, paste0(pre,"Del2"), freqs))
	feats
}

# Generate feature for file?
gen_features_onefile <- function (indata, f, t) {
  
  #  Args: indata = submatrix consisting of 10 seconds of data 
  #        f = sample frequency
  #        t = length of time of matrix (seconds/nsplit = 600/10 = 60 seconds)
  
  #Set timere
	b=Sys.time()
	#Initialise object
	feats=c()
	# Get number of channels (16)
	chans=nrow(indata)
	# Get number of data points (~24000)
	nc=ncol(indata)
	# Seems redundant - at no point is the number of data points precisely = sample rate x seconds
	# Will crap out every time
	if (round(f*t) != nc) {
		stop(paste("Stop in gen_features_onefile(), f*t=",f*t,"!= number of data points,",nc))
	}
	
	freqs=(1:nc)*1/t # Takes the column index and divides by 60. WTF for ???
	
	# Loop for each channel
	for (i in 1:chans) {
		prename=paste0("chan",i) # make a channel name  = 'chan1', 'chan2' etc
		# passes a single channel to the function, the channel name and that fucking odd 'freqs' numeric sequence
		# The single channel is passed as a numeric sequence; class(indata[1,]) = 'numeric'
		feats=c(feats,gen_features_oneseries (indata[i,], pre=prename, freqs)) 
	}
	fnames=names(feats)
#	cat(paste("Total number of features before global features",length(feats),format(Sys.time()-b),"\n"))
#	flush.console()
#	uniq_postfix=grep('^chan[0-9]*FFT_[0-9][0-9]',fnames,invert=T,value=T)
	uniq_postfix=fnames
	uniq_postfix=unique(gsub('^chan[0-9]*','',uniq_postfix))
	for (mypost in uniq_postfix) {
		pre=paste0("All",mypost)
		mynames=grep(paste0('^chan[0-9]*',mypost),fnames,value=T)
		mydata=feats[mynames]
		sumheads=c('Min','1stQrt','Med','Mean','3rdQrt','Max')
		sumindex=c(4,6)
		sumheads=sumheads[sumindex]
		a=summary(mydata)[sumindex]
		heads=paste0(pre, "_",sumheads)
		feats[heads]=a
		feats[paste0(pre,"_","Sd")]=sd(abs(mydata))
	}
#	cat(paste("Total number of features after AllCh features",length(feats),format(Sys.time()-b),"\n"))
#	flush.console()
	feats=c(feats,gen_features_oneseries (colMeans(indata), pre="chanAvg", freqs))
#	cat(paste("Total number of features after chAvg features",length(feats),format(Sys.time()-b),"\n"))
#	flush.console()
	mydata=indata
	for (del in 0:2) {
		if (del != 0) {
			mydata_tmp=matrix(rep(0,length(indata)), nrow=nrow(indata))
			mydata_tmp[,1:(ncol(mydata)-del)]=mydata[,2:(ncol(mydata)-del+1)]-mydata[,1:(ncol(mydata)-del)]
			mydata=mydata_tmp
			rm(mydata_tmp)
			pre=paste0("Del",del)
		} else {
			mydata=indata
			pre=""
		}
		cov=cov(t(mydata))
#		cat(paste("Size of data is:", paste(dim(mydata),collapse=','),
#			"Size of covar is:",paste(dim(cov),collapse=','),"\n"));flush.console()
		covarray=abs(cov[upper.tri(cov)])
		feats[paste0(pre,'CovMean')]=mean(covarray)
		feats[paste0(pre,'CovSd')]=sd(covarray)
		covarray=covarray[order(covarray,decreasing=T)]
		feats[paste0(pre,'CovMax')]=covarray[1]
		feats[paste0(pre,'Cov2nd')]=covarray[2]
		feats[paste0(pre,'Cov3rd')]=covarray[3]
#		cat(paste("Before FFT", format(Sys.time()-b),"\n"));flush.console()
		myfft=matrix(rep(0,length(mydata)),nrow=nrow(mydata))
		for (i in 1:chans) {
			if (dolog == 1) {
				myfft[i,]=log(abs(fft(mydata[i,]))+1)
			} else {
				myfft[i,]=abs(fft(mydata[i,]))
			}
		}
#		cat(paste("After FFT", format(Sys.time()-b),"\n"));flush.console()
		pre=paste0(pre,"FFT")
		cov=cov(t(myfft))
#		cat(paste("Size of FFT data is:", paste(dim(myfft),collapse=','),
#			"Size of FFT covar is:",paste(dim(cov),collapse=','),"\n"));flush.console()
		covarray=abs(cov[upper.tri(cov)])
		feats[paste0(pre,'CovMean')]=mean(covarray)
		feats[paste0(pre,'CovSd')]=sd(covarray)
		covarray=covarray[order(covarray,decreasing=T)]
		feats[paste0(pre,'CovMax')]=covarray[1]
		feats[paste0(pre,'Cov2nd')]=covarray[2]
		feats[paste0(pre,'Cov3rd')]=covarray[3]
#		gc()
#		cat(paste("Total number of features after Del",del,"covar features",length(feats),
#			format(Sys.time()-b),"\n"))
#		flush.console()
	}
#	cat(paste("Total number of features after covar features",length(feats),format(Sys.time()-b),"\n"))
	flush.console()
	feats
}

# Returns a list of values. Each element in the list is a window or segment of the data. There are x elements corresponding to y splits.
# If nsplit = 10 then there are 10 elements in the list
# Windowing function
split_mat <- function (mymat, nsplit) {  # this error function breaks every time - not sure what it achieves
#	if (ncol(mymat) %% nsplit != 0) {  # x %% y or x modulus y
#		stop(paste("In split_mat(), the nsplit",nsplit,"and column number",ncol(mymat),"do not match to even blocks."))
#	}
  # Set up a list
	retdata=list()
	# length for each sub-matrix
	mysize=ncol(mymat) / nsplit # Total colums / number of splits
	for (i in 1:nsplit) {
		retdata[[i]]=mymat[,((i-1)*mysize+1):(i*mysize)] # Create a list of matrices  - column 0:23976, then 23977:47952, 
	} 
	# return
	retdata
}

# Initialise project variables
{
  # Set target sample rate for resampling (Hz)
  fixfreq=200
  # Set window size 
   nsplit=10	# original clip is 10min=600sec, nsplit=10, new length=60sec, nsplit=20, newlength=30sec, nsplit=40 newlength=15sec
  
  # FFT parameters
  # Base for log calc?
  dolog=1
  addFFT=2
  FFTratio=0.2
  FFTavglen=24
  # Set counter to identify record number 
  inum=1

  # Endode data file categories into a numerical annotation
  types=c('Dog_1','Dog_2','Dog_3','Dog_4','Dog_5','Patient_1','Patient_2')
  typenums=c(Dog_1=1, Dog_2=2, Dog_3=3, Dog_4=4, Dog_5=5, Patient_1=6, Patient_2=7)
  
  # Set up logical array with 7000 elements. Why 7000? Are there 1000 files per patient?
  nv=rep(NA,length(types)*1000)
  # Create results data frame
  summary_df=data.frame(fname=nv, second=nv,freq=nv,ch=nv,label=nv,seq=nv,datalen=nv, time1=nv,time2=nv)
}

# Main code sequence
# Loop for all patients
for (mytype in types) {
  # Set timer for ??  Overwritten in the following loops. redundant??? 
  #begTime0=Sys.time()
  # Set path for data files, each patient in its own folder
  datadir=paste0(parent.dir,data.dir,mytype)

  # Get list of interictal files
  interfiles=dir(datadir, ".*_interictal_segment_.*.mat")
  # Get list of preictal files
  prefiles=dir(datadir, ".*_preictal_segment_.*.mat")

  # Initialise object for training data
  trainmat=NULL
  # Set timer for ??
  begTime=Sys.time()
  # Set counter for ??
  isub=1
  
  # Loop for all interictal files
  for (myfile in interfiles) {
    # Set timer for ??
  	begTime0=Sys.time()
  	# Get filename
  	filename=paste0(datadir,"/",myfile)
  	# Read in .mat file
  	retval=read_one_matfile(filename)
  	# Save the electrode data component 
  	orimat=retval[["mat"]]
  	# Get length of clip
  	seconds=retval[["seconds"]]
  	# Get sample frequency
  	freq=retval[["freq"]]
  	# Get electrode labels
  	labels=retval[["labels"]]
  	# Get sequence number
  	seq=retval[["seq"]]
  	
  	# Frankly, I think skip this code for resampling. This creates an object 'xmat' which appears
  	# nowhere else in the code? Maybe in another file?
  	# If sampling frequency is less than new sample rate then downsample 
  	# Factor is Q/P or Fs/Ftarget
  	if (fixfreq > 0 && fixfreq < freq) {
  		xmat=down_sampling(orimat, freq/fixfreq)
  		freq=fixfreq
  	}
    
  	# Code for splitting or windowing
  	# Try to find a built-in funtion for doing this
  	newmats=split_mat(orimat, nsplit)  # Splits matrix into a list of matrices, each 1/nsplit of the original is size
  	for (mi in 1:nsplit) { # Step through the individual smaller matrices
  		mymat=newmats[[mi]]  # Call the matrix by referencing the list 
  		f=gen_features_onefile(mymat, freq, seconds/nsplit) # Call the function to make features....
  		f['seq']=seq
  		f['flag']=0
  		f['id']=typenums[mytype]*100000+isub*100+mi
  		f['si']=mi
  		if(is.null(trainmat)) {
  			trainmat=f
  		} else {
  			trainmat=rbind(trainmat, f)
  		}
  	}
  	endTime=Sys.time()
  	time1=difftime(endTime, begTime0, units="secs")
  	time2=difftime(endTime, begTime, units="secs")
  	cat(paste(inum, myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
  		retval[["labels"]][1], retval[["seq"]],ncol(orimat), time1, time2,"\n"))
  	summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
  				retval[["labels"]][1], retval[["seq"]],ncol(orimat), time1, time2)
  	inum=inum+1
  	isub=isub+1
  	flush.console()
  }
  
  for (myfile in prefiles) {
  	begTime0=Sys.time()
  	filename=paste0(datadir,"/",myfile)
  	retval=read_one_matfile(filename)
  	orimat=retval[["mat"]]
  	seconds=retval[["seconds"]]
  	freq=retval[["freq"]]
  	labels=retval[["labels"]]
  	seq=retval[["seq"]]
  	if (fixfreq > 0 && fixfreq < freq) {
  		orimat=down_sampling(orimat, freq/fixfreq)
  		freq=fixfreq
  	}
  	newmats=split_mat(orimat, nsplit)
  	for (mi in 1:nsplit) {
  		mymat=newmats[[mi]]
  		f=gen_features_onefile(mymat, freq, seconds/nsplit)
  		f['seq']=seq
  		f['flag']=1
  		f['id']=typenums[mytype]*100000+isub*100+mi
  		f['si']=mi
  		if(is.null(trainmat)) {
  			trainmat=f
  		} else {
  			trainmat=rbind(trainmat, f)
  		}
  	}
  	endTime=Sys.time()
  	time1=difftime(endTime, begTime0, units="secs")
  	time2=difftime(endTime, begTime, units="secs")
  	cat(paste(inum, myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),retval[["labels"]][1],
  			retval[["seq"]],ncol(orimat),time1, time2,"\n"))
  	summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
  				retval[["labels"]][1], retval[["seq"]],ncol(orimat),time1, time2)
  	inum=inum+1
  	isub=isub+1
  	flush.console()
  }
  
  testmat=NULL
  for (myfile in testfiles) {
  	begTime0=Sys.time()
  	filename=paste0(datadir,"/",myfile)
  	retval=read_one_matfile(filename)
  	orimat=retval[["mat"]]
  	seconds=retval[["seconds"]]
  	freq=retval[["freq"]]
  	labels=retval[["labels"]]
  	seq=retval[["seq"]]
  	if (fixfreq > 0 && fixfreq < freq) {
  		orimat=down_sampling(orimat, freq/fixfreq)
  		freq=fixfreq
  	}
  	newmats=split_mat(orimat, nsplit)
  	for (mi in 1:nsplit) {
  		mymat=newmats[[mi]]
  		f=gen_features_onefile(mymat, freq, seconds/nsplit)
  		f['id']=typenums[mytype]*100000+isub*100+mi
  		f['si']=mi
  		if(is.null(testmat)) {
  			testmat=f
  		} else {
  			testmat=rbind(testmat, f)
  		}
  	}
  	endTime=Sys.time()
  	time1=difftime(endTime, begTime0, units="secs")
  	time2=difftime(endTime, begTime, units="secs")
  	cat(paste(inum, myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),retval[["labels"]][1],
  		retval[["seq"]],ncol(orimat),time1, time2,"\n"))
  	summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
  				retval[["labels"]][1], retval[["seq"]],ncol(orimat),time1, time2)
  	inum=inum+1
  	isub=isub+1
  	flush.console()
  }
  
  if (mytype %in% c("Dog_1","Dog_2","Dog_3","Dog_4") && do_holdout) {
  holdoutmat=NULL
  for (myfile in holdoutfiles) {
  	begTime0=Sys.time()
  	filename=paste0(holdoutdir,"/",myfile)
  	retval=read_one_matfile(filename)
  	orimat=retval[["mat"]]
  	seconds=retval[["seconds"]]
  	freq=retval[["freq"]]
  	labels=retval[["labels"]]
  	seq=retval[["seq"]]
  	if (fixfreq > 0 && fixfreq < freq) {
  		orimat=down_sampling(orimat, freq/fixfreq)
  		freq=fixfreq
  	}
  	newmats=split_mat(orimat, nsplit)
  	for (mi in 1:nsplit) {
  		mymat=newmats[[mi]]
  		f=gen_features_onefile(mymat, freq, seconds/nsplit)
  		f['id']=typenums[mytype]*100000+isub*100+mi
  		f['si']=mi
  		if(is.null(holdoutmat)) {
  			holdoutmat=f
  		} else {
  			holdoutmat=rbind(holdoutmat, f)
  		}
  	}
  	endTime=Sys.time()
  	time1=difftime(endTime, begTime0, units="secs")
  	time2=difftime(endTime, begTime, units="secs")
  	cat(paste(inum, myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),retval[["labels"]][1],
  		retval[["seq"]],ncol(orimat),time1, time2,"\n"))
  	summary_df[inum,]=c(myfile,retval[["seconds"]],retval[["freq"]],length(retval[["labels"]]),
  				retval[["labels"]][1], retval[["seq"]],ncol(orimat),time1, time2)
  	inum=inum+1
  	isub=isub+1
  	flush.console()
  }
  }
  # datadir=paste0("Data/",mytype)
  datadir=paste0("F:/Work/Kaggle/SeizurePrediction/Kaggle-SeizureDetection-Official/Data/",mytype)
  if (addFFT>=1) {
  	freq=paste0(freq,"FFT",addFFT,"-",FFTratio)
  }
  filename=paste0(datadir,"/",mytype,"Split",nsplit,"_Freq",freq,ifelse(dolog==1,paste0("Log",dolog),""),".RData")
  summary_df=summary_df[!is.na(summary_df$fname),]
  if (mytype %in% c("Dog_1","Dog_2","Dog_3","Dog_4") && do_holdout) {
      save(trainmat, testmat, holdoutmat, summary_df, file=filename, compress='bzip2')
  } else {
      save(trainmat, testmat, summary_df, file=filename, compress='bzip2')
  }
  endTime=Sys.time()
  cat(paste("Total time used for",mytype,":",format(endTime-begTime0),"\n"))
  cat(paste("File saved:",filename,"\n"))
  flush.console()
  }

summary_df=summary_df[!is.na(summary_df$fname),]

totaltime=(Sys.time()-begTime)
print(tail(summary_df, n=20))
print(totaltime)

