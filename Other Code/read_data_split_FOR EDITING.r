require(R.matlab)

getwd()
setwd('./Data')
filename = "Dog_1_interictal_segment_0476.mat"


# Read in EEG file
read_one_matfile <- function (filename) {
	# Set up list for EEG data structure
  retval=list()
  # Read in the .mat file
  a=readMat(filename)
  # Data matrix - element 1
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
	rm(a);gc()
	retval
}

# Down sampling to reduce data load and normalise features
down_sampling <- function (data, factor = 1) {
	if (factor <= 1) {
		return(data)
	}
	not_multi=is.null(nrow(data))
	if (not_multi) {
		newlen=floor(length(data)/factor)
		if (newlen > 1) {
			newdata=sapply(1:newlen, FUN=function(i, d, f) { mean(d[round((i-1)*f+1):round(i*f)]) },
				d=data, f=factor)
		}
	} else {
		newlen=floor(ncol(data)/factor)
		if (newlen > 1) {
			newdata=sapply(1:newlen,
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
	newdata
}

# Generate features for array?
gen_features_onearray <- function (indata, pre, freqs) {
	feats=c()
	sumheads=c('Min','1stQrt','Med','Mean','3rdQrt','Max')
	sumindex=c(4,6)
	sumheads=sumheads[sumindex]
	a=summary(indata)[sumindex]
	heads=paste0(pre,"_",sumheads)
	feats[heads]=a
	feats[paste0(pre,"_","Sd")]=sd(abs(indata))
	a=summary(abs(indata))[sumindex]
	heads=paste0(pre, "Amp", "_",sumheads)
	feats[heads]=a
	feats[paste0(pre,"Amp_Sd")]=sd(abs(indata))
	freqs=freqs[1:length(indata)]
#	cat(paste("In gen_features_onearray(), before fft size is:",paste(dim(indata),collapse=','),"\n"))
#	flush.console()
#	b=Sys.time()
	if (dolog == 1) {
		myfft=log(abs(fft(indata))+1)
	} else {
		myfft=abs(fft(indata))
	}
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

# Generate feature for series?
gen_features_oneseries <- function (indata, pre, freqs) {
	feats=c()
	indata=as.vector(indata)
	delta1=rep(0,length(indata))
	delta1[1:(length(indata)-1)]=indata[2:length(indata)]-indata[1:(length(indata)-1)]
	delta2=rep(0,length(indata))
	delta2[1:(length(delta1)-2)]=delta1[2:(length(delta1)-1)]-delta1[1:(length(delta1)-2)]
	feats=c(feats, gen_features_onearray(indata, paste0(pre,""), freqs))
	feats=c(feats, gen_features_onearray(delta1, paste0(pre,"Del1"), freqs))
	feats=c(feats, gen_features_onearray(delta2, paste0(pre,"Del2"), freqs))
	feats
}


#Need to use windowed solution. Some of his code computes an average for entire one hour.
#This will be useless and can be discarded.

# Generate feature for file?
gen_features_onefile <- function (indata, f, t) {
	b=Sys.time()
	feats=c()
	chans=nrow(indata)
	nc=ncol(indata)
	if (round(f*t) != nc) {
		stop(paste("Stop in gen_features_onefile(), f*t=",f*t,"!= number of data points,",nc))
	}
	freqs=(1:nc)*1/t
	for (i in 1:chans) {
		prename=paste0("chan",i)
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

# Split matrix - not sure what this is needed for. For Patient1&2? Those files are much bigger.
# Used for windowing I think
split_mat <- function (mymat, nsplit) {
	if (ncol(mymat) %% nsplit != 0) {
		stop(paste("In split_mat(), the nsplit",nsplit,"and column number",ncol(mymat),"do not match to even blocks."))
	}
	retdata=list()
	mysize=ncol(mymat) / nsplit
	for (i in 1:nsplit) {
		retdata[[i]]=mymat[,((i-1)*mysize+1):(i*mysize)]
	} 
	retdata
}

#Endode data file names into a numerical annotation
types=c('Dog_1','Dog_2','Dog_3','Dog_4','Dog_5','Patient_1','Patient_2')
typenums=c(Dog_1=1,Dog_2=2,Dog_3=3,Dog_4=4,Dog_5=5,Patient_1=6,Patient_2=7)

#Set up logical array with 7000 elements. Why 7000? Are there 1000 files per patient?
#This would perhaps be the array of results eg 0 = non-seizure 1 = seizure.
#Set up logical array with 7000 elements
nv=rep(NA,length(types)*1000)
#Create results data frame
summary_df=data.frame(fname=nv, second=nv,freq=nv,ch=nv,label=nv,seq=nv,datalen=nv, time1=nv,time2=nv)

#Redundant
fixfreq=0
#Sample freq
fixfreq=200
#FFT parameters
#Base for log calc?
dolog=1
addFFT=2
FFTratio=0.2
#Windowing split setting
nsplit=10	# original clip is 10min=600sec, nsplit=10, new length=60sec, nsplit=20, newlength=30sec, nsplit=40 newlength=15sec
FFTavglen=24
do_holdout=1

#Set counter
inum=1


#Data files are arranged per folder

for (mytype in types) {
#Set stopwatch  
begTime0=Sys.time()
# datadir=paste0("Data/",mytype)
#Set path for data files, each patient in its own folder
datadir=paste0("F:/Work/Kaggle/SeizurePrediction/Data/",mytype)
#Set holdout directory as sub dir - OMIT NOT USING HOLDOUT DATA
holdoutdir=paste0(datadir,"_holdout")


interfiles=dir(datadir, ".*_interictal_segment_.*.mat")
prefiles=dir(datadir, ".*_preictal_segment_.*.mat")
testfiles=dir(datadir, ".*_test_segment_.*.mat")
if (mytype %in% c("Dog_1","Dog_2","Dog_3","Dog_4") && do_holdout) {
    holdoutfiles=dir(holdoutdir, ".*_holdout_segment_.*.mat")
}

trainmat=NULL
begTime=Sys.time()
isub=1
for (myfile in interfiles) {
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


