library(R.matlab)
pat <- readMat('Patient_1\\Patient_1_interictal_segment_0018.mat')
#pat[[1]][[2]] == 600
#pat[[1]][[3]] == 5000
df <- data.frame(t(pat[[1]][[1]]))
names(df) <- unlist(pat[[1]][[4]])
head(df)