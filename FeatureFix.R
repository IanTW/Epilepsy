setwd('H:/Features/Set_3_A')
load('Dog_1_stat_plus_FFT_features.rda')
df$ID <- paste0(df$ID, "_", df$SLICE)
df$SEQ <- NULL
df$SLICE <- NULL
df[,c(-1,-2)] <-round(df[,c(-1,-2)],3)
save(df, file = "Dog_1_stat_plus_FFT_features.rda")

setwd('H:/Features/Set_3_A')
load('Dog_2_stat_plus_FFT_features.rda')
df$ID <- paste0(df$ID, "_", df$SLICE)
df$SEQ <- NULL
df$SLICE <- NULL
df[,c(-1,-2)] <-round(df[,c(-1,-2)],3)
save(df, file = "Dog_2_stat_plus_FFT_features.rda")

setwd('H:/Features/Set_3_A')
load('Dog_3_stat_plus_FFT_features.rda')
df$ID <- paste0(df$ID, "_", df$SLICE)
df$SEQ <- NULL
df$SLICE <- NULL
df[,c(-1,-2)] <-round(df[,c(-1,-2)],3)
save(df, file = "Dog_3_stat_plus_FFT_features.rda")

setwd('H:/Features/Set_3_A')
load('Dog_4_stat_plus_FFT_features.rda')
df$ID <- paste0(df$ID, "_", df$SLICE)
df$SEQ <- NULL
df$SLICE <- NULL
df[,c(-1,-2)] <-round(df[,c(-1,-2)],3)
save(df, file = "Dog_4_stat_plus_FFT_features.rda")

setwd('H:/Features/Set_3_A')
load('Dog_5_stat_plus_FFT_features.rda')
df$ID <- paste0(df$ID, "_", df$SLICE)
df$SEQ <- NULL
df$SLICE <- NULL
df[,c(-1,-2)] <-round(df[,c(-1,-2)],3)
save(df, file = "Dog_5_stat_plus_FFT_features.rda")

setwd('H:/Features/Set_3_A')
load('Patient_1_stat_plus_FFT_features.rda')
df$ID <- paste0(df$ID, "_", df$SLICE)
df$SEQ <- NULL
df$SLICE <- NULL
df[,c(-1,-2)] <-round(df[,c(-1,-2)],3)
save(df, file = "Patient_1_stat_plus_FFT_features.rda")

setwd('H:/Features/Set_3_A')
load('Patient_2_stat_plus_FFT_features.rda')
df$ID <- paste0(df$ID, "_", df$SLICE)
df$SEQ <- NULL
df$SLICE <- NULL
df[,c(-1,-2)] <-round(df[,c(-1,-2)],3)
save(df, file = "Patient_2_stat_plus_FFT_features.rda")