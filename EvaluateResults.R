######################################################################################
# File Name: EvaluateResults.R                                                       #
# Purpose: Summarise results of predictions with evaluation criteria                 #
#                                                                                    #
# Author: Ian Watson                                                                 #
# Email1: d13128934@mydit.ie                                                         #
# Email2: iantwatson@gmail.com                                                       #
#                                                                                    #
# Institution: Dublin Institute of Technology                                        #
# Course Code: DT228B                                                                #
# Course Title: MSc. Data Analytics                                                  # 
# Date of Dissertation Commencement: September 2017                                  #
# Title: A Comparison of SVM and Neural Network Classifiers for Epileptic Seizure    #
# Prediction                                                                         #
#                                                                                    #
# R code for implementing a machine learning classification experiment to compare    #
# the performance of SVM and neural network classifiers used for epileptic seizure   #
# prediction                                                                         #
#                                                                                    #
######################################################################################

# This file should be called from the main code file SeizurePrediction.R

library(data.table)
library(ROCR)
library(plotly)
library(ggplot2)

# Set directory
results.folder <- "E:/Results"
partition.folder <- "E:/Partitions"
predict.folder <- paste0(results.folder, "/Predict/Neural")
label <- "Neural"
list.of.files <- dir(predict.folder)
setwd(predict.folder)

# Make results object
summary.results <- data.frame(matrix(ncol = 9, nrow = 0))

for (filename in list.of.files){
  
  # Console output
  cat("Processing prediction", filename, "\n")
  
  # Load prediction
  load(filename)
    
  # Get all digits from string
  matches <- regmatches(filename, gregexpr("[[:digit:]]+", filename))
  # Unlist the matches
  matches <- as.numeric(unlist(matches))
  # The first match is the file number
  pre <- matches[1]
  
  if (pre < 10){
    pre = paste("0", pre, sep = "")}
  
  if (pre <= 26){
    slice.num = 19
  } else if (pre > 26 && pre <= 53){
    slice.num = 10
  } else if (pre > 53 && pre <= 80){
    slice.num = 39
  } else if (pre > 80){
    slice.num = 20
  }
  
  cat("There are", slice.num, "slices", "\n")
  
  # Set directory to test files
  test.dir <- paste0(partition.folder, "/Test/")
  # Get test file that matches training file
  test.file <- list.files(test.dir, pattern = paste0("^", pre, "_"))
  # Load test file
  load(paste0(test.dir,test.file))
  
  
  # For SVM predictions
  if(grepl("SVM", filename)){
  
    # Get probabilities from prediction
    probs <- attr(svmPredict, "probabilities")
  }  else{
    
    # Get probabilities from prediction
    probs <- as.data.frame(neuralPredict)
  }
    
  # Bind with test data to get original file identities
  test.partition.prob <- cbind(probs, test.partition)
  
  # Remove slice number from ID
  test.partition.prob$ID <- paste0(read.table(text = test.partition.prob$ID,
                                              sep = ".",
                                              as.is = TRUE)$V1,
                                   ".mat")
  
  # Convert to data.table
  test.partition.prob <- as.data.table(test.partition.prob)
  
  # For SVM predictions
  if(grepl("SVM", filename)){
  
  # Summarise to get total probability across all slices for each data file
  results <- test.partition.prob[,.(PreictalProb = sum(Preictal)/slice.num,
                                    InterictalProb = sum(Interictal)/slice.num),
                                 by =.(ID)]
  } else {
  # Summarise to get total probability across all slices for each data file
  results <- test.partition.prob[,.(PreictalProb = sum(V1/slice.num)),
                                 by =.(ID)]
  }
  
  # Create predicted label based on highest probability
  results$Prediction <- factor(ifelse(results$PreictalProb > results$InterictalProb,
                                      "Preictal", "Interictal"))
  
  # Create truth label based on the filename
  results$Truth <- factor(ifelse(grepl("inter", results$ID), "Interictal", "Preictal"))

  # Get confusion matrix
  confusion <- table(results$Prediction, results$Truth)
  
  # Initialise object
  df <- c()
  # Get probabilities
  df$predictions <- results$PreictalProb
  # Get class labels
  df$label <- ifelse(results$Truth == "Preictal", 1, 0)
  # Create prediction object
  pred <- prediction(df$predictions, df$label)
  # Get performance measures
  # AUC
  perf <- performance(pred,"auc")
  auc.score <- round(unlist(attr(perf, "y.values")),2)
  # Specificity
  spec.score <- round(confusion[1]/(confusion[1]+confusion[2]),2)
  # Sensitivity
  sens.score <- round(confusion[4]/(confusion[3]+confusion[4]),2)
  # S1 score
  s1.score <- round(2*(sens.score*spec.score)/(sens.score+spec.score),2)
  
  # Compile results
  results.list <- c(filename,
                    confusion[1],
                    confusion[2],
                    confusion[3],
                    confusion[4],
                    spec.score,
                    sens.score,
                    auc.score,
                    s1.score)
  
  # Bind to output 
  summary.results <- rbind(summary.results, results.list, stringsAsFactors = FALSE)

}

# Create column names
x <- c("Filename", "TN", "FP", "FN", "TP", "Spec", "Sens", "AUC", "S1")
colnames(summary.results) <- x
# Set directory for output
setwd(results.folder)
file.label.rda = paste0(label, "_", "Summary_Results.rda")
file.label.csv = paste0(label, "_", "Summary_Results.csv")
save(summary.results, file = file.label.rda)
write.csv(summary.results, file = file.label.csv, row.names = FALSE)

summary.results

######################################################
# For summary plots

file.names <- read.table(text = summary.results$Filename, sep = "_", colClasses = "character")
summary.results <- cbind(file.names, summary.results)
summary.results$V1 <- NULL # SVM and Neural only
summary.results$V2 <- NULL # SVM and Neural only
summary.results$V8 <- NULL # SVM and Neural only
summary.results$V9 <- NULL # SVM and Neural only
summary.results$V6 <- NULL # Random only
summary.results$V7 <- NULL # Random only

summary.results$Filename <- NULL
colnames(summary.results) <- c("Number",
                               "Window",
                               "Feature",
                               "Window",
                               "Sampling",
                               "TN",
                               "FP",
                               "FN",
                               "TP",
                               "Specificity",
                               "Sensitivity",
                               "AUC",
                               "S1_Score")


a <- summary.results  # Binomial
a$Classifier <- "Random"
b <- summary.results  # Neural
b$Classifier <- "ANN"
c <- summary.results  # SVM
c$Classifier <- "SVM"

summary.results <- rbind(a,b,c)

summary.results$Window <- as.factor(summary.results$Window)
summary.results$Feature <- as.factor(summary.results$Feature)
summary.results$Window <- as.factor(summary.results$Window)
summary.results$Sampling <- as.factor(summary.results$Sampling)
summary.results$Classifier <- as.factor(summary.results$Classifier)
summary.results$Specificity <- as.numeric(summary.results$Specificity)
summary.results$Sensitivity <- as.numeric(summary.results$Sensitivity)
summary.results$TN <- as.numeric(summary.results$TN)
summary.results$TP <- as.numeric(summary.results$TP)
summary.results$FN <- as.numeric(summary.results$FN)
summary.results$FP <- as.numeric(summary.results$FP)
summary.results$AUC <- as.numeric(summary.results$AUC)
summary.results$S1_Score <- as.numeric(summary.results$S1_Score)

dat <- summary.results 
save(dat, file="Merged_Results.rda")
##############################################################################################
#BOXPLOTS CLASSIFIERS

#Reorder factor levels
dat$Classifier <- factor(dat$Classifier, levels = c("Neural", "SVM", "Random"))

xax = list(title = "")
xax1 =list(title = "", showticklabels = FALSE)
yax <- list(range = c(0,1.1), title = "Sensitivity")
yax1 <- list(range = c(0,1.1), title = "Specificity")
yax2<- list(range = c(0,1.1), title = "S1 Score")

m <- list(
  l = 130,
  r = 30,
  b = 60,
  t = 60,
  pad = 4)

p1 <- plot_ly(dat, x = ~Classifier, y = ~Sensitivity, type = "box", boxpoints = "all", jitter = 0.2,
              pointpos = -1.8, marker = list(size = 3)) %>%
  layout(boxmode = "group", yaxis = yax) 

p2 <- plot_ly(dat, x = ~Classifier, y = ~Specificity, type = "box", boxpoints = "all", jitter = 0.2,
              pointpos = -1.8, marker = list(size = 3)) %>%
  layout(boxmode = "group", yaxis = yax1) 

p3 <- plot_ly(dat, x = ~Classifier, y = ~S1_Score, type = "box", boxpoints = "all", jitter = 0.2,
              pointpos = -1.8, marker = list(size = 3)) %>%
  layout(boxmode = "group", yaxis = yax2) 

subplot(p1,p2,p3,nrows = 3, shareY = TRUE, shareX = TRUE, margin = 0.03) %>% 
  layout(showlegend = FALSE, xaxis = xax)

###############################################

# Filter out random classifier
dat <- dat[dat$Classifier != "Random",]
dat$Classifier <- levels(droplevels(dat$Classifier))
dat$Classifier <- as.factor(dat$Classifier)



# Rename factor levels
dat$Window <- revalue(dat$Window, c("Lvq"="LVQ", "Rfe"="RFE", "Non"="None"))
dat$Feature <- revalue(dat$Feature, c("Stat" = "Statistical",
                                      "FFT" = "Spectral",
                                      "Both" = "Combined"))
dat$Window <- revalue(dat$Window, c("30-00" = "30s-0%",
                                    "30-50" = "30s-50%",
                                    "60-00" = "60s-0%",
                                    "60-50" = "60s-50%"))
#Reorder factor levels
dat$Window <- factor(dat$Window, levels = c("None", "LVQ", "RFE"))
#Reorder factor levels
dat$Feature <- factor(dat$Feature, levels = c("Statistical", "Spectral", "Combined"))
dat$Sampling <- factor(dat$Sampling, levels = c("Normal", "Increased", "Reduced"))

p1 <- plot_ly(dat, x = ~Classifier, y = ~Sensitivity, color = ~Sampling, type = "box", boxpoints = "all", marker = list(size = 3)) %>%
  layout(boxmode = "group", yaxis = yax, xaxis = xax1, showlegend = FALSE) 

p2 <- plot_ly(dat, x = ~Classifier, y = ~Specificity,color = ~Sampling, type = "box", boxpoints = "all", marker = list(size = 3)) %>%
  layout(boxmode = "group", yaxis = yax1, xaxis = xax1, showlegend = TRUE) 

p3 <- plot_ly(dat, x = ~Classifier, y = ~S1_Score, color = ~Sampling, type = "box", boxpoints = "all", marker = list(size = 3)) %>%
  layout(boxmode = "group", yaxis = yax2, xaxis = xax, showlegend = FALSE) 

subplot(p1,p2,p3,nrows = 3, shareY = TRUE, shareX = TRUE) %>% 
  layout(showlegend = FALSE, xaxis = xax)

######################################################################################################

datsvm <- dat[dat$Classifier == "SVM",]
datann <- dat[dat$Classifier == "Neural",]
datann <- datann[,c(10:14)]
names(datann)[names(datann) == 'Specificity'] <- 'Specificity2'
names(datann)[names(datann) == 'Sensitivity'] <- 'Sensitivity2'
names(datann)[names(datann) == 'S1_Score'] <- 'S1_Score2'
datann$AUC <- NULL
datann$Classifier <- NULL

d1 <- cbind(datsvm, datann)


d1$Sampling <- factor(d1$Sampling, levels = c("Normal", "Increased", "Reduced"))


#d1 <- d1[order(d1$Feature,decreasing = TRUE),]
d1 <- d1[order(d1$Sampling,decreasing = FALSE),]
#d1 <- d1[order(d1$Window,decreasing = TRUE),]
d1 <- d1[order(d1$Sampling, d1$Window, decreasing = TRUE),]
d1 <- d1[order(d1$Sampling, d1$Feature, decreasing = TRUE),]

d1$Num <- c(1:108)
p


plot_ly(d1, x = ~Num, y = ~Sensitivity, type = 'scatter', name = "SVM", mode = 'lines') %>%
  add_trace(y= ~Sensitivity2, type = 'scatter',name = "ANN", mode = 'lines') %>% 
  add_trace(x = 36, y=c(0,1.1),
            hoverinfo = "text",
            showlegend = FALSE,
            line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'),
            mode = "lines",
            type = "scatter") %>%
  add_trace(x = 72, y=c(0,1.1),
            hoverinfo = "text",
            line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'),
            showlegend = FALSE,
            mode = "lines",
            type = "scatter") %>%
  add_trace(x = 12, y=c(0,1.1),
            hoverinfo = "text",
            line = list(color = 'rgb(205, 12, 24)', width = 1, dash = 'dot'),
            showlegend = FALSE,
            mode = "lines",
            type = "scatter") %>%
  add_trace(x = 24, y=c(0,1.1),
            hoverinfo = "text",
            line = list(color = 'rgb(205, 12, 24)', width = 1, dash = 'dot'),
            showlegend = FALSE,
            mode = "lines",
            type = "scatter") %>%
  add_trace(x = 48, y=c(0,1.1),
            hoverinfo = "text",
            line = list(color = 'rgb(205, 12, 24)', width = 1, dash = 'dot'),
            showlegend = FALSE,
            mode = "lines",
            type = "scatter") %>%
  add_trace(x = 60, y=c(0,1.1),
            hoverinfo = "text",
            line = list(color = 'rgb(205, 12, 24)', width = 1, dash = 'dot'),
            showlegend = FALSE,
            mode = "lines",
            type = "scatter") %>%
  add_trace(x = 84, y=c(0,1.1),
            hoverinfo = "text",
            line = list(color = 'rgb(205, 12, 24)', width = 1, dash = 'dot'),
            showlegend = FALSE,
            mode = "lines",
            type = "scatter") %>%
  add_trace(x = 96, y=c(0,1.1),
            hoverinfo = "text",
            line = list(color = 'rgb(205, 12, 24)', width = 1, dash = 'dot'),
            showlegend = FALSE,
            mode = "lines",
            type = "scatter") %>%
  
    layout(xaxis = list(title = "Model Number"), legend = list(x=0.11,y=0.4), 
           annotations = list(list(text = "REDUCED SAMPLING",  x = 19, y = 1.15, showarrow=FALSE), 
                              list(text = "INCREASED MINORITY",  x = 54, y = 1.15, showarrow=FALSE),
                              list(text = "NORMAL SAMPLING",  x = 90, y = 1.15, showarrow=FALSE),
                              list(text = "STATISTIC",  x = 6, y = 1.05, showarrow=FALSE),
                              list(text = "SPECTRAL",  x = 18, y = 1.05, showarrow=FALSE),
                              list(text = "COMBINED",  x = 30, y = 1.05, showarrow=FALSE),
                              list(text = "STATISTIC",  x = 42, y = 1.05, showarrow=FALSE),
                              list(text = "SPECTRAL",  x = 54, y = 1.05, showarrow=FALSE),
                              list(text = "COMBINED",  x = 66, y = 1.05, showarrow=FALSE),
                              list(text = "STATISTIC",  x = 78, y = 1.05, showarrow=FALSE),
                              list(text = "SPECTRAL",  x = 90, y = 1.05, showarrow=FALSE),
                              list(text = "COMBINED",  x = 102, y = 1.05, showarrow=FALSE)
                              ))
######################################################
# For AUC plots

# Graphics
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

#####################################################
# Statistical Tests for classifier comparison

library(pastecs)
library(pgirmess)
load("Merged_Results.rda")

# Histogram to check normality
hist <- ggplot(dat, aes(Specificity))+geom_histogram(aes(y=..density..))

# Summary stats
# Classifier
by(dat$Sensitivity, dat$Classifier, stat.desc, basic = FALSE, norm = TRUE)
by(dat$Specificity, dat$Classifier, stat.desc, basic = FALSE, norm = TRUE)
by(dat$S1_Score, dat$Classifier, stat.desc, basic = FALSE, norm = TRUE)

# Wilcoxon Rank Sum (2 samples - suitable for comparing results of two samples)
# Remove Random if needed
dat <- dat[dat$Classifier != "Random",]
newModel <- wilcox.test(S1_Score ~ Classifier, data = dat, paired = FALSE)
newModel

# Function to get effect size (See 15.5.6.)
rFromWilcox<-function(wilcoxModel, N){
z<- qnorm(wilcoxModel$p.value/2)
r<- z/ sqrt(N)
cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

# Run the function (N = total samples)
rFromWilcox(newModel, 216)

###########################

# Kruskal-Wallis (multiple samples)
# Reorder factors
dat$Classifier <- factor(dat$Classifier, levels = levels(dat$Classifier)[c(2,1,3)])

# Sensitivity
kruskal.test(Sensitivity ~ Classifier, data = dat)
# Specificity
kruskal.test(Specificity ~ Classifier, data = dat)
# S1 Score
kruskal.test(S1_Score ~ Classifier, data = dat)

# Create rank for each group
dat$Ranks <- rank(dat$Classifier)
# Mean rank
by(dat$Ranks,dat$Classifier,mean)

#Post hoc tests - Sensitivity
#kruskalmc(Sensitivity ~ Classifier, data = dat)
#Two-tailed version
#kruskalmc(Sensitivity ~ Classifier, data = dat, cont='two-tailed')

#Post hoc tests - Specificity
kruskalmc(Specificity ~ Classifier, data = dat)
#Two-tailed version
kruskalmc(Specificity ~ Classifier, data = dat, cont='two-tailed')

#Post hoc tests - S1 score
kruskalmc(S1_Score ~ Classifier, data = dat)
#Two-tailed version
kruskalmc(S1_Score ~ Classifier, data = dat, cont='two-tailed')

####################################################
# Statistical Tests for processing comparison

library(pastecs)
library(pgirmess)
load("Merged_Results.rda")
dat <- dat[dat$Classifier != "Random",]

# Kruskal-Wallis (multiple samples)
# Check levels
levels(dat$Selection)
# Reorder factors
dat$Selection <- factor(dat$Selection, levels = levels(dat$Selection)[c(2,1,3)])

# Sensitivity
kruskal.test(Sensitivity ~ Selection, data = dat)
# Specificity
kruskal.test(Specificity ~ Selection, data = dat)
# S1 Score
kruskal.test(S1_Score ~ Selection, data = dat)

# Create rank for each group
dat$Ranks <- rank(dat$Selection)
# Mean rank
by(dat$Ranks,dat$Selection,mean)

#Post hoc tests - Sensitivity
kruskalmc(Sensitivity ~ Selection, data = dat)
#Two-tailed version
kruskalmc(Sensitivity ~ Selection, data = dat, cont='two-tailed')

#Post hoc tests - Specificity
kruskalmc(Specificity ~ Selection, data = dat)
#Two-tailed version
kruskalmc(Specificity ~ Selection, data = dat, cont='two-tailed')

#Post hoc tests - S1 score
kruskalmc(S1_Score ~ Selection, data = dat)
#Two-tailed version
kruskalmc(S1_Score ~ Selection, data = dat, cont='two-tailed')
