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
                               "Selection",
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


library(plotly)
library(ggplot2)

summary.results$Window <- as.factor(summary.results$Window)
summary.results$Feature <- as.factor(summary.results$Feature)
summary.results$Selection <- as.factor(summary.results$Selection)
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
yax <- list(range = c(0,1), title = "Sensitivity")
yax1 <- list(range = c(0,1), title = "Specificity")
yax2<- list(range = c(0,1), title = "S1 Score")

m <- list(
  l = 130,
  r = 30,
  b = 60,
  t = 20,
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

dat <- dat[dat$Classifier != "Random",]

p1 <- plot_ly(dat, x = ~Classifier, y = ~Sensitivity, color = ~Selection, type = "box") %>%
  layout(boxmode = "group", yaxis = yax, xaxis = xax, showlegend = FALSE) 

p2 <- plot_ly(dat, x = ~Classifier, y = ~Specificity,color = ~Selection, type = "box") %>%
  layout(boxmode = "group", yaxis = yax1, xaxis = xax, showlegend = TRUE) 

p3 <- plot_ly(dat, x = ~Classifier, y = ~S1_Score, color = ~Selection, type = "box") %>%
  layout(boxmode = "group", yaxis = yax2, xaxis = xax, showlegend = FALSE) 

subplot(p1,p2,p3,nrows = 3, shareY = TRUE, shareX = TRUE) %>% 
  layout(showlegend = FALSE, xaxis = xax)



























#Ordering
summary.results$Feature <- factor(summary.results$Feature, levels = c("Stat", "FFT", "Both"))


p <- ggplot(summary.results, aes(x=Window, y=Specificity, fill=Window)) + geom_boxplot()

# Plot of specificity vs features and selection
ggplot(summary.results, aes(x=Feature, y=Specificity, fill=Selection)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())

# Plot of sensitvity vs features and selection
ggplot(summary.results, aes(x=Feature, y=Sensitivity, fill=Selection)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())



# Plot of sensitivity vs windows and Feature
ggplot(summary.results, aes(x=Window, y=Sensitivity, fill = Feature)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())


###############KEEPERS#####################
#Ordering
summary.results$Window <- factor(summary.results$Window, levels = c("30-00", "60-50", "30-50","60-00"))
# Plot of sensitivity vs windows
ggplot(summary.results, aes(x=Window, y=Sensitivity, fill = Window)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21)
###############KEEPERS#####################
# Plot of specificity vs features
ggplot(summary.results, aes(x=Feature, y=Specificity, fill=Feature)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())
###############KEEPERS#####################
#Ordering
summary.results$Selection<- factor(summary.results$Selection, levels = c("Non", "Rfe", "Lvq"))
# Plot of specificity vs selection
ggplot(summary.results, aes(x=Selection, y=Specificity, fill=Selection)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(size = 3, colour = "black", shape = 21, position = position_jitterdodge())









######################################################
# For AUC plots

# Graphics
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

#####################################################