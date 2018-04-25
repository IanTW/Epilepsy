######################################################################################
# File Name: SelectFeature.R                                                         #
# Purpose: Feature reduction                                                         #
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

# Load feature data
setwd(features.dir)
#load("Combined_stat_features.rda")
#load("Combined_FFT_features.rda")
#load("Combined_stat_plus_FFT_features.rda")

###########################
# Calculate correlation matrix

# Ignore first two columns containing filename and class labels
correlationMatrix <- cor(combined.feature[,3:ncol(combined.feature)])
# Indices of features that are highly corrected
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)

# Save reduced feature set indexes
#save(highlyCorrelated, file = "Corr_stat.rda")
#save(highlyCorrelated, file = "Corr_FFT.rda")
#save(highlyCorrelated, file = "Corr_stat_plus_FFT.rda")

###########################
# Subset
temp <- combined.feature[combined.feature$CLASS == "Preictal",]
temp1 <- combined.feature[combined.feature$CLASS == "Interictal",]
temp1 <- temp1[sample(nrow(temp1),30000),]
combined.feature <-rbind(temp, temp1)
table(combined.feature$CLASS)

###########################
# RFE Method
library(caret)

# Calculate Recursive Feature Elimenation (RFE)
# Number of columns in feature vector
control <- rfeControl(functions = rfFuncs, method = "cv", number = 2, verbose = TRUE)

results <- rfe(combined.feature[,3:ncol(combined.feature)], # Features
               combined.feature[,2], # Class labels
               sizes=c(1:80), # Set to required number of features - /80/224/304
               rfeControl=control)

# Save RFE results
save(results, file = "RFE_Stat_50k.rda")
#save(results, file = "RFE_FFT_40k.rda")
#save(results, file = "RFE_Both_40k.rda")

###########################
#LVQ method
library(caret)

control <- trainControl(method="cv", number = 2, verboseIter = TRUE)

#grid <- expand.grid(size=c(33,66), k=c(67,99, 121))

# Train the model
model <- train(combined.feature[,3:ncol(combined.feature)], # Features
               combined.feature[,2], # Class labels
               method = "lvq", 
               trControl=control)

# Train the model with grid tune
#model <- train(combined.feature[,3:ncol(combined.feature)], # Features
#               combined.feature[,2], # Class labels
#               method = "lvq", 
#               trControl=control,
#               tuneGrid = grid)


# Save LVQ results
save(model, file = "LVQ_Stat_50k.rda")
#save(model, file = "LVQ_FFT_40k.rda")
#save(model, file = "LVQ_Both_40k.rda")

#################################################################
# Graphics/Options for LVQ

# estimate variable importance
importance <- varImp(model, scale=FALSE)
plot(varImp(model, scale = FALSE), top = 20)
# summarize importance
print(importance)
# plot model (show k size and codebook size)
plot(model)

# Top variables
LVQ <- data.frame(varImp(model)[1])
LVQ <- LVQ[order(-LVQ$importance.Interictal),]
LVQ <- LVQ[c(1:50),] # Top 20 etc
# Get var names
LVQ <- rownames(LVQ)
cols <- c("ID", "CLASS", LVQ)
save(cols, file = "LVQ.rda")

#################################################################
# Graphics/Options for RFE
# Plot results
print(results)
print(results$optVariables)
plot(results, main = "Title Here",type=c("g", "o"))

library(data.table)
# Get list of optimal features 
RFE <- results$optVariables[1:50] # Set according to number of features 20/50 etc
cols <- c("ID", "CLASS", RFE)
save(cols, file = "RFE.rda")

# Plot RFE
library(plotly)

# Load stat
df <- results$results

# Load FFT
ef <- results$results

# Load both 
ff <- results$results

xax <- list(
  title = "Features")
yax <- list(
  title = "Accuracy",
  range = c(0.7,0.95))

m <- df[which.max(df$Accuracy), ]

a <- list(
  x = m$Variables,
  y = m$Accuracy,
  text = paste0("Optimal Features = ", rownames(m)),
  xref = "x1",
  yref = "y1",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 60,
  ay = 60
)
p1 <- plot_ly(df, x = ~Variables, y = ~Accuracy, type = 'scatter', mode = 'lines+markers', name = "Statistical Features") %>%
add_trace(df, 
          x = m$Variables, y=c(0.85,0.94),
          hoverinfo = "text",
          showlegend = FALSE,
          mode = "lines",
          type = "scatter") %>%
layout(xaxis =xax, yaxis = yax, annotations = a) 

n <- ef[which.max(ef$Accuracy), ]

b <- list(
  x = n$Variables,
  y = n$Accuracy,
  text = paste0("Optimal Features = ", rownames(n)),
  xref = "x2",
  yref = "y2",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 60,
  ay = 60
)

p2 <- plot_ly(ef, x = ~Variables, y = ~Accuracy, type = 'scatter', mode = 'lines+markers', line = list(color = '#2ca02c'),name = "Spectral Features") %>%
  layout(xaxis =xax, yaxis = yax, annotations =b) %>%
  add_trace(ef, 
            x = n$Variables, y=c(0.87,0.96),
            hoverinfo = "text",
            showlegend = FALSE,
            mode = "lines",
            type = "scatter")

o <- ff[which.max(ff$Accuracy), ]

c <- list(
  x = o$Variables,
  y = o$Accuracy,
  text = paste0("Optimal Features = ", rownames(o)),
  xref = "x3",
  yref = "y3",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 60,
  ay = 60
)

p3 <- plot_ly(ff, x = ~Variables, y = ~Accuracy, type = 'scatter', mode = 'lines+markers', line = list(color = '#9467bd'),name = "Combined Features") %>%
  add_trace(ff, 
            x = o$Variables, y=c(0.87,0.96),
            hoverinfo = "text",
            showlegend = FALSE,
            mode = "lines",
            type = "scatter") %>%
  layout( xaxis =xax, yaxis = yax, annotations = c) 

subplot(p1,p2,p3,nrows = 3, shareX = TRUE, titleY = TRUE) %>% 
  layout(showlegend = TRUE, legend = list(x = 0.7, y = 0.9))

