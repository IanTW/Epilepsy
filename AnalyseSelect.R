setwd("~/Epilepsy/Select/FFT")
library(data.table)
library(plotly)
library(stringr)
library(plyr)

# All selections

load("60-50_LVQ.rda")
a <- list(cols)
load("60-50_RFE.rda")
b <- list(cols)
load("60-00_LVQ.rda")
c <- list(cols)
load("60-00_RFE.rda")
d <- list(cols)
load("30-50_LVQ.rda")
e <- list(cols)
load("30-50_RFE.rda")
f <- list(cols)
load("30-00_LVQ.rda")
g <- list(cols)
load("30-00_RFE.rda")
h <- list(cols)

merged <- list(a,b,c,d,e,f,g,h)

x <- table(unlist(merged))
x <-as.data.frame(x)
n <- nrow(x)-2
x <- x[1:n,]
x$Var1 <- as.character(x$Var1)
# For Stat
#y <- read.table(text = x$Var1, sep = "_", colClasses = "character")
# For FFT
y <- str_split_fixed(as.character(x$Var1),'_', 3)

z <- cbind(x,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
dat <- z[,.(Sum = sum(Freq)), by = .(Feats)]

#Rename factors
dat$Feats <- revalue(dat$Feats, c("Alpha_Rel" = "Alpha_Norm",
                     "GammaHigh" = "Gamma_High",
                     "GammaLow" = "Gamma_Low",
                     "Beta_Rel" = "Beta_Norm",
                     "Delta_Rel" = "Delta_Norm",
                     "GammaHigh_Rel" = "Gamma_High_Norm",
                     "GammaLow_Rel" = "Gamma_Low_Norm",
                     "Theta_Rel" = "Theta_Norm",
                     "Total_Power" = "Total_Energy"))

# Set up margins
m <- list(
  l = 130,
  r = 30,
  b = 60,
  t = 20,
  pad = 4)

yax <- list(
  title = "")

xax <- list(
  title = "Number of Times Selected")

Feats <- dat$Feats
Counts <- dat$Sum
dat <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
dat$Feats <- factor(dat$Feats, levels = unique(dat$Feats)[order(dat$Counts, decreasing = FALSE)])
plot_ly(dat, x = ~Counts, y = ~Feats, type = "bar", orientation = 'h') %>%
  layout(margin = m, yaxis = yax, xaxis = xax)

##########################################

setwd("~/Epilepsy/Select/Stat")
library(data.table)
library(plotly)
library(stringr)
library(plyr)

# All selections

load("60-50_LVQ.rda")
a <- list(cols)
load("60-50_RFE.rda")
b <- list(cols)
load("60-00_LVQ.rda")
c <- list(cols)
load("60-00_RFE.rda")
d <- list(cols)
load("30-50_LVQ.rda")
e <- list(cols)
load("30-50_RFE.rda")
f <- list(cols)
load("30-00_LVQ.rda")
g <- list(cols)
load("30-00_RFE.rda")
h <- list(cols)

merged <- list(a,b,c,d,e,f,g,h)

x <- table(unlist(merged))
x <-as.data.frame(x)
n <- nrow(x)-2
x <- x[1:n,]
x$Var1 <- as.character(x$Var1)
# For Stat
y <- read.table(text = x$Var1, sep = "_", colClasses = "character")
# For FFT
#y <- str_split_fixed(as.character(x$Var1),'_', 3)

z <- cbind(x,y)
z <- as.data.table(z)
z$V1 <- NULL
z$V2 <- NULL
colnames(z) <- c("Feature", "Freq", "Feats")
dat <- z[,.(Sum = sum(Freq)), by = .(Feats)]

# Set up margins
m <- list(
  l = 70,
  r = 10,
  b = 60,
  t = 20,
  pad = 4)

yax <- list(
  title = "")

xax <- list(
  title = "Number of Times Selected")

Feats <- dat$Feats
Counts <- dat$Sum
dat <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
dat$Feats <- factor(dat$Feats, levels = unique(dat$Feats)[order(dat$Counts, decreasing = FALSE)])
plot_ly(dat, x = ~Counts, y = ~Feats, type = "bar", orientation = 'h') %>%
  layout(margin = m, yaxis = yax, xaxis = xax)

##########################################

setwd("~/Epilepsy/Select/Both")
library(data.table)
library(plotly)
library(stringr)
library(plyr)

# All selections

load("60-50_LVQ.rda")
a <- list(cols)
load("60-50_RFE.rda")
b <- list(cols)
load("60-00_LVQ.rda")
c <- list(cols)
load("60-00_RFE.rda")
d <- list(cols)
load("30-50_LVQ.rda")
e <- list(cols)
load("30-50_RFE.rda")
f <- list(cols)
load("30-00_LVQ.rda")
g <- list(cols)
load("30-00_RFE.rda")
h <- list(cols)

merged <- list(a,b,c,d,e,f,g,h)

x <- table(unlist(merged))
x <-as.data.frame(x)
n <- nrow(x)-2
x <- x[1:n,]
x$Var1 <- as.character(x$Var1)
# For FFT
y <- str_split_fixed(as.character(x$Var1),'_', 3)

z <- cbind(x,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
dat <- z[,.(Sum = sum(Freq)), by = .(Feats)]

# Set up margins
m <- list(
  l = 120,
  r = 10,
  b = 60,
  t = 20,
  pad = 4)

yax <- list(
  title = "")

xax <- list(
  title = "Number of Times Selected")

Feats <- dat$Feats
Counts <- dat$Sum
dat <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
dat$Feats <- factor(dat$Feats, levels = unique(dat$Feats)[order(dat$Counts, decreasing = FALSE)])
plot_ly(dat, x = ~Counts, y = ~Feats, type = "bar", orientation = 'h') %>%
  layout(margin = m, yaxis = yax, xaxis = xax)



