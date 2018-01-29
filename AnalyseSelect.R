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

#####################################################################################

# 4 way LVQ subplot for FFT

setwd("~/Epilepsy/Select/FFT")
library(data.table)
library(plotly)
library(stringr)
library(plyr)

# All selections

load("60-50_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
e <- z[,.(Sum = sum(Freq)), by = .(Feats)]
e$win <- "60-50"

load("60-00_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
f <- z[,.(Sum = sum(Freq)), by = .(Feats)]
f$win <- "60-00"

load("30-50_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
g <- z[,.(Sum = sum(Freq)), by = .(Feats)]
g$win <- "30-50"

load("30-00_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
h <- z[,.(Sum = sum(Freq)), by = .(Feats)]
h$win <- "30-00"

fin <- rbind(e,f,g,h)

#Rename factors
fin$Feats <- revalue(fin$Feats, c("Alpha_Rel" = "Alpha_Norm",
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

e <- fin[fin$win == "60-50",]
Feats <- e$Feats
Counts <- e$Sum
e <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
e$Feats <- factor(e$Feats, levels = unique(e$Feats)[order(e$Counts, decreasing = FALSE)])
p1 <- plot_ly(e, x = ~Counts, y = ~Feats, name = "60s 50% Window", type = "bar", orientation = 'h')

f <- fin[fin$win == "60-00",]
Feats <- f$Feats
Counts <- f$Sum
f <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
f$Feats <- factor(f$Feats, levels = unique(f$Feats)[order(f$Counts, decreasing = FALSE)])
p2 <- plot_ly(f, x = ~Counts, y = ~Feats, name = "60s 0% Window", type = "bar", orientation = 'h') %>% 
layout(margin = m, yaxis = yax, xaxis = xax)

g <- fin[fin$win == "30-50",]
Feats <- g$Feats
Counts <- g$Sum
g <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
g$Feats <- factor(g$Feats, levels = unique(g$Feats)[order(g$Counts, decreasing = FALSE)])
p3 <- plot_ly(g, x = ~Counts, y = ~Feats, name = "30s 50% Window", type = "bar", orientation = 'h')

e <- fin[fin$win == "30-00",]
Feats <- h$Feats
Counts <- h$Sum
h <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
h$Feats <- factor(h$Feats, levels = unique(h$Feats)[order(h$Counts, decreasing = FALSE)])
p4 <- plot_ly(h, x = ~Counts, y = ~Feats, name = "30s 0% Window", type = "bar", orientation = 'h')

subplot(p1, p2, p3, p4, nrows = 4) %>%
  layout(margin = m, legend = list(x = 0.7, y = 0.6))

################################################################################

# 4 way LVQ subplot for Stat

setwd("~/Epilepsy/Select/Stat")
library(data.table)
library(plotly)
library(stringr)
library(plyr)

# All selections

load("60-50_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
e <- z[,.(Sum = sum(Freq)), by = .(Feats)]
e$win <- "60-50"

load("60-00_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
f <- z[,.(Sum = sum(Freq)), by = .(Feats)]
f$win <- "60-00"

load("30-50_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
g <- z[,.(Sum = sum(Freq)), by = .(Feats)]
g$win <- "30-50"

load("30-00_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
h <- z[,.(Sum = sum(Freq)), by = .(Feats)]
h$win <- "30-00"

fin <- rbind(e,f,g,h)

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

e <- fin[fin$win == "60-50",]
Feats <- e$Feats
Counts <- e$Sum
e <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
e$Feats <- factor(e$Feats, levels = unique(e$Feats)[order(e$Counts, decreasing = FALSE)])
p1 <- plot_ly(e, x = ~Counts, y = ~Feats, name = "60s 50% Window", type = "bar", orientation = 'h')

f <- fin[fin$win == "60-00",]
Feats <- f$Feats
Counts <- f$Sum
f <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
f$Feats <- factor(f$Feats, levels = unique(f$Feats)[order(f$Counts, decreasing = FALSE)])
p2 <- plot_ly(f, x = ~Counts, y = ~Feats, name = "60s 0% Window", type = "bar", orientation = 'h') %>% 
  layout(margin = m, yaxis = yax, xaxis = xax)

g <- fin[fin$win == "30-50",]
Feats <- g$Feats
Counts <- g$Sum
g <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
g$Feats <- factor(g$Feats, levels = unique(g$Feats)[order(g$Counts, decreasing = FALSE)])
p3 <- plot_ly(g, x = ~Counts, y = ~Feats, name = "30s 50% Window", type = "bar", orientation = 'h')

e <- fin[fin$win == "30-00",]
Feats <- h$Feats
Counts <- h$Sum
h <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
h$Feats <- factor(h$Feats, levels = unique(h$Feats)[order(h$Counts, decreasing = FALSE)])
p4 <- plot_ly(h, x = ~Counts, y = ~Feats, name = "30s 0% Window", type = "bar", orientation = 'h')

subplot(p1, p2, p3, p4, nrows = 4) %>%
  layout(margin = m, legend = list(x = 0.7, y = 0.6))

################################################################################

# 4 way LVQ subplot for Both

setwd("~/Epilepsy/Select/Both")
library(data.table)
library(plotly)
library(stringr)
library(plyr)

# All selections

load("60-50_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
e <- z[,.(Sum = sum(Freq)), by = .(Feats)]
e$win <- "60-50"

load("60-00_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
f <- z[,.(Sum = sum(Freq)), by = .(Feats)]
f$win <- "60-00"

load("30-50_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
g <- z[,.(Sum = sum(Freq)), by = .(Feats)]
g$win <- "30-50"

load("30-00_LVQ.rda")
a <- list(cols)
a <- table(unlist(a))
a <-as.data.frame(a)
n <- nrow(a)-2
a <- a[1:n,]
a$Var1 <- as.character(a$Var1)
y <- str_split_fixed(as.character(a$Var1),'_', 3)
z <- cbind(a,y)
z <- as.data.table(z)
colnames(z) <- c("Feature", "Freq", "Lab", "Chan", "Feats")
h <- z[,.(Sum = sum(Freq)), by = .(Feats)]
h$win <- "30-00"

fin <- rbind(e,f,g,h)

#Rename factors
fin$Feats <- revalue(fin$Feats, c("Alpha_Rel" = "Alpha_Norm",
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

e <- fin[fin$win == "60-50",]
Feats <- e$Feats
Counts <- e$Sum
e <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
e$Feats <- factor(e$Feats, levels = unique(e$Feats)[order(e$Counts, decreasing = FALSE)])
p1 <- plot_ly(e, x = ~Counts, y = ~Feats, name = "60s 50% Window", type = "bar", orientation = 'h')

f <- fin[fin$win == "60-00",]
Feats <- f$Feats
Counts <- f$Sum
f <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
f$Feats <- factor(f$Feats, levels = unique(f$Feats)[order(f$Counts, decreasing = FALSE)])
p2 <- plot_ly(f, x = ~Counts, y = ~Feats, name = "60s 0% Window", type = "bar", orientation = 'h') %>% 
  layout(margin = m, yaxis = yax, xaxis = xax)

g <- fin[fin$win == "30-50",]
Feats <- g$Feats
Counts <- g$Sum
g <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
g$Feats <- factor(g$Feats, levels = unique(g$Feats)[order(g$Counts, decreasing = FALSE)])
p3 <- plot_ly(g, x = ~Counts, y = ~Feats, name = "30s 50% Window", type = "bar", orientation = 'h')

e <- fin[fin$win == "30-00",]
Feats <- h$Feats
Counts <- h$Sum
h <- data.frame(Feats, Counts, stringsAsFactors = FALSE)
h$Feats <- factor(h$Feats, levels = unique(h$Feats)[order(h$Counts, decreasing = FALSE)])
p4 <- plot_ly(h, x = ~Counts, y = ~Feats, name = "30s 0% Window", type = "bar", orientation = 'h')

subplot(p1, p2, p3, p4, nrows = 4) %>%
  layout(margin = m, legend = list(x = 0.7, y = 0.6))

################################################################################








