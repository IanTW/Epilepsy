setwd("~/Epilepsy/Select/Stat")


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

test <- list(a,b,c,d,e,f,g,h)

table(unlist(test))