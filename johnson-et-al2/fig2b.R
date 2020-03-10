#date created 2020.03.10
#last update 2020.03.10
library(brms)
library(data.table)
library(tidyverse)
setwd("~/github/johnson-et-al2")

#data
X <- list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))
x <- X[X$Target=="government"]
x <- x[x$Action %in% c("strike", "occupation", " demonstration", "rally"),]

#data.table
d<-data.table(when = as.Date(x$Date, format = "%d-%b-%y"), 
              where = x$Location)

#variables
d<-d[order(d$where, d$when),]
d[, F.when := shift(when, n=1, type="lead"), by = "where"]
d[, tau := as.numeric(F.when - when),]
d[, n := seq_len(.N), by = 'where']
d<-d[d$tau != 0,]
d[, `:=`(ln.tau = log10(tau), ln.n = log10(n))]

#estimates
m<-brm(ln.tau ~ ln.n + (1 + ln.n | where), d)
beta<-ranef(m, pars = c("ln.n", "Intercept"))

r<-data.table(where = names(beta[[1]][,,1][,1]),
              intercept = beta[[1]][,,1][,1],
              estimate = -beta[[1]][,,2][,1])

#plot
plot(estimate ~ intercept, r)
abline(lm(estimate ~ intercept, r))
