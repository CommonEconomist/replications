#date created 2020.03.10
#last update  2020.03.14
library(brms)
library(data.table)
library(tidyverse)
setwd("~/github/replications/johnson-et-al2")

#data
X <- list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))
x <- X[X$Target=="government"]
x <- x[x$Action %in% c("strike", "occupation", " demonstration", "rally"),]

#data.table
d<-data.table(event.date = as.Date(x$Date, format = "%d-%b-%y"), 
              location = x$Location)

#variables
d<-d[order(d$location, d$event.date),]
d[, next.event := shift(event.date, n=1, type="lead"), by = "location"]
d[, interval := as.numeric(next.event - event.date),]
d[, event.number := seq_len(.N), by = 'location']
d<-d[d$interval != 0,]
d[, `:=`(log.interval = log10(interval), 
         log.event.number = log10(event.number))]

#estimates
m<-brm(log.interval ~ log.event.number + (1 + log.event.number | location), d)
beta<-ranef(m, pars = c("log.event.number", "Intercept"))

r<-data.table(location = names(beta[[1]][,,1][,1]),
              intercept = beta[[1]][,,1][,1],
              estimate = -beta[[1]][,,2][,1])

#plot
par(pty="s")
plot(estimate ~ intercept, r)
abline(lm(estimate ~ intercept, r))
