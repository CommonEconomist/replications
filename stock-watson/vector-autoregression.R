# Replication Stock & Watson (2001). "Vector Autoregressions".
# Given that the provided replication data for this paper is only available 
# in a RATS data file, the data needs to be build from scratch following
# the description in the paper. 
# The following data sources are used:
# https://fred.stlouisfed.org/series/GDPCTPI
# https://fred.stlouisfed.org/series/UNRATE
# https://fred.stlouisfed.org/series/FEDFUNDS 
# Only the interest rate is not seasonally adjusted
setwd("~/github/replications/stock-watson")

#prepare data
# NB - Can probably download data using quantmod package
library(data.table)

#gdp
x <- fread("GDPCTPI.csv") 
x[, L.gdpctpi := shift(GDPCTPI, n = 1, type = "lag") ]
x[, pi := 400 * (log(GDPCTPI/L.gdpctpi))]

pi <- ts(x$pi, start = c(1947, 1), frequency = 4)
pi <- window(pi,start = c(1960,1), end=c(2000,4))

#unemployment
x <- fread("UNRATE.csv")

u <- ts(x$UNRATE, start = c(1948,1), frequency=12)
u <- aggregate(u, nfrequency = 4, FUN = mean)
u <- window(u, start = c(1960,1), end = c(2000,4))

#interest rate
x <- fread("FEDFUNDS.csv")
r <- ts(x$FEDFUNDS, start = c(1954,7), frequency = 12) 
r <- aggregate(r, nfrequency = 4, FUN = mean)
r <- window(r, start = c(1960,1), end = c(2000,4))

x <- cbind(pi,u,r) #combine

#plot data
par(mar=c(5,5,2,2), mfrow=c(3,1), las=1, bty="n")
plot(pi, axes=FALSE, xlab="",ylab="",main="Inflation",lwd=2)
axis(2,tick=FALSE)

plot(u,axes=FALSE,xlab="",ylab="",main="Unemployment",lwd=2)
axis(2,tick=FALSE)

plot(r,axes=FALSE,xlab="",ylab="",main="Interest rate",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

#fit model
library(vars)
m <- VAR(x, p = 4) #VAR with 4 lags

#impulse response functions (NB - takes some seconds)
irf.pi <- irf(m, impulse = "pi", ci = .5, runs=1000,seed=42, n.ahead=24) 
irf.u <- irf(m, impulse = "u", ci = .5, runs=1000,seed=42, n.ahead=24) 
irf.r <- irf(m, impulse = "r", ci = .5, runs=1000,seed=42, n.ahead=24) 

#plot (NB - difficulties with adjusting lay-out)
par(pty="s",bty="n")
plot(irf.pi)
plot(irf.u)
plot(irf.r)

#^similar to figure 1 in paper; only small changes in magnitude.

#Bayesian var
library(BVAR)
m <- bvar(x, lags = 4)
imp.response <- irf(m, bv_irf(horizon = 24L))
plot(imp.response)
