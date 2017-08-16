## Replication "Vector Autoregressions"
# Stock & Watson (2001)
# https://www.aeaweb.org/articles?id=10.1257/jep.15.4.101

# Given that the provided replication data for this paper is only available 
# in a RATS data file, the data needs to be build from scratch following
# the description in the paper. 
# The following data sources are used:
# https://fred.stlouisfed.org/series/GDPCTPI
# https://fred.stlouisfed.org/series/UNRATE
# https://fred.stlouisfed.org/series/FEDFUNDS 
# Only the interest rate is not seasonally adjusted
setwd("~/Dropbox/github/replications/2001_Stock_Watson")

#### Prepare data ####
require(dplyr)

# Inflation rate
d<-read.csv("GDPCTPI.csv",stringsAsFactors=FALSE)
d$gdp.l<-lag(d[,2],lag=1)
d$pi=400*(log(d[,2]/d[,3]))

pi<-ts(d$pi,start=c(1947,1),frequency=4)
pi<-window(pi,start=c(1960,1),end=c(2000,4))

# Unemployment rate
d<-read.csv("UNRATE.csv",stringsAsFactors=FALSE)
u<-ts(d$UNRATE,start=c(1948,1),frequency=12)
u<-aggregate(u,nfrequency=4,FUN=mean)

u<-window(u,start=c(1960,1),end=c(2000,4))

# Interest rate
d<-read.csv("FEDFUNDS.csv",stringsAsFactors=FALSE)
R<-ts(d$FEDFUNDS,start=c(1954,7),frequency=12)
R<-aggregate(R,nfrequency=4,FUN=mean)

R<-window(R,start=c(1960,1),end=c(2000,4))

x<-cbind(pi,u,R)

#### Plot data ####
par(mar=c(5,5,2,2),mfrow=c(3,1),las=1,bty="n",cex.lab=2,cex.axis=2,cex.main=2)
plot(pi,axes=FALSE,xlab="",ylab="",main="Inflation",lwd=2);axis(2,tick=FALSE)
plot(u,axes=FALSE,xlab="",ylab="",main="Unemployment",lwd=2);axis(2,tick=FALSE)
plot(R,axes=FALSE,xlab="",ylab="",main="Interest rate",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

#### Estimate model ####
# TO DO: Replicate table 1 (Granger causality, variance decomposition)
library(vars)
m<-VAR(x,p=4) # Fit VAR with 4 lags

# Impulse response functions (takes a couple of minutes)
pi.i<-irf(m,impulse="pi",ci=.95,runs=1000,seed=42,n.ahead=24) # Inflation
u.i<-irf(m,impulse="u",ci=.95,runs=1000,seed=42,n.ahead=24)   # Unemployment
R.i<-irf(m,impulse="R",ci=.95,runs=1000,seed=42,n.ahead=24)   # Interest rate

# Plot results
# NB - Having difficulties in chaning the plot lay out
# Shape of IRF is similar to those reported in paper, only some minor 
# differences here and there in estimated magnitude. 
par(pty="s",bty="n")
plot(pi.i)
plot(u.i)
plot(R.i)


#### Test with Bayesian VAR estimation ####
detach("package:vars",unload=TRUE)
require(MSBVAR)
m2<-reduced.form.var(x,4,z=NULL)
dfev(m2,k=12) # Variance decomposition

m2.irf<-irf(m2,nsteps=24)
plot(m2.irf)

# Try to get uncertainty level with MC simulation
imp<- mc.irf(m2,nsteps=24, draws=2000)
plot(imp,method="Percentile",probs=c(.16,.84)) # Strange

imp<- mc.irf(m2,nsteps=4, draws=100)
plot(imp,method="Percentile",probs=c(.16,.84)) # Problem persists

# BVAR model
# NB - mc.irf gives same weird IRF plot
m2.bvar<-szbvar(x,p=4,z=NULL,
                 lambda0=1,lambda1=1,lambda3=1,lambda4=1,lambda5=0,
                mu5=1,mu6=1,nu=4,qm=4,prior=1, posterior.fit=TRUE)
plot(irf(m2.bvar,nsteps=24))

