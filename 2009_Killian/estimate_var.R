## Attempting to replicate "Not all oil price shocks are alike"
# Killian (2009)
# https://www.aeaweb.org/articles?id=10.1257/aer.99.3.1053
# In progress
setwd("~/Dropbox/github/replications/2009_Killian")

# Load data
d<-read.csv("data.csv",header=FALSE);str(d)
colnames(d)<-c("d.prod","rea","rpo")

x<-ts(d,start=c(1973,1),frequency=12)
plot(x,bty="n")

#### Estimate model: vars package ####
require(vars)
m<-VAR(x,p=24) # 24 lags

# Impulse response functions, takes about three minutes each
d.prod.i<-irf(m,impulse="d.prod",ci=.95,runs=1000,seed=42,n.ahead=15) 
rea.i<-irf(m,impulse="rea",ci=.95,runs=1000,seed=42,n.ahead=15) 
rpo.i<-irf(m,impulse="rpo",ci=.95,runs=1000,seed=42,n.ahead=15) 

# Examine the IRFs; compare with fig.3 from paper.
plot(d.prod.i) # Way off
plot(rea.i) # Slight resemblance
plot(rpo.i) # Slight resemblance

#### Estimate model: MBSVAR package ####
detach("package:vars",unload=TRUE)
library(MSBVAR)
m2<-reduced.form.var(x,p=24)
m2.irf<-irf(m2,nsteps=24)

plot(m2.irf) # Same IRFs, little resemblance for oil supply shocks, or others
