## Attempting to replicate "Not all oil price shocks are alike"
# Killian (2009)
# https://www.aeaweb.org/articles?id=10.1257/aer.99.3.1053
# For 'vars' package see:
# http://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/vignettes/vars/vars.pdf
# In progress
setwd("~/Dropbox/github/replications/2009_Killian")

# Load data
d<-read.csv("data.csv",header=FALSE);str(d)
colnames(d)<-c("d.prod","rea","rpo")

x<-ts(d,start=c(1973,1),frequency=12)
plot(x,bty="n")

#### Estimate model: vars package ####
require(vars)
m<-VAR(x,p=24) # 24 lags, reduced-form VAR

# For IRF compute SVAR
Am=diag(3)
Am[2:3,1]<-NA
Am[3,2]<-NA

s<-SVAR(m,estmethod="direct",Amat=Am,Bmat=NULL,hessian=TRUE,method="BFGS")

# Impulse response functions; can take a couple of minutes
d.prod.i<-irf(s,impulse="d.prod",ci=.95,runs=1000,seed=42,n.ahead=15) 
rea.i<-irf(s,impulse="rea",ci=.95,runs=1000,seed=42,n.ahead=15) 
rpo.i<-irf(s,impulse="rpo",ci=.95,runs=1000,seed=42,n.ahead=15) 

# Examine the IRFs; compare with fig.3 from paper.
plot(d.prod.i) 
plot(rea.i) 
plot(rpo.i) 

## IRFs do not really correspond to what is reported in the paper. 
# Kilian estimates a VAR model with OLS and uses the estimates to construct
# the structural VAR representation of the model which is used to calculate
# the IRFs. However, some detail is lacking and so far I have been unable to 
# replicate the result.
