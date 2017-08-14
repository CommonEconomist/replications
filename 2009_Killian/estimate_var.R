## Attempting to replicate "Not all oil price shocks are alike", Killian, 2009
# https://www.aeaweb.org/articles?id=10.1257/aer.99.3.1053
# In progress
setwd("~/Dropbox/github/replications/2009_Killian")
library(vars)

# Load data
d<-read.csv("data.csv",header=FALSE);str(d)
colnames(d)<-c("d.prod","rea","rpo")

# Estimate model
m<-VAR(d,p=24)

# Impulse response function (NB - takes about three minutes)
i<-irf(m,ci=.95,runs=1000,seed=42,n.ahead=15,ortho=TRUE) 
plot(i)

# NB - The IRFs for the aggregate and oil-specific demand shocks look somehwat 
# similar to those reported in figure 3 of the paper. However, the IRF for
# the oil supply shock looks nothing like it. 