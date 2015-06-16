#******************************************************************************
# This version:  16-06-2015
# First version: 14-05-2014
# Replication "Contagious Rebellion and Preemptive Repression"
# Estimation using JAGS rather than BUGS
# Only model 1
#******************************************************************************

## Set working directory
setwd(["SPECIFY DIR"])

## Load data
load("2014_Danneman_Ritter_JCR.RData")

#### Model 1 ####

## Estimate with BUGS
mod1 <- bugs(data=c("N", "J", "y", "country", "w.pdH", "intconflicthigh", 
                    "cpop", "cgdp", "var.uds", "lciri", "int"),
                   inits=NULL,
                   parameters.to.save=
               c("b00", "b1", "b2", "b3", "b4", "b5", "b6", "b7"),
                   model.file=model.file,
                   n.burnin=1000,
                   n.thin=1,
                   n.chains=3,
                   n.iter=2500)
print(mod1,digits.summary=3)


## Try with JAGS
library(R2jags)
rep1<-jags(data=c("N", "J", "y", "country", "w.pdH", "intconflicthigh", 
                     "cpop", "cgdp", "var.uds", "lciri", "int"),
                   inits=NULL,
                   parameters.to.save=
                c("b00", "b1", "b2", "b3", "b4", "b5", "b6", "b7"),
                   model.file=model.file,
                   n.burnin=1000,
                   n.thin=1,
                   n.chains=3,
                   n.iter=2500)
print(rep1,digits.summary=3)



