## Test fatality interpolation for Iraq
# Using method from Friedman (2014)
# http://jcr.sagepub.com/content/59/7/1216
options(scipen=4)
library(poweRlaw)

#source("code/clean_data.R") # Run twice because of error, takes some seconds
load("data/input.RData")
source("code/functions.R")

#InterPL(ged1) # Period Hagopian et al. 
InterPL(ged2) # Period Wikileaks
InterPL(wikileaks)
#InterPL(guardian)

# p-value on GED data
# Takes about 500 seconds
pl=displ$new(ged2) 
est=estimate_xmin(pl)
pl$setXmin(est);pl
bts=bootstrap_p(pl,no_of_sims=500,seed=42,threads=2);bts$p

# xmin=11, alpha=2.39, p-value=0.024
# p-value below thresholds: no power law relation

# Fit power law to wikileaks data to get p-value
# This takes about 1100 seconds
pl=displ$new(wikileaks) 
est=estimate_xmin(pl)
pl$setXmin(est);pl
bts=bootstrap_p(pl,no_of_sims=500,seed=42,threads=2);bts$p

# xmin=2, alpha=2.46, p-value=0
# Model is firmly rejected, casting doubts on the strenght of the estimations.


# Test with randomly sampling data
require(poweRlaw)
beta<-c()
p<-c()

for(i in 1:100){
  test<-sample(wikileaks,200,replace=FALSE)
  pl=displ$new(test) 
  est=estimate_xmin(pl)
  pl$setXmin(est)
  beta[i]<-pl$pars
  bts=bootstrap_p(pl,no_of_sims=100,seed=42,threads=2) 
  p[i]<-bts$p
}

par(las=1,bty="n",pty="s")
plot(p,beta,xlab="p-value",ylab="alpha parameter",xlim=c(0,1),ylim=c(2,3))
