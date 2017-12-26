## Replication "Using Power Laws to Estimate Conflict Size"
# http://jcr.sagepub.com/content/59/7/1216
library(haven)
library(poweRlaw)

dat<-read_dta("data/American Indian Wars event data.dta")
x<-dat$recordeduscasualties # Change data according to taste

#### Fit power law to data ####
pl=displ$new(na.omit(x)) 
est=estimate_xmin(pl)
pl$setXmin(est)
plot(pl);lines(pl,col="red") # Visual inspection

Xmin<-pl$xmin
alpha<-pl$pars

#### Calculate missing values ####

## Indices
a=1:max(x,na.rm=TRUE)
n=1:max(x,na.rm=TRUE)
 
## Calculate frequency per event size
for(i in 1:length(n)){
  n[i]=sum(x==i,na.rm=TRUE)
}
# NB - Some small discrepancies here for native American fatalities

## Set parameters
if (Xmin>1){
  S=cumsum(n)
  lambda=S[length(S)]-S[Xmin-1] # Need to find out what lambda actually is
  K<-99999999
  Z<-sum(1/(0:K+Xmin)^alpha) # Hurwitz zeta normalisation constant
}

## Estimate number of incidents and fatalities
# Slight differences can occur. 
Proj<-c() 
for (j in 1:Xmin-1){
  Proj[j]=lambda*a[j]^((-1)*alpha)*1/Z
}

R<-a[1:length(Proj)]*Proj # Number of fatalities

## FIN