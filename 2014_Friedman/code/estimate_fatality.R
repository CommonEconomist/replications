## Test fatality interpolation for Iraq
# Using method from Friedman (2014)
# http://jcr.sagepub.com/content/59/7/1216
options(scipen=4)
library(poweRlaw)

#source("code/clean_data.R") # Run twice because of error, takes some seconds
load("data/input.RData")

# Fit power law
x<-ged2
pl=displ$new(x)
est=estimate_xmin(pl)
pl$setXmin(est);pl
bts<-bootstrap(pl,no_of_sims=1000,threads=3)

alpha<-bts$bootstraps$pars
xmin<-bts$bootstraps$xmin

#### Estimate number of fatalities ####
N<-length(alpha)
mort<-c()
a=1:max(x)
n=1:max(x)

for(y in 1:N){
  print(y)
  Xmin=xmin[y]
  Alpha=alpha[y]
  
  for(i in 1:length(n)){
    n[i]=sum(x==i,na.rm=TRUE)
  }
  # Set parameters (can take a few moments)
  if (Xmin>1){
    S=cumsum(n)
    lambda=S[length(S)]-S[Xmin-1]
    K<-99999999
    Z<-sum(1/(0:K+Xmin)^Alpha) # Hurwitz zeta normalisation constant
  }
  
  # Estimate number of incidents and fatalities
  Proj<-c() 
  for (j in 1:Xmin-1){
    Proj[j]=lambda*a[j]^((-1)*Alpha)*1/Z
  }
  
  R<-a[1:length(Proj)]*Proj # Number of fatalities
  mort[y]=round(sum(x[x>=Xmin])+sum(R),digits=2)
}




