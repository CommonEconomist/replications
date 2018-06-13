# "Taking Time Seriously"
# de Boef & Keele (2008)
# http://onlinelibrary.wiley.com/doi/10.1111/j.1540-5907.2007.00307.x/abstract
# Replication of simulated example (table 3) illustrating equivalnece ADL/ECM

#### Data generating process ####
# Autocorrelation is set to 0.75, a0=0, a1=0.75, b0=0.50, b1=0.25
N=1000
x<-c();y<-c()

# Starting values
set.seed(42)
x[1]=.75*100+rnorm(1)
y[1]=0+.75*100+.5*x[1]+.25*100+rnorm(1)

for(i in 2:N){
  x[i]<-.75*x[i-1]+rnorm(1)
  y[i]=0+.75*y[i-1]+.5*x[i]+.25*x[i-1]+rnorm(1)
}

# Data to data frame
require(dplyr)
d<-data.frame(y,x)

d$y.l<-lag(d$y,n=1)
d$x.l<-lag(d$x,n=1)

d$y.d<-d$y-d$y.l
d$x.d<-d$x-d$x.l
d<-na.omit(d)

#### Fit models ####

# ADL 
adl<-lm(y~y.l+x+x.l,d)
summary(adl)
c.adl<-as.vector(coef(adl))

# ECM
ecm<-lm(y.d~y.l+x.l+x.d,d)
summary(ecm)
c.ecm<-as.vector(coef(ecm))

# Check coefficients
c.adl[4]
c.ecm[3]-c.ecm[4]

k.adl=(c.adl[3]+c.adl[4])/(1-c.adl[2])
k.ecm=c.ecm[3]/-(c.ecm[2])
