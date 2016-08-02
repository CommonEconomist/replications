## Replication "Location, Location, Location: An MCMC Approach to Modeling the Spatial Context of War and Peace"
# By Ward and Gleditsch (2002)
# http://pan.oxfordjournals.org/content/10/3/244.abstract
setwd("2002_Ward_Gleditsch")

#------------------------------------------------------------------------------
#### 1) Construct distance based adjacency matrix ####
require(cshapes)
dat88<-read.csv("inputdata.csv")
shp88<-cshp(date=as.Date("1988-12-31"))

## Subset data to included countries
#setdiff(dat88$cownr,shp88$COWCODE) # 426 is Niger
shp88$COWCODE<-replace(shp88$COWCODE,shp88$COWCODE==436,426)
shp<-shp88[shp88$COWCODE %in% dat88$cownr,]

## Create distance matrix
require(rgeos);require(fields)
shp<-shp[order(shp$COWCODE),]  # Shapefile and data, identical order
cent<-gCentroid(shp,byid=TRUE) # Get centre points
mat88<-as.matrix(rdist.earth(cbind(cent$x,cent$y),miles=F,R=NULL))

#------------------------------------------------------------------------------
#### 2) Prepare input data ####
# Description from original code:
# d is polity 98 democracy - autocracy score
# d.s is average of democracy scores among neighbors within 475 km
# cwar is Correlates of War civil war
# iwar is Correlates of War interstate war

## Create outcome variable
y<-as.numeric(dat88$cwar==1|dat88$iwar==1) 
x<-as.matrix(dat88[,c("d","d.s")])

## Create a binary contiguity matrix from mat88 with 475Km threshold
W<-ifelse(mat88<=475,1,0)
diag(W)<-0 # Set diagonal to 0, can't be your own neighbour
W.y<-as.vector(W%*%y) # Spatial lag of outcome
N=length(y)

#------------------------------------------------------------------------------
#### 3) Define regression models ####

## Logit model 
M1<-function () {
  for(i in 1:N) {
    incidence[i]~dbern(yhat[i])
    logit(yhat[i])<-a0 + b1*d[i] + b2*dW[i] 
    fit[i]<-yhat[i] # Predicted values
  } 
    # Priors
    a0~dnorm(0,0.01)
    b1~dnorm(0,0.01)
    b2~dnorm(0,0.01)
}

## Autologistic model 
M2<-function () {
  for(i in 1:N) {
    incidence[i]~dbern(yhat[i])
    logit(yhat[i])<-a0 + b1*d[i] + b2*dW[i] + b3*Wy[i]
    fit[i]<-yhat[i] # Predicted values
  } 
    # Priors
    a0~dnorm(0,0.01)
    b1~dnorm(0,0.01)
    b2~dnorm(0,0.01)
    b3~dnorm(0,0.01)
}

#------------------------------------------------------------------------------
#### 4) Fit models ####
set.seed(42); runif(1)
options(scipen=4)

## Pseudolikelihood estimates
require(MASS)
psi<-glm(y~x+W.y,family=binomial(link=logit))

## Logit model 
require(rjags);require(R2jags)
set.seed(42);m1<-jags(data=list(incidence=y,d=x[,1],dW=x[,2],N=N),
         inits=NULL,model.file=M1,
         parameters.to.save=c("a0","b1","b2","fit"),
         n.chains=3,n.iter=1000,n.burnin=100,n.thin=2)

## Autologit model
set.seed(42);m2<-jags(data=list(incidence=y,d=x[,1],dW=x[,2],Wy=W.y,N=N),
         inits=NULL,model.file=M2,
         parameters.to.save=c("a0","b1","b2","b3","fit"),
         n.chains=3,n.iter=1000,n.burnin=100,n.thin=2)

#------------------------------------------------------------------------------
#### 5) Checking empirical results ####

## Table 1: Regression results 
summary(psi)
print(m1$BUGSoutput$summary[1:3,c(1:3,7:8)],digits.summary=3)
print(m2$BUGSoutput$summary[1:4,c(1:3,7:8)],digits.summary=3)

# The estimated coefficients are almost similar, at least if we consider the 
# reported standard errors. 
# The only large discrepancy is the estimated effect of the spatial lag of 
# conflict incidence. 

## Table 2: actual and predicted conflict in 1988
require(jagstools)
fit2<-data.frame(jagsresults(m2,"fit"))
predict<-ifelse(fit2$mean>0.5,1,0)
xtabs(~predict+y) 

# No fitted values >0.5 in the autologistic model 

## Figure 1: Density function predicted probability of conflict
par(mar=c(5,5,1,2),cex.lab=1.5,cex.axis=1.5,las=1,pty="m")
plot(density(fit2$mean),axes=FALSE,bty="n",lwd=2,
     xlab="Predicted probability",ylab="Density",main="")
axis(1,tick=FALSE)

# The shape is similar to the one shown in the paper. 
# Only more wobbely around the 0.3-0.5 interval. 

## Table 3: actual and predicted conflict in 1988, different cutoff
predict<-ifelse(fit2$mean>0.35,1,0)
xtabs(~predict+y) 

## Figure 2: ROC plots
require(pROC)
roc_psi=roc(y,fitted(psi))
fit1<-data.frame(jagsresults(m1,"fit"));roc_logit=roc(y,fit1$mean)
roc_alogit=roc(y,fit2$mean)

# Plot results 
par(mar=c(5,5,2,2),pty="s",las=0,cex.axis=1.5,cex.lab=1.5)
plot(roc_psi,axes=FALSE,xlab="",ylab="",main="",lty=3)
plot(roc_logit,add=TRUE,lty=2)
plot(roc_alogit,add=TRUE,lty=1)
axis(1,tick=FALSE);mtext("Specificity",side=1,line=3,cex=1.5)
axis(2,tick=FALSE,las=1);mtext("Sensitivity",side=2,line=3,cex=1.5)
legend("bottomright",legend=c("Pseudo Likelihood","Logit","MCMC-ML"),
       col=c("black","black","black"),lty=c(3,2,1),text.width=.3,
       bty="n",lwd=2,y.intersp=c(0.3),x.intersp=c(0),xjust=1)

# Rather different from the one reported in terms of predictive performance
# NB - I haven't figured out how to plot it with predicted percentages yet. 

#------------------------------------------------------------------------------
#### 6) Out-of-sample forecasting ####
# In the original paper they generate out-of-sample forecasts using only 
# the 1988 and run simulations with the estimated coefficients. 
# Try to recreate that here. 

# Estimated coefficients
coef<-cbind(m2$BUGSoutput$sims.list$a0,
            m2$BUGSoutput$sims.list$b1,
            m2$BUGSoutput$sims.list$b2,
            m2$BUGSoutput$sims.list$b3)

## Starting values
yT<-y
etaT<-yT-yT
phatT<-yT-yT

## Simulations
# I depart a bit here from what's in the original code. 
# The original code simply seems to calculate 'etaT'and 'phatT' N times. 
# I'll use the distribution of the the estimated parameters. 
index<-1:length(yT)
D<-1000
set.seed(42);k<-ceiling(runif(D,1,1350))
R<-matrix(nrow=D,ncol=length(phatT)) # Matrix for results

for (i in 1:D) {
  for (j in index) {
    etaT[j]<-(exp(coef[k[i],1]+coef[k[i],2]*x[j,1]+coef[k[i],3]*x[j,2]+
                    coef[k[i],4]*W.y[j])) 
    phatT[j]<-etaT[j]/(1+etaT[j])
  } 
  R[i,]<-phatT
}

## Merge data
forecast<-data.frame(cowid=dat88$cowid,yhat=colMeans(R))
war89to98<-read.csv("war89to98.csv") # Observed data
colnames(war89to98)[1]<-"cowid"
forecast<-merge(forecast,war89to98)

## Table 4: Out-of-sample forecasts
predict<-ifelse(forecast$yhat>=0.35,1,0)
xtabs(~predict+forecast$war89to98) # Not so good, should use other treshold

#------------------------------------------------------------------------------
#### 7) Test with optimal cut-off point ####

## For 1988
coords(roc_alogit,"best") # Treshold at 0.22
predict<-ifelse(fit2$mean>0.2240776,1,0)
xtabs(~predict+y) # 21 correct, 55 false +ve, 10 false -ve

## For 1989-1998
predict<-ifelse(forecast$yhat>0.2240776,1,0)
xtabs(~predict+forecast$war89to98) # 32 correct, 44 false +ve, 18 false -ve

