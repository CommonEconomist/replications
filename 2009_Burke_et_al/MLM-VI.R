#******************************************************************************
##### Multilevel modeling ####
## Test of using multilevel models using the data from Burke et al. (2009)
## http://www.pnas.org/content/106/49/20670
## Trying to replicate the results in O'Loughlin et al. (2014), model 3
## http://www.pnas.org/content/111/6/2054
## Note: reported results are the following (se between parentheses).
# "within" is simply the demeaned variable, "between" the country average
# Intercept                9.668 (8.248)
# Temperature within       0.045 (0.023)
# Temperature lag within   0.013 (0.024)
# Rainfall within          0.014 (0.072)
# Rainfall lag within      0.029 (0.073)
# Year indicator          -0.005 (0.004)
# Temperature between      0.384 (1.557)
# Temperature lag between -0.392 (1.561)
# Rainfall between         1.420 (3.386)
# Rainfall lag between    -1.456 (3.394)
#******************************************************************************

rm(list=ls(all=TRUE)) # Clear workspace
options(scipen=4)     

#### LOAD ####

## Libraries
library(jagstools)
library(lme4)
library(PerformanceAnalytics)
library(plyr)
library(R2jags)
library(reshape)
library(rjags)

## Data
pnas<-read.dta("climate_conflict.dta")

#### CLEAN ####

## Subset the data removing NAs
pnas<-pnas[pnas$year_actual<=2002,] 
pnas<-pnas[order(pnas$ccode,pnas$year),]

## Calculate mean of climate variables
c.mean<-ddply(pnas,.(ccode), summarize,
         temp.m=mean(temp_all),
         temp.ml=mean(temp_all_lag),
         prec.m=mean(prec_all),
         prec.ml=mean(prec_all_lag))

# Country-level variables (Note: These are highly correlated)
tempm<-c.mean[,2]
tempm.l<-c.mean[,3]
rainm<-c.mean[,4]
rainm.l<-c.mean[,5]

## Create variable vectors (country-year level)
incidence<-pnas$war_prio_new # Outcome variable

temp<-pnas$temp_all          
temp.l<-pnas$temp_all_lag 
rain<-pnas$prec_all         
rain.l<-pnas$prec_all_lag    

# Indices
C<-as.numeric(factor(pnas$ccode))        # Country indicator
Y<-as.numeric(factor(pnas$year_actual))  # Year indicator

J<-length(unique(C)) 
N<-length(C)

#### ANALYSIS ####

## JAGS estimation: Linear model based on Bell & Jones (2015).
# Model specification based on Bafumi & Gelman (2006).
# This means that I just include the country-mean at the
# varying intercept level and don't have to demean the variables

# Model function
M <- function() {
  for(i in 1:N) {
    incidence[i] ~ dnorm(yhat[i],tau)
    yhat[i] <- b0[C[i]] + b1*temp[i] + b2*temp.l[i] + 
      b3*rain[i] + b4*rain.l[i] + b5*Y[i] 
          
    # Predicted values
    fit[i] <-yhat[i]
  }
   
  for (j in 1:J) {
    b0[j] ~ dnorm(mu.b0[j], tau.b0)
    mu.b0[j]<-b00 + b6*tempm[j] + b7*tempm.l[j] + b8*rainm[j]+ b9*rainm.l[j]   
    
  }
   # Priors and hyperpriors
  b1 ~ dnorm(0, 0.001)   
  b2 ~ dnorm(0, 0.001)
  b3 ~ dnorm(0, 0.001)
  b4 ~ dnorm(0, 0.001)
  b5 ~ dnorm(0, 0.001)
  b6 ~ dnorm(0, 0.001)
  b7 ~ dnorm(0, 0.001)
  b8 ~ dnorm(0, 0.001)
  b9 ~ dnorm(0, 0.001)
  b00~ dnorm(0,  0.001)
  
  tau <- pow(sd, -2)
  sd ~ dunif(0,100)  
  tau.b0 <- pow(sd.b0, -2)
  sd.b0 ~ dunif(0,100)  
}

## Estimate model
dat<-list(N=N,Y=Y,C=C,J=J,
          incidence=incidence,temp=temp,temp.l=temp.l,rain=rain,rain.l=rain.l,
          tempm=tempm,tempm.l=tempm.l,rainm=rainm,rainm.l=rainm.l)
m1<-jags(data=dat,inits=NULL,model.file=M,
         parameters.to.save=c('b00','b1','b2','b3','b4','b5','b6','b7','b8','b9'),
         n.chains=3,n.iter=10000,n.burnin=5000,n.thin=4)
print(m1,digits.summary=3);plot(m1)

## Fitted values
m.fit<-jags(data=dat,inits=NULL,model.file=M,
         parameters.to.save=c('fit'),
         n.chains=3,n.iter=10000,n.burnin=5000,n.thin=4)

## Plot predicted values
df<-data.frame(incidence=incidence,
               temp=round(temp-tempm,2),
               pr=m.fit$BUGSoutput$summary[-1,1],
               country=pnas$country,
               year=Y)

## Plot
par(pty="s",las=0,family="serif")
plot(df$temp,df$pr,axes=FALSE,ylab="",xlab="",pch=1,xlim=c(-2,2),cex=.8,col="gray20")
points(df[df$incidence==1,]$temp,df[df$incidence==1,]$pr,col="black",pch=19,cex=1)

# Axis
axis(1,at=seq(-2,2,.5),tick=FALSE,cex.axis=1.2)
axis(2,at=seq(-.1,.9,.1),tick=FALSE, cex.axis=1.2,las=1,line=1)
mtext("Deviation from mean temperature",side=1,line=3,cex=1.5)
mtext("Predicted probability",side=2,line=5,cex=1.5)

roc(df$incidence,df$pr) # 791 controls, 98 cases; AUC=0.9477

## Plot per country
fig<-df[,c("pr","country","year")]
md<-melt(fig,id=c("country","year"))
k<-cast(md,variable+year~country)
k<-k[,-1:-2]

chart.Boxplot(k, main = "", xlab="Distribution fitted values per country", ylab="", 
              element.color = "transparent", as.Tufte=TRUE)


