## Best-fit power-law progress curve for Afghanistan IEDs
# "Pattern in Escalations in Insurgent and Terrorist Activity"
# Johnson et al. (2011, Science)
# http://www.sciencemag.org/content/333/6038/81.abstract
# Last version:  2020.03.30
# First version: 2015.07.10
rm(list=ls(all=TRUE)) # Clear workspace
options(scipen=4)     
library(devtools)
source_url("https://raw.githubusercontent.com/sjmurdoch/fancyaxis/master/fancyaxis.R")

#data
d<-read.csv("afghanistan.csv",header=TRUE,stringsAsFactors=FALSE)
d<-na.omit(d) #remove last observation without tau

#create variables and indicators
#indicator: event number per province
d$i<-seq.int(nrow(d)) 
d$n<-as.integer(with(d,ave(i, Province,
                       FUN = function(x) cumsum(!duplicated(x)))))
d$j<-as.numeric(factor(d$Province))

N<-max(as.vector(unique(d$j))) #number of provinces
lbl<-as.vector(unique(d$Province))

#variables on log10 scale
d$n.n<-log10(d$n)
d$tau.n<-log10(d$Tau)

#estimate model
beta.hat<- list()
tau.1<-list()

for(i in 1:N){
  unit.lm <- lm(tau.n ~ n.n, data = d[d$j==i,] )
  beta.hat[i] <- coef(unit.lm)[2]
  tau.1[i] <- coef(unit.lm)[1]
}

beta.hat <- -as.numeric(beta.hat)
tau.1<- 10^as.numeric(tau.1)

#plot
par(mar=c(5,5,3,3),pty='m',las=1)
plot(tau.1,beta.hat,log= "x",tck=-.02,bty="n",pch=19,cex=1.2,xlim=c(10,1000),
     xlab="",ylab="",tck=.02,cex.lab=1.5,axes=FALSE)

# Axis
axis(1, tick=F)
axis(2, tick=F, las=2)
minimalrug(tau.1, side=1, line=-.8,lwd=2)
minimalrug(beta.hat, side=2, line=-0.8,lwd=2)
mtext(expression(beta),2,line=3,cex=1.5)
mtext(expression(tau),1,line=3,cex=1.5)

# Lines
abline(lm(beta.hat ~ log10(tau.1)),col="red",lwd=2) # Best-fit line
abline(h=.5,lty=2,lwd=2)

# Add labels
y.adjust<-c(.05,-.05,.05,.05,.05,
            .05,-.05,.05,.05,.05,
            .05,-.05,-.05,-.05,-.05,
            .05)
text(tau.1,beta.hat+y.adjust,lbl) # Labels

