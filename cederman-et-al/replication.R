#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Replication Cederman et al. (2011)
# "Testing Clausewitz: Nationalism, Mass Mobilization, and the Severity of War."
# https://icr.ethz.ch/publications/testing-clausewitz/
# Last update 2018 07 09
# NB - Does not replicate
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
d<-read.csv('~/github/replications/cederman-et-al/major-power-wars.csv')
library(poweRlaw)

# 1.1) Fit power law: 1495-1789
p0=displ$new(d$battle_deaths[d$year<1789 & d$battle_deaths>=1e4])
p0$setXmin(estimate_xmin(p0,xmax=1e8))
p0$setPars(estimate_pars(p0))
p0 # Reported value: a=0.65 -> Does not replicate (1.8, 22000)

# 1.2) Fit power law: 1789-1997
p1=displ$new(d$battle_deaths[d$year>=1789 & d$battle_deaths>=1e4])
p1$setXmin(estimate_xmin(p1,xmax=1e8))
p1$setPars(estimate_pars(p1))
p1 # Reported value: a=0.35 -> Does not replicate (1.4, 12000)

# 2) Plot results
par(mar=c(5,5,2,2),bty='n',las=1)
plot(p0,pch=4,axes=F,xlab="War severity x, in thousands",
     ylab=expression(paste("Pr (",'X'>='x',")")),xlim=c(1e4,3e7))
lines(p0,lwd=1.5)
par(new=TRUE)
plot(p1,pch=5,axes=F,xlab='',ylab='',xlim=c(1e4,3e7))
lines(p1,lwd=1.5)

# Axis
axis(1,tick=F,at=c(1e4,1e5,1e6,1e7),label=c(10,100,1000,10000))
axis(2,tick=F,line=-1.5)

## FIN