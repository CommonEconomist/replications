#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Replication: Trends and fluctuations in the severity of interstate wars
# http://advances.sciencemag.org/content/4/2/eaao3580
# Last update: 2018 02 23
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
par(mar=c(5,5,2,2),las=1,bty='n')
setwd('~/github/replications/clauset')
library(plyr)
library(poweRlaw)
d<-read.csv('data_wars_CoWv4.txt', stringsAsFactors=FALSE,,sep='\t')

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Time-series plot
# Figure 1 in paper
plot(d$year,log10(d$battle_deaths),axes=F,xlab='',ylab='Severity')
axis(1,tick=F);axis(2,tick=F)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) Fit power law model to data
pl=displ$new(d$battle_deaths)
pl$setXmin(estimate_xmin(pl,xmax=1e8))
pl$setPars(estimate_pars(pl))
pl #a=1.51,xmin=6252 -> Small discrepancy

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3.1) Plot data with power law fit
# Figure 2
par(pty='s')
plot(pl,axes=F,
     xlab='Battle deaths (in thousands), X',
     ylab='Fraction of wars with at least X deaths')
lines(pl,col='red',lwd=1.5) # Takes couple of seconds
axis(1,tick=F,at=c(1e3,1e4,1e5,1e6,1e7),label=c(1,10,100,1000,10000))
axis(2,tick=F)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4) Calculate uncertainty
# Inset figure 2: Density alpha 
pl.u<-bootstrap(pl,no_of_sims=1000,threads=3,xmax=1e8) # Will take 12 hours

# Plot results
par(pty='m')
plot(density(pl.u$bootstraps$pars),axes=F,
     main='',xlab='Power-law exponent',ylab='Density')
abline(v=pl$pars,lwd=2)
axis(1,tick=F)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4.1) Plausability of model
pl.p=bootstrap_p(pl,no_of_sims=1000,seed=42,threads=3) # Also takes some time
pl.p$p

## FIN