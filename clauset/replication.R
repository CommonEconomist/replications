# Replication: Trends and fluctuations in the severity of interstate wars
# http://advances.sciencemag.org/content/4/2/eaao3580
# last update: 2019 06 04
setwd('~/github/replications/clauset')

# Libraries
library(plyr)
library(poweRlaw)

d<-read.csv('data_wars_CoWv4.txt', stringsAsFactors=FALSE,,sep='\t')

# Plot: figure 1
par(mar=c(5,5,2,2),las=1,bty='n')
plot(d$year,log10(d$battle_deaths),axes=F,xlab='',ylab='Severity')
axis(1,tick=F);axis(2,tick=F)

# Fit power law model to data
pl=displ$new(d$battle_deaths)
pl$setXmin(estimate_xmin(pl,xmax=1e8))
pl$setPars(estimate_pars(pl))
pl #a=1.51,xmin=6252 -> Small discrepancy

# Plot: figure 2
par(pty='s')
plot(pl,axes=F,
     xlab='Battle deaths (in thousands), X',
     ylab='Fraction of wars with at least X deaths')
lines(pl,col='red',lwd=1.5) # Takes couple of seconds
axis(1,tick=F,at=c(1e3,1e4,1e5,1e6,1e7),label=c(1,10,100,1000,10000))
axis(2,tick=F)

# Calculate uncertainty (inset figure 2: Density alpha) 
pl.u<-bootstrap(pl,no_of_sims=1000,threads=3,xmax=1e8) #~ 12h

# Plot
par(pty='m')
plot(density(pl.u$bootstraps$pars),axes=F,
     main='',xlab='Power-law exponent',ylab='Density')
abline(v=pl$pars,lwd=2)
axis(1,tick=F)

# Plausability of model
pl.p=bootstrap_p(pl,no_of_sims=1000,seed=42,threads=3)
pl.p$p

## FIN