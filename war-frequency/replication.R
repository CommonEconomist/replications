# Replication Clauset (2018)
# "Trends and fluctuations in the severity of interstate wars"
# http://advances.sciencemag.org/content/4/2/eaao3580
# last update 2020.05.29
library(data.table)
library(poweRlaw)

d <- fread("~/github/replications/clauset/data_wars_CoWv4.txt")

#Fig.1
par(mar = c(5,5,2,2), las = 1, bty = 'n')
plot(battle_deaths ~ year, d, log = "y", type = "h", ylim = c(1e2,1e7),
     xlab = "", ylab = "Severity", axes = FALSE)
axis(1,tick=F);axis(2,tick=F)

#fit power law model to data
pl = displ$new(d$battle_deaths)
pl$setXmin(estimate_xmin(pl, xmax = 1e8)) #need to set xmax (~3m)
pl$setPars(estimate_pars(pl))

pl #alpha = 1.51, xmin = 6526: small discrepancy.

#Fig.2
par(pty='s')
plot(pl,axes=FALSE,
     xlab='Battle deaths (in thousands), X',
     ylab='Fraction of wars with at least X deaths')
lines(pl, col = 'red', lwd = 1.5)
axis(1, tick = FALSE, at=c(1e3,1e4,1e5,1e6,1e7), label=c(1,10,100,1000,10000))
axis(2, tick = FALSE)

#Fig.2-inset: Alpha density
bts <- bootstrap(pl, no_of_sims = 1000, threads = 3, xmax = 1e8) #~12h

par(pty='m')
plot(density(bts$bootstraps$pars), axes = FALSE,
     main='', xlab = 'Power-law exponent', ylab = 'Density')
abline(v = pl$pars, lwd = 2)
axis(1,tick = FALSE)
