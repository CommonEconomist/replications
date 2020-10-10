# Replication Killian (2009) 
# "Not all oil price shocks are alike"
# https://www.aeaweb.org/articles?id=10.1257/aer.99.3.1053
# using `vars` package
#http://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/vignettes/vars/vars.pdf
library(data.table)
library(vars)

#data
d <- fread("~/github/replications/kilian/data.csv", header = FALSE,
           col.names = c("d.prod","rea","rpo"))

x <- ts(d, start = c(1973,1), frequency = 12)
plot(x ,bty="n")

#fit reduced-form VAR (24 lags)
m <- VAR(x, p = 24) 

#construct impulse response function; from structural VAR
Am = diag(3) #restrictions
Am[2:3, 1] <- NA
Am[3,2] <- NA

s <- SVAR(m, estmethod = "direct", Amat = Am, Bmat = NULL, hessian = TRUE, 
          method = "BFGS")

#impulse response functions (can take some minutes)
irf.d.prod <- irf(s, impulse = "d.prod", 
                  ci = .5, runs =1000, seed = 42, n.ahead = 15)
irf.rea <- irf(s, impulse = "rea", 
               ci = .5, runs =1000, seed = 42, n.ahead = 15)
irf.rpo <- irf(s, impulse = "rpo", 
               ci = .5, runs =1000, seed = 42, n.ahead = 15)

#plot; compare with figure 3 in paper. 
plot(irf.d.prod) #?
plot(irf.rea)
plot(irf.rpo)