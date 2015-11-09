##### Robust clustered standard errors ####
# Source:
# http://people.su.se/~ma/mcluster.R
# http://people.su.se/~ma/clustering.pdf

# Script written by Mahmood Arai
clse <- 
  function(fm, dfcw, cluster){
    # R-codes (www.r-project.org) for computing
    # clustered-standard errors. Mahmood Arai, Jan 26, 2008.
    
    # The arguments of the function are:
    # fitted model, cluster1 and cluster2
    # You need to install libraries `sandwich' and `lmtest'
    
    # reweighting the var-cov matrix for the within model
    library(sandwich);library(lmtest)
    M <- length(unique(cluster))   
    N <- length(cluster)           
    K <- fm$rank                        
    dfc <- (M/(M-1))*((N-1)/(N-K))  
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)*dfcw
    coeftest(fm, vcovCL) }

#### Mean squared error ####
mse <- function(sm) { 
    mse <- mean(sm$residuals^2)
    return(mse)
}
