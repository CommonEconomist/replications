# Replication "Contagious Rebellion and Preemptive Repression" | Only model 1
# Estimation using JAGS rather than BUGS
# date created 2014.05.14
# last update  2020.01.01
load("danneman-ritter.rda")
library(R2jags)

# MODEL
model1<-function(){
  for(i in 1:N){
    y[i] ~ dnorm(yhat[i], tau)
    yhat[i] <- b0[country[i]] + b1*w.pdH[i] + b2*intconflicthigh[i] +
      b5*var.uds[i] + b6*lciri[i] + b7*int[i]  
    
    lciri[i]~dnorm(3,1)        # missing data
    var.uds[i] ~ dnorm(0,1.3) # missing data
  }
  
  for(j in 1:J){                   # random intercepts
    b0[j]~dnorm(mu.b0[j], tau.b0)      
    mu.b0[j] <- b00 + b3*cpop[j] + b4*cgdp[j] # slow-moving covariates
  }
  b1~dnorm(0, .01)    # w.pdH
  b2~dnorm(0, .01)    # civil war - High
  b3~dnorm(0, .01)    # pop
  b4~dnorm(0, .01)    # GDP
  b5~dnorm(0, .01)    # UDS
  b6~dnorm(0, .01)    # lagged ciri
  b7~dnorm(0, .01)    # w.pdH X intconflictHigh
  
  b00~dnorm(0, .01)   # intercept on re
  
  
  
  tau.b0 <- pow(sd.b0, -2)
  sd.b0 ~ dunif(0,6)
  
  tau <- pow(sd, -2)
  sd ~ dunif(0, 20)
}
  

# ESTIMATE
yhat<-jags(data=c("N", "J", "y", "country", "w.pdH", "intconflicthigh", 
                     "cpop", "cgdp", "var.uds", "lciri", "int"),
           inits=NULL,
           parameters.to.save=
             c("b00", "b1", "b2", "b3", "b4", "b5", "b6", "b7"),
           model.file = model1,
           n.burnin=1000,
           n.thin=1,
           n.chains=3,
           n.iter=2500)

print(yhat, digits.summary=3)

## FIN