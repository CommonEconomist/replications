# Albouy (2012) AER
# The Colonial Origins of Comparative Development: An Empirical Investigation: Comment
# https://www.aeaweb.org/articles?id=10.1257/aer.102.6.3059
# Testing IV using 'brms' package
# last update 2019.07.25
library(beepr)
library(brms)
library(haven)

x<-read_dta("~/github/replications/albouy/ajrcomment.dta")

# First stage | Table 2, Panel A,  Column 1
m1<-brm(risk~logmort0,x,iter=5e3,seed=42);beep(2)
summary(m1) # b=-0.61 (0.13)

# IV estimation | Table 3, Panel A, Column 1
s1<-bf(risk~logmort0) # first stage
s2<-bf(loggdp~risk)   # second stage
iv<-brm(s1+s2,x,iter=5e3,seed=42,control=list(adapt_delta=0.99));beep(2)
summary(iv) # b=-0.62 (0.13); b=0.92 (0.17)

# Including latitude as control
s1<-bf(risk~logmort0+latitude)
s2<-bf(loggdp~risk+latitude)
iv2<-brm(s1+s2,x,iter=5e3,seed=42,control=list(adapt_delta=0.99));beep(2)

## FIN