#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Replication Schularick & Taylor (2012)
# Credit Booms Gone Bust
# https://www.aeaweb.org/articles?id=10.1257/aer.102.2.1029
# Last update 2018 11 19
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Libraries
library(haven)
library(brms)
library(beepr)
library(data.table)
library(ROCR)
library(caTools)

x<-read_dta('~/github/replications/schularick-taylor/panel17.dta')

# 1) Data preparation - according to original script
x$loans1[x$iso=='FRA' & x$year<=1895]<-NA # drop French loans before 1895
x<-data.table(x[x$year<2009,])

# Time-series for figure 1 and table 3
x[,':='(loansgdp=loans1/gdp,
        credgdp=bassets2/gdp,
        moneygdp=money/gdp,
        lpc=log(cpi),
        lloans=log(loans1))]

# 2) Figure 1
# 2.1) Estimate models
m<-brm(loansgdp~(1|iso+year),x);beep(2)
y1<-ts(ranef(m)$year[1:138],start=1870)

m<-brm(credgdp~(1|iso+year),x);beep(2)
y2<-ts(ranef(m)$year[1:138],start=1870)

m<-brm(moneygdp~(1|iso+year),x);beep(2)
y3<-ts(ranef(m)$year[1:138],start=1870)

# 2.2) Plot results : trends are similar, shift in magnitude
par(bty='n',las=1)
plot(y1,ylim=c(min(y1,y2,y3),max(y1,y2,y3)),type='p',pch=0)
lines(y2,type='p',pch=1)
lines(y3,type='p',pch=2)

# 3) Table 3
# 3.1) Prepare data
x<-x[,c('iso','year','crisisST','lloans','lpc')]

# (!) Need to set observations during the world wars to NA
x$war<-ifelse(x$year>=1914 & x$year<=1919 | x$year>=1939 & x$year<=1947,1,0)
x$lloans[x$war==1]<-NA
x$lpc[x$war==1]<-NA

# Log differences
x[,':='(dlloans=c(NA,diff(lloans)),
        dlpc=c(NA,diff(lpc))),by=iso]
x$dlloansr<-x$dlloans-x$dlpc

# Lag variable
x[,':='(L.dlloansr=shift(dlloansr,n=1,type='lag'),
        L2.dlloansr=shift(dlloansr,n=2,type='lag'),
        L3.dlloansr=shift(dlloansr,n=3,type='lag'),
        L4.dlloansr=shift(dlloansr,n=4,type='lag'),
        L5.dlloansr=shift(dlloansr,n=5,type='lag')),by=iso]
x<-na.omit(x)

# 3.2) Estimate models
ols1<-brm(crisisST~L.dlloansr+L2.dlloansr+L3.dlloansr+L4.dlloansr+
         L5.dlloansr,x);beep(3)
ols2<-brm(crisisST~L.dlloansr+L2.dlloansr+L3.dlloansr+L4.dlloansr+
          L5.dlloansr+(1|iso),x);beep(3)
ols3<-brm(crisisST~L.dlloansr+L2.dlloansr+L3.dlloansr+L4.dlloansr+
          L5.dlloansr+(1|iso+year),x);beep(3)
logit1<-brm(crisisST~L.dlloansr+L2.dlloansr+L3.dlloansr+L4.dlloansr+
              L5.dlloansr,x,
            family=bernoulli(link = "logit"));beep(3)
logit2<-brm(crisisST~L.dlloansr+L2.dlloansr+L3.dlloansr+L4.dlloansr+
          L5.dlloansr+(1|iso),x,
          family=bernoulli(link = "logit"),
          control = list(adapt_delta = 0.99));beep(3)

# 3.3) Results: similar results
print(ols1)
print(ols2)
print(ols3)
print(logit1)
print(logit2)

# 4) Receiver Operating Characteristic Curve
obs<-logit2$data[,1]
yhat<-fitted(logit2)[,1]

pred<-prediction(yhat,obs)
roc<-performance(pred,'tpr','fpr')
prc<-performance(pred,"prec","rec")

trapz(roc@x.values[[1]][-1],roc@y.values[[1]][-1]) # 0.68 -> lower
trapz(prc@x.values[[1]][-1],prc@y.values[[1]][-1]) # 0.09

## FIN