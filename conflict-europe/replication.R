#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Replication Tol and Wagner (2009)
# "Climate change and violent conflict in Europe over the last millennium"
# https://link.springer.com/article/10.1007/s10584-009-9659-2
# Data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/14818
# Last update: 2018 06 13
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replications/2009_tol_wagner')
par(mar=c(5,5,2,2),bty='n',las=1,cex.lab=2,cex.axis=2)

# Libraries
library(DataCombine)
library(smoother)

d<-read.csv('data_tol_wagner_2009.csv') # Data

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Figure 1
plot(d$Year,d$War,type='h',xlab='year',ylab='number of conflicts',axes=F)
axis(1,tick=F);axis(2,tick=F)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Figure 3 
# Data preparation: standardised time series
# War
w<-ts((d[,2]-mean(d[,2],na.rm=T))/sd(d[,2],na.rm=T),start=1000)
w<-ts(smth.gaussian(w,window=10),start=1000)
w<-window(w,start=1500,end=1900)

# Temperature
t<-ts((d[,3]-mean(d[,3],na.rm=T))/sd(d[,3],na.rm=T),start=1000)
t<-ts(smth.gaussian(t,window=10),start=1000)
t<-window(t,start=1500,end=1900)

# Precipitation
p<-ts((d[,4]-mean(d[,4],na.rm=T))/sd(d[,4],na.rm=T),start=1000)
p<-ts(smth.gaussian(p,window=10),start=1000)
p<-window(p,start=1500,end=1900)

# Plot 
plot(w,ylim=c(-2,2),lwd=2,col='steelblue4',axes=F,xlab='year CE',ylab='Stdev')
lines(t,lwd=2,col='firebrick3')
lines(p,lwd=2,col='chartreuse4')
axis(1,tick=F);axis(2,tick=F,line=-1)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) Regression analysis
# Data preparation
d<-slide(d,Var='War',NewVar='War.l',slideBy=-1)
d<-slide(d,Var='War',NewVar='War.l2',slideBy=-2)
d$Luther<-ifelse(d$Year>=1517,1,0)

# Estimate model (unable to replicate model 3 and 4)
m1<-lm(War~Mann_2003,d);summary(m1)
m2<-lm(War~Mann_2003+War.l+War.l2,d);summary(m2)
m5<-lm(War~poly(Time,2)+Mann_2003+Luther+War.l+War.l2,d);summary(m5)

# Discrepancies with model 5

## FIN