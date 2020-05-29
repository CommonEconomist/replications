# "The Waning of the Little Ice Age: Climate Change in Early Modern Europe"
# http://www.mitpressjournals.org/doi/abs/10.1162/JINH_a_00573
# https://muse.jhu.edu/article/526410/pdf
# Replicating Fig.1-3 from paper. 
# Temperature data Netherlands (1301-200) from van Engelen, Buisman, and Ijnsen
# http://projects.knmi.nl/klimatologie/daggegevens/antieke_wrn/index.html
# English wheat price data from Clarke
# http://faculty.econ.ucdavis.edu/faculty/gclark/data.html
# last update 2020.01.23
library(bcp)
library(data.table)
library(dplyr)
library(zoo)

# DATA
weather<-read.csv("milleniumOfWeather.csv")
wheat<-read.csv("pricesWheat.csv")

# FIG.1: English wheat prices vs. Dutch summer temperature (1211-1500)
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2,pty="s")

# Temperature average over two years previous to wheat price. 
# NB: Divide T by 10. 
x<-data.table(year = weather$YYY, temp = weather$T_sum/10)
x[, `:=`(L.temp = shift(temp, n=1, type='lag'),
         L2.temp = shift(temp, n=2, type='lag'))]
x[, temp.mean := rowMeans(x[,-1], na.rm = TRUE)]
x[, L.temp.mean := shift(temp.mean, n=1, type='lag')]

d<-merge(wheat, x[,c("year", "L.temp.mean")], all.x=TRUE)
d<-d[d$year>=1211 & d$year<=1500,]

# Plot
plot(wheat ~ L.temp.mean, d,
     xlab="Summer temperature",ylab="Wheat price", axes=FALSE)
axis(1,tick=FALSE,line=-1);axis(2,tick=FALSE,line=-1)
abline(lm(wheat ~ L.temp.mean, d), lwd=2)

# FIG.2: The LIA as Slutsky effect
par(mar=c(3,6,3,2), mfrow=c(2,1), pty="m")
# Summer temperature in the Netherlands 1301-1980
temp<-ts(weather$T_sum, start=c(751,1),frequency=1)/10
temp.ma<-rollapply(temp,25,mean,na.rm=TRUE) #25-year moving average

LIA<-window(temp,start=c(1301,1),end=c(1980,1)) #Little Ice Agre
LIA.ma<-window(temp.ma,start=c(1301,1),edn=c(1980,1))

# Mean-center data
LIA.c<-LIA-mean(LIA, na.rm=TRUE)
LIA.ma.c<-LIA.ma-mean(LIA.ma, na.rm=TRUE)

# Plot
plot(LIA.ma.c,type="l",lwd=2,xlab="",ylab="",axes=F,ylim=c(-.45,.55))
abline(h=0,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

plot(LIA.c,type="l",lwd=2,xlab="",ylab="",axes=F,ylim=c(-2.6,2.3))
abline(h=0,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

# FIG.3: Historical series of Dutch summer temperature
par(mar=c(5,5,2,2),mfrow=c(1,1))

# Smoothed using Bayesian Change Point Procedure
# NB - Need to account for NAs in data.
id.na<-which(is.na(LIA.c)) # Years with NA
s.T<-as.vector(LIA.c)
s.T<-as.vector(na.omit(s.T)) #can't be done in one step for some reason

# Estimate change points
bcp.s.T<-bcp(s.T)
pm<-as.vector(bcp.s.T$posterior.mean)

# Adjust time series including NAs
pm2<-c(NA, pm[1:6], NA, NA, pm[7:8], NA, pm[9:14], NA, NA,
       pm[15:16], NA, pm[17:19], NA, pm[20:37], NA, pm[38:53],
       NA, pm[54:85], NA, pm[86:134], NA, pm[135:668])
pm2<-ts(pm2,start=c(1301,1))

# Plot
plot(LIA.c,type="p",axes=FALSE,xlab="",ylab="")
lines(pm2,lwd=2,col="firebrick3")
axis(1,tick=FALSE);axis(2,tick=FALSE)