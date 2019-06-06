# "The Waning of the Little Ice Age: Climate Change in Early Modern Europe"
# Kelly and Ó Gráda
# http://www.mitpressjournals.org/doi/abs/10.1162/JINH_a_00573
# https://muse.jhu.edu/article/526410/pdf
# Reconstructing figure 1,2 and part of 3 from paper.
# Data on temperature in the Netherlands 1301-2000 
# from van Engelen, Buisman, and Ijnsen
# http://projects.knmi.nl/klimatologie/daggegevens/antieke_wrn/index.html
# Data on English Wheat prices from Clarke
# http://faculty.econ.ucdavis.edu/faculty/gclark/data.html

# Load data
weather<-read.csv("millenium_of_weather.csv")
wheat<-read.csv("wheat.csv")

#### Fig.1 English wheat prices vs. Dutch summer temperature ####
# Year: 1211-1500
# Temperature average over two years previous to wheat price. 
# Divide T by 10. 
require(dplyr)
f<-data.frame(year=weather$YYYY,temp=weather$T_sum/10)
f$temp.l<-lag(f$temp,n=1)
f$temp.l2<-lag(f$temp,n=2)

f$temp.s=rowMeans(f[,c("temp","temp.l","temp.l2")],na.rm=TRUE)
f$temp.s.l=lag(f$temp.s,n=1)

# Add wheat price data
d<-merge(wheat,f[,c("year","temp.s.l")],all.x=TRUE)
d<-d[d$year>=1211 & d$year<=1500,]

## Plot data
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2,pty="s")
plot(d$temp.s.l,d$wheat,pch=19,xlab="Summer temperature",ylab="Wheat price",
     axes=FALSE)
axis(1,tick=FALSE,line=-1);axis(2,tick=FALSE,line=-1)
abline(lm(d$wheat~d$temp.s.l),lwd=2)

# Almost the same

#### Fig.2 The LIA as Slutsky effect ####
# Summer temperature in the Netherlands 1301-1980

# Data to time-series
require(zoo)
temp<-ts(weather$T_sum,start=c(751,1),frequency=1)/10
temp.ma<-rollapply(temp,25,mean,na.rm=TRUE) # 25-year moving average

# Subset period
LIA<-window(temp,start=c(1301,1),end=c(1980,1))
LIA.ma<-window(temp.ma,start=c(1301,1),edn=c(1980,1))

# Mean-center data
LIA.c<-LIA-mean(LIA,na.rm=TRUE)
LIA.ma.c<-LIA.ma-mean(LIA.ma,na.rm=TRUE)

# Plot data
par(mar=c(3,6,3,2),mfrow=c(2,1),las=1,bty="n",cex.axis=1.5,cex.lab=1.5,pty="m")
plot(LIA.ma.c,type="l",lwd=2,xlab="",ylab="",axes=F,ylim=c(-.45,.55))
abline(h=0,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

plot(LIA.c,type="l",lwd=2,xlab="",ylab="",axes=F,ylim=c(-2.6,2.3))
abline(h=0,lty=2)
axis(1,tick=FALSE);axis(2,tick=FALSE)

#### Fig.3 Historical series of Dutch summer temperature ####
# Smoothed using Bayesian Change Point Procedure
# Note that the time-series contains NAs which doesn't go well with the bcp.
# Therefore some adjustments need to be made.
require(bcp)

id.na<-which(is.na(LIA.c)) # Years with NA
s.T<-as.vector(LIA.c)
s.T<-as.vector(na.omit(s.T)) # Can't be done in one step for some reason

# Estimate change points
bcp.s.T<-bcp(s.T)
pm<-as.vector(bcp.s.T$posterior.mean)

# Adjust time series including NAs
pm2<-c(NA,pm[1:6],NA,NA,pm[7:8],NA,pm[9:14],NA,NA,pm[15:16],NA,pm[17:19],NA,
       pm[20:37],NA,pm[38:53],NA,pm[54:85],NA,pm[86:134],NA,pm[135:668])
pm2<-ts(pm2,start=c(1301,1))

# Plot data
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2,pty="m")
plot(LIA.c,type="p",axes=FALSE,xlab="",ylab="")
lines(pm2,lwd=2,col="firebrick3")
axis(1,tick=FALSE);axis(2,tick=FALSE)

# Slight difference in the smoothed line; conclusions stays the same though.