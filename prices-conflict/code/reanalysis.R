##### Reanalysis Brückner & Ciccone (2010) ####
# International Commodity Prices, Growth and 
# the Outbreak of Civil War in Sub-Saharan Africa
# Updating and extending the data
# This version:  25-08-2015
# First version: 24-06-2015
setwd("/Replications/2010_Bruckner_Ciccone")
options(scipen=5)

## Libraries
library(countrycode)
library(devtools)
library(foreign)
library(stargazer)

## Data and functions
load("tidy_data/newData.Rdata")
df<-read.dta("raw_data/data.dta")
load("raw_data/ucdpConflict.rdata")
source("code/functions.R") 

## Create data frames
t1<-na.omit(df.New[,c(1,2,7:10,21)]) # Civil war
t2<-na.omit(df.New[,c(1,2,7:10,19)]) # Civil conflict
t3<-na.omit(df.New[,c(1,2,7:12,21)]) # Civil war, income
t4<-na.omit(df.New[,c(1,2,7:12,19)]) # Civil conflict, income
t0<-t1[t1$year<=2006,]               # Original period, most recent data

#### Descriptive statistics ####
stargazer(df.New[,c("war.onset","onset","ind","index.g","oecd.exp","gdp.g")],
          type="text",digits=2,median=TRUE)

## Conflict-onset years
onset<-df.New[,c("country","year","war.onset")]
onset<-na.omit(onset[onset$war.onset==1,])

sum(onset[onset$year<=2006,]$war.onset) # 27
sum(onset[onset$year>2006,]$war.onset)  # 5

#**************************************
##### Figure: conflict proportion #####
#**************************************

## Subset data
africa<-ucdpConflict[ucdpConflict$Region==4 & ucdpConflict$Type==3 | 
                       ucdpConflict$Region==4 & ucdpConflict$Type==4,]
africa$iso3c<-countrycode(africa$Location,"country.name","iso3c",warn=TRUE)

# Remove North African countries
NAf<-c("DZA","MAR","LBY","TUN")
africa$NAf<-as.numeric(africa$iso3c %in% NAf)
africa<-africa[africa$NAf==0,]

## Conflict/war indicators
africa$incidence<-1
africa$war<-as.numeric(africa$IntensityLevel==2)

##  Calculate proportion per year
d<-aggregate(cbind(incidence,war)~Year,
               africa[africa$Year>=1980,],FUN=sum) 
d$n.countries<-c(rep(46,10),rep(47,3),rep(48,18),rep(49,3))

d$civ.p<-d$incidence/d$n.countries 
d$war.p<-d$war/d$n.countries

## Plot
par(mar=c(3,6,5,3),family="serif")
plot(d$Year,d$civ.p,type="n",xlim=c(1978,2017),ylim = c(0,.37),
     axes=FALSE,xlab="",ylab="")

# Trimmings
rect(2007,0,2013,.35,col="grey90",lwd=0)                # New data period
lines(d$Year,d$civ.p,lwd=2,lty=1,col="black",type="b")  # Civil conflict 
lines(d$Year,d$war.p,lwd=2,lty=2,col="grey10",type="b") # Civil war

# Add labels
text(2015.5,d[34,5],labels="Civil war \n or conflict",cex=1.5)
text(2015.5,d[34,6],labels="Civil war",cex=1.5)

# Axis
axis(1,at=seq(1980,2015,5),tck=0.02, cex.axis=1.5,col="white")
axis(2,at=seq(0,.35,.05),las=1,tck=0.02,cex.axis=1.5,col="white")
text(1980,.36,"Proportion \n of countries",cex=1.5)

#**************************************
#### Figure: incidence and onset ####
#**************************************

## Aggregate data
d<-df.New[,c("year","country","war","war.onset")]
d[is.na(d)]<-0
d<-aggregate(cbind(war,war.onset)~year,d,FUN=sum)
year<-1981:2013

## Plot data
par(mar=c(3,5,3,3),family="serif")
plot(year,d$war,type="h",axes=F,xlab="",ylab="",
     col="gray80",ylim=c(0,8),lwd=20,lend=1)
par(new=TRUE)
plot(year,d$war.onset,type="h",xlab="",ylab="",col="grey20",
     axes=FALSE,ylim=c(0,8),lwd=20,lend=1)

# Axis
axis(1,las=1,at=seq(1980,2015,5),tck=0.02,cex.axis=1.5,col="white")
axis(2,las=1,at=seq(0,10,1),tck=0.02,cex.axis=1.5,col="white")
abline(h=seq(0,10,1), col="white",lwd=3)

# Legend
legend(2000,8,c("Ongoing wars","New wars"),
       cex=1.5,lwd=10,col=c("gray80","grey20"),bty="n",
       y.intersp=c(0.25),x.intersp=c(0.25))

#**************************************
#### TABLE 9 (annex) ####
# Commodity price shocks 
# and conflict onset
#**************************************

# Replication Table 2 column 1 1981-2006
m1o<-lm(war.onset~index.g+index.g.l+index.g.l2+
            factor(ccode)+factor(year)+factor(ccode)*year,t0)
clse(m1o,1,m1o$model[,5])
summary(m1o);mse(m1o)

# Replication Table 2 column 5 1981-2006
m5o<-lm(war.onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t0)
clse(m5o,1,m5o$model[,3])
summary(m5o);mse(m5o)

# Replication Table 2 column 1 1981-2013
m1<-lm(war.onset~index.g+index.g.l+index.g.l2+
            factor(ccode)+factor(year)+factor(ccode)*year,t1)
clse(m1,1,m1$model[,5])
summary(m1);mse(m1)

# Replication Table 2 column 5 1981-2013
m5<-lm(war.onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t1)
clse(m5,1,m5$model[,3])
summary(m5);mse(m5)

# Replication Table 2 column 1 1981-2013, civil conflict
m1a<-lm(onset~index.g+index.g.l+index.g.l2+
            factor(ccode)+factor(year)+factor(ccode)*year,t2)
clse(m1a,1,m1a$model[,5])
summary(m1a);mse(m1a)

# Replication Table 2 column 5 1981-2013, civil conflict
m5a<-lm(onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t2)
clse(m5a,1,m5a$model[,3])
summary(m5a);mse(m5a)

#**************************************
#### TABLE: IV-2SLS MAIN  ####
# Economic growth, export demand,
# and civil war onset 1981-2013 
#**************************************

# Column 1
w1<-lm(gdp.g~ind+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(w1,1,w1$model[,3])
summary(w1);mse(w1)

# Column 2
w2<-lm(gdp.g~ind+oecd.exp+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(w2,1,w2$model[,4])
summary(w2);mse(w2)

# Column 3
w3<-lm(war.onset~gdp.g+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(w3,1,w3$model[,3])
summary(w3);mse(w3)

# Column 4
w4<-lm(war.onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(w4,1,w4$model[,3])
summary(w4);mse(w4)

# Column 5
w5<-lm(war.onset~ind+oecd.exp+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(w5,1,w5$model[,4])
summary(w5);mse(w5)

# Column 6
t3$yhat<-predict(w1)
iv1<-lm(war.onset~yhat+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(iv1,1,iv1$model[,3])
mse(iv1)

# Column 7
t3$yhat<-predict(w2)
iv2<-lm(war.onset~yhat+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(iv2,1,iv1$model[,3])
mse(iv2)

#**************************************
#### TABLE: IV-2SLS MAIN  ####
# Economic growth, export demand,
# and civil war onset 1981-2013 
#**************************************

# Column 1
v1<-lm(gdp.g~ind+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(v1,1,v1$model[,3])
summary(v1);mse(v1)

# Column 2
v2<-lm(gdp.g~ind+oecd.exp+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(v2,1,v2$model[,4])
summary(v2);mse(v2)

# Column 3
v3<-lm(onset~gdp.g+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(v3,1,v3$model[,3])
summary(v3);mse(v3)

# Column 4
v4<-lm(onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(v4,1,v4$model[,3])
summary(v4);mse(v4)

# Column 5
v5<-lm(onset~ind+oecd.exp+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(v5,1,v5$model[,4])
summary(v5);mse(v5)

# Column 6
t4$yhat<-predict(v1)
iv3<-lm(onset~yhat+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(iv3,1,iv3$model[,3])
mse(iv3)

# Column 7
t4$yhat<-predict(v2)
iv4<-lm(onset~yhat+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(iv4,1,iv4$model[,3])
mse(iv4)

#**************************************
#### UPDATE: 13-07-2015 ####
#**************************************

#### Check changes in war data ####
a<-df[df$war_prio_on==1,]
table(a$country,a$year)

a<-df.New[df.New$war.onset==1,]
table(a$country,a$year)

#### Data changes: commodity price models ####

# Estimate model checking influence of data changes in 
# outcome and explanatory variables
outcome<-df.New[,c("year","ccode","war.onset","gdp.g","ind","oecd.exp")]
colnames(outcome)[2]<-"countryisocode"
colnames(outcome)[5]<-"ind3"
df2<-merge(df,outcome,all.x=TRUE)

#### New outcome variable ####
d<-df2[!is.na(df2$war.onset) & !is.na(df2$ind),]

# First stage estimation
n1<-lm(gdp_g~ind+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(n1,1,n1$model[,3])
summary(n1);mse(n1)

# Economic growth on war onset
n2<-lm(war.onset~gdp_g+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(n2,1,n2$model[,3])
summary(n2);mse(n2)

# Reduced form
n3<-lm(war.onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(n3,1,n3$model[,3])
summary(n2);mse(n2)

# Second stage
d$yhat<-predict(n1)
iv5<-lm(war.onset~yhat+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(iv5,1,iv5$model[,3])
mse(iv5)

#### New income data ####
d<-df2[!is.na(df2$war_prio_on) & !is.na(df2$ind) & !is.na(df2$gdp.g),]

# First stage estimation
i1<-lm(gdp.g~ind+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(i1,1,i1$model[,3])
summary(i1);mse(i1)

# Economic growth on war onset
i2<-lm(war_prio_on~gdp.g+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(i2,1,i2$model[,3])
summary(i2);mse(i2)

# Reduced form
i3<-lm(war_prio_on~ind+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(i3,1,i3$model[,3])
summary(i3);mse(i3)

# Second stage
d$yhat<-predict(i1)
iv6<-lm(war_prio_on~yhat+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(iv6,1,iv6$model[,3])
mse(iv6)

#### New index data ####
d<-df2[!is.na(df2$war_prio_on) & !is.na(df2$ind3),]

# First stage estimation
p1<-lm(gdp_g~ind3+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(p1,1,p1$model[,3])
summary(p1);mse(p1)

# Economic growth on war onset
p2<-lm(war_prio_on~gdp.g+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(p2,1,p2$model[,3])
summary(p2);mse(p2)

# Reduced form
p3<-lm(war_prio_on~ind3+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(p3,1,p3$model[,3])
summary(p3);mse(p3)

# Second stage
d$yhat<-predict(p1)
iv7<-lm(war_prio_on~yhat+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(iv7,1,iv7$model[,3])
mse(iv7)

#**************************************
##### Figure: coefficient plot #####
#**************************************

## Vectors with mu, sd, variable names, and y-axis settings
var.names<-c("Narrow replication \n 1981-2006",
             "ACD updated \n 1981-2006",
             "GDP growth updated \n 1981-2006",
             "Commodity prices updated \n 1981-2006",
             "All data updated \n 1981-2006",
             "Extending period \n 1981-2013",
             "Including civil conflict \n 1981-2013")
             
mu1<-c(-0.05914086,-0.019920948,-0.056119524 ,-0.038480482,-0.02042361,-0.01008197,0.0319266)
sd1<-c(0.028620376,0.015444291,0.030341211,0.025241068,0.01679852,0.00944932,0.0317275)

y.axis<-length(var.names):1 

## Plot data 
par(mgp=c(5,1,0),mar=c(5,15,5,1),family="serif")
plot(mu1,y.axis,type="n",axes=F,xlab="", ylab="",main="",
     xlim=c(min(mu1-qnorm(.975)*sd1),max((mu1+qnorm(.975)*sd1))),
     ylim=c(min(y.axis),max(y.axis)))

# Trimmings
rect(-.1109949,0,-.0072868,8,col="grey90",lwd=0)
abline(v=-.0591409,lwd=1,lty=2)
abline(v=0,lwd=1)

# Estimates and intervals
segments(mu1-qnorm(.975)*sd1,y.axis,mu1+qnorm(.975)*sd1,y.axis,
         lwd=5,col="grey50",lend=1)
segments(mu1-qnorm(.84)*sd1,y.axis,mu1+qnorm(.84)*sd1,y.axis,
         lwd=6,lend=1)
points(mu1,y.axis,type="p",col="black",pch=19,cex=2,
       xlim=c(min(mu1-qnorm(.975)*sd1),max((mu1+qnorm(.975)*sd1))),
       ylim=c(min(y.axis),max(y.axis)),main="")
    
# Axis
axis(2,at=y.axis,label=var.names,las=1,tick=F,mgp=c(2,.6,0),cex.axis=1.5,col="white") 
axis(1, tick=F,line=1,cex.axis=1.5)
