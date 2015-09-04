#******************************************************************************
# Replication "Warming increases the risk of civil war in Africa
# This version:  04-09-2015
#******************************************************************************

rm(list=ls(all=TRUE)) # Clear workspace
options(scipen=4)     

## Libraries
library(foreign)
library(plyr)

## Load data
cc<-read.dta("2009_Burke_et_al/climate_conflict.dta")  # Original data

## Functions
source("2009_Burke_et_al/clse.R")                      # Robust standard errors

#**************************************
##### REPLICATION ####
#**************************************

cc<-cc[order(cc$ccode,cc$year),]
cc<-cc[cc$year_actual<=2002,] # Subset to included data only (N=889)

## Calculate mean temperature per country (for figure)
mean<-ddply(cc, .(ccode), summarize,
         mean=mean(temp_all))
cc<-merge(cc,mean)

## Variables
incidence<-cc$war_prio_new    
temp<-cc$temp_all
temp.l<-cc$temp_all_lag
pre<-cc$prec_all
pre.l<-cc$prec_all_lag
ccode<-cc$ccode
year<-cc$year_actual
year2<-cc$year
gdp<-cc$gdp_l
polity2<-cc$polity2_lag
m<-cc$mean

# Vector for number of observations model 3
v3<-na.omit(cc[,c("ccode","war_prio_new","temp_all",
          "temp_all_lag","prec_all","prec_all_lag",
          "gdp_l","polity2_lag")])


#### Model 1 (preferred model) ####
m1<-lm(incidence~temp+temp.l
       +factor(ccode)+factor(ccode)*year)
summary(m1)          
clse(m1,1,ccode)     # Robust standard errors     

#### Model 2 ####
m2<-lm(incidence~temp+temp.l+pre+pre.l
       +factor(ccode)+factor(ccode)*year)
summary(m2)     
clse(m2,1,ccode)     # Robust standard errors

#### Model 3 ####
m3<-lm(incidence~temp+temp.l+pre+pre.l+gdp+polity2
       +factor(ccode)+year2)
summary(m3) 
clse(m3,1,v3$ccode) # Robust standard errors


#**************************************
##### FIGURE: Predictions ####
#**************************************

## Data
d<-data.frame(incidence=incidence,temp=round(temp-m,2),pr=fitted(m1),res=m1$residuals)

## Plot
par(mar=c(4,4,2,2),pty="s",las=0,family="serif")
plot(d$temp,d$pr,axes=FALSE,ylab="",xlab="",pch=1,xlim=c(-2,2),cex=.8,col="gray20")
points(d[d$incidence==1,]$temp,d[d$incidence==1,]$pr,col="black",pch=19,cex=1)

# Axis
axis(1,at=seq(-2,2,.5),tck=0.02, cex.axis=1.2,col="white")
axis(2,at=seq(-1,1.5,.25),tck=0.02, cex.axis=1.2,las=1,line=1,col="white")
mtext("Deviation from mean temperature",side=1,line=2.5,cex=1.2)
mtext("Predicted probability",side=2,line=5,cex=1.2)

# Lines
abline(h=0,lty=2,lwd=2)
abline(h=1,lty=2,lwd=2)

# Labels
text(1.25,0.85,"Observed war")
segments(1.22,0.82,.8,0.74)

