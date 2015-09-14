#******************************************************************************
# Replication of Besley and Reynal-Querrol (2014)
# "The Legacy of Historical Conflict: Evidence from Africa"
# Between-country estimation
# This version:  09-06-2015
# First version: 07-08-2014
#******************************************************************************

setwd("[SPECIFY DIR]")
rm(list=ls(all=TRUE)) # Clear workspace
options(scipen=4)     # # Preferences

## Libraries
library(car)
library(countrycode)
library(lmtest)
library(foreign)
library(sandwich)

## Load data
d<-read.dta("raw_data/table2.dta")
load("tidy_data/conflict_data.Rdata")

#### Figure: War prevalance ####
par(mar=c(5,6,2,2))

## Plot
plot(d$WarPrevalence14001700,d$CivilWarIncidence,pch=19, col="black",
     axes=FALSE,xlab="",ylab="")

# Axis
axis(1,las=1,at=seq(0,100,10),tck=0.02,cex.axis=1.2)
axis(2,las=1,at=seq(0,50,10),tck=0.02,cex.axis=1.2)
axis(1,at=seq(0,100,5),tck=0.01,labels=FALSE)
axis(2,at=seq(0,50,5),tck=0.01,labels=FALSE)

mtext("Civil war incidence",side=2,cex=1.2,line=3)
mtext("War prevalence 1400-1700 CE",side=1,cex=1.2,line=3)

# Label points
country<-c("Algeria","Angola",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Ethiopia",
           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Mali",NA,NA,"Morocco","Mozambique",
           NA,NA,"Nigeria",NA,NA,NA,"Somalia",NA,"Sudan",NA,NA,NA,NA,"Uganda",
           NA,"Zimbabwe")
h.adj<-c(2.9,2.9,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,-3.1,
         NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,NA,NA,3.5,1,NA,NA,2.9,NA,NA
         ,NA,2.5,NA,2.7,NA,NA,NA,NA,3,NA,3.9)
v.adj<-c(0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,NA,
         NA,NA,NA,NA,NA,NA,0,NA,NA,0,1,NA,NA,0,NA,NA,NA,1,NA,0,NA,NA,NA,
         NA,0,NA,0)
text(d$WarPrevalence14001700+h.adj,d$CivilWarIncidence+v.adj,labels=d$country,cex=1.0)

# Regression line
fit<-lm(d$CivilWarIncidence~d$WarPrevalence14001700-1)
abline(fit,lty=4)

## There are some countries with potential high leverage or outliers:
# Sudan, Angola, Ethiopia, Nigeria, Morocco, Mali

#### Replication ####

#### Table 2 ####

## Model 1
m1<-lm(CivilWarIncidence~WarPrevalence14001700+f_french+f_spain+f_pothco+
          f_dutch+f_belg+f_italy+f_germ+region_nNUNN+region_sNUNN+region_wNUNN+
          region_eNUNN+region_cNUNN, data=d);summary(m1)
coeftest(m1,vcov.=vcovHC(m1,type="HC1"))

## Model 2
m2<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+region_nNUNN+
         region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+f_french+f_spain+
         f_pothco+f_dutch+f_belg+f_italy+f_germ+abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+ln_coastline_areaNUNN+
         island_dumNUNN+islam+legor_frNUNN+ln_avg_gold_pop+ln_avg_oil_pop+
         ln_avg_all_diamonds_pop+ETHPOL+yellow+rugged, data=d);summary(m2)
coeftest(m2,vcov.=vcovHC(m2,type="HC1"))

## Interpretation of results
standardize(m1)
standardize(m2)

#### Regression diagnostics ####

## Outliers
outlierTest(m1)
outlierTest(m2)

## Influential observations
row.names(d)<-d$country

# Model 1
cutoff <- 4/((nrow(d)-length(m1$coefficients)-2))
plot(m1, which=4, cook.levels=cutoff)

# Model 2
cutoff <- 4/((nrow(d)-length(m2$coefficients)-2))
plot(m2, which=4, cook.levels=cutoff)

## Re-estimate model removing Sudan
d0<-d[d$country!="Sudan",]
m1a<-lm(CivilWarIncidence~WarPrevalence14001700+f_french+f_spain+f_pothco+
          f_dutch+f_belg+f_italy+f_germ+region_nNUNN+region_sNUNN+region_wNUNN+
          region_eNUNN+region_cNUNN, data=d0);summary(m1a)
coeftest(m1a,vcov.=vcovHC(m1a,type="HC1")) # Larger effect

#### Simpler model ####
# 29 variables seems a bit much for 48 observations

a1<-lm(CivilWarIncidence~WarPrevalence14001700+
         ln_coastline_areaNUNN+ln_avg_oil_pop,data=d)
coeftest(a1,vcov.=vcovHC(a1,type="HC1"))


#### Figure: Fitted versus outcomes ####
par(mar=c(3,4,1,1),
    pty="s",
    mfrow=c(1,3))
plot(fitted(m1),m1$model[,1])
abline(a=0,b=1)
plot(fitted(m2),m2$model[,1])
abline(a=0,b=1)
plot(fitted(a1),a1$model[,1])
abline(a=0,b=1)

## Seems like model 2 is an overfit

#### Extension ####

## Re-estimate model 2 using latest data

d$ccode<-countrycode(d$country,"country.name","iso3c",warn=TRUE)
d<-merge(d,df[,-5],all.x=TRUE)

## Most recent data
a1<-lm(war.paper~WarPrevalence14001700+lrgdpl2631970+region_nNUNN+
         region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+f_french+f_spain+
         f_pothco+f_dutch+f_belg+f_italy+f_germ+abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+ln_coastline_areaNUNN+
         island_dumNUNN+islam+legor_frNUNN+ln_avg_gold_pop+ln_avg_oil_pop+
         ln_avg_all_diamonds_pop+ETHPOL+yellow+rugged, data=d)
summary(a1)
coeftest(a1,vcov.=vcovHC(a1,type="HC1"))
standardize(a1) # Much stronger effect

# Control for colonial wars
a2<-lm(war.paper~WarPrevalence14001700+col.prevalence+lrgdpl2631970+region_nNUNN+
         region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+f_french+f_spain+
         f_pothco+f_dutch+f_belg+f_italy+f_germ+abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+ln_coastline_areaNUNN+
         island_dumNUNN+islam+legor_frNUNN+ln_avg_gold_pop+ln_avg_oil_pop+
         ln_avg_all_diamonds_pop+ETHPOL+yellow+rugged, data=d)
coeftest(a2,vcov.=vcovHC(a2,type="HC1"))
standardize(a2) # Much stronger effect

## Include lower intensity type of conflicts
a3<-lm(conflict.paper~WarPrevalence14001700+lrgdpl2631970+region_nNUNN+
         region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+f_french+f_spain+
         f_pothco+f_dutch+f_belg+f_italy+f_germ+abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+ln_coastline_areaNUNN+
         island_dumNUNN+islam+legor_frNUNN+ln_avg_gold_pop+ln_avg_oil_pop+
         ln_avg_all_diamonds_pop+ETHPOL+yellow+rugged, data=d)
coeftest(a3,vcov.=vcovHC(a3,type="HC1"))
standardize(a3) # Much stronger effect

## Extend the period
a4<-lm(conflict.update~WarPrevalence14001700+lrgdpl2631970+region_nNUNN+
         region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+f_french+f_spain+
         f_pothco+f_dutch+f_belg+f_italy+f_germ+abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+ln_coastline_areaNUNN+
         island_dumNUNN+islam+legor_frNUNN+ln_avg_gold_pop+ln_avg_oil_pop+
         ln_avg_all_diamonds_pop+ETHPOL+yellow+rugged, data=d)
coeftest(a4,vcov.=vcovHC(a4,type="HC1"))
standardize(a4) # Strongest effect so far




