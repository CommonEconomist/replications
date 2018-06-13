#******************************************************************************
# R-script for replicating results from Iraq mortality study
# Hagopian et al. (2013)
# http://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001533
# Script is based on original python script
# This version:  16-09-2015
# First version: 29-11-2013
#******************************************************************************

#**************************************
#### LOAD ####
#**************************************

## Data
deaths<-read.csv("2013_Hagopian_et_al/hh_deaths.csv",header=TRUE,sep=",",row.names=NULL)
households<-read.csv("2013_Hagopian_et_al/hh_roster.csv",header=TRUE,sep=",",row.names=NULL)
population<-read.csv("2013_Hagopian_et_al/pop.csv",header=TRUE,sep=",",row.names=NULL)

## Bootstrap function
# Code taken from: http://biostatmatt.com/archives/2125
# NB - the code calls itself

# Bootstrap resample function
resample <- function(dat, cluster, replace) {
  
  # exit early for trivial data
  if(nrow(dat) == 1 || all(replace==FALSE))
    return(dat)
  
  # clustering factor
  cft <- dat[[cluster[1]]]
  
  # sample the clustering factor
  cls <- sample(unique(cft), replace=replace[1])
  
  # subset on the sampled clustering factors
  sub <- lapply(cls, function(b) subset(dat, cft==b))
  
  # sample lower levels of hierarchy (if any)
  if(length(cluster) > 1)
    sub <- lapply(sub, resample, cluster=cluster[-1], replace=replace[-1])
  
  # join and return samples
  do.call(rbind, sub)
}

#**************************************
#### CLEAN DATA ####
#**************************************

## Drop cluster 47 and 73 
deaths<-deaths[deaths$cluster!=47 & deaths$cluster!=73,] # -2 obs
households<-households[households$cluster!=47 & households$cluster!=73,] # - 203 obs.

## Recode missing observations on gov
households[households$cluster==34,]$gov<-13
households[households$cluster==87,]$gov<-6
households[households$cluster==90,]$gov<-2
deaths[deaths$cluster==87,]$gov<-6
deaths[deaths$cluster==90,]$gov<-2

## Recode incorrectly coded clusters
households[households$cluster==21 & households$hh==7,]$gov<-8
households[households$cluster==75 & households$hh==7,]$gov<-9
deaths[deaths$cluster==21 & deaths$hh==7,]$gov<-8
deaths[deaths$cluster==75 & deaths$hh==7,]$gov<-9

## Set NAs for mod to June
sum(is.na(deaths$mod)) # 26 obs.
deaths$mod[is.na(deaths$mod)]<-6

## Recode cardiovascular violent death
#deaths[deaths$cod=="cardiovascular" & deaths$war_death=="Y",]$war_death<-"N"

## Dummy variable for war & normal deaths
# NB: one obs. coded as D (row 100), unsure about meaning.
deaths$col<-as.numeric(deaths$war_death=="Y") # war deaths
deaths$norm<-as.numeric(deaths$war_death=="N") # normal deaths

## Create variable for date
deaths$day<-"01" 
deaths$date <- paste(deaths$yod, deaths$mod, deaths$day, sep="-")
deaths$date <- as.Date(deaths$date)
deaths$day<-NULL

# Pre-war deaths (all deaths before 03-2003)
deaths$norm_pre<-as.numeric(deaths$norm==1 & 
                              deaths$date<="2003-02-01") # pre-war

# During-war deaths (all deaths after 03-2003)
# NB: 1 obs. in August 2011
deaths$norm_dum<-as.numeric(deaths$norm==1 & 
                              deaths$date>"2003-02-01") # during war

d.pre<-sum(deaths$norm_pre) # Pre-war (44)
d.during<-sum(deaths$norm_dum) # Normal during war (262)
d.war<-sum(deaths$col) # Fatalities (76)

#### Calculate exposure: households ####

## Year of birth and year household formed
households$yob<-2011.5-(households$age+0.5)
households$hh_f<-households$year_hh_formed+0.5

## Pre-war period
households$t0<-2001
households$t1<-2003.167

# Account for household formation
households$t0<-apply(households[,c("hh_f","t0")],1,max)
households$t1<-apply(households[,c("hh_f","t1")],1,max)

## Exposure
households$exp0<-households$t1-households$yob
households[households$yob<=households$t0,]$exp0<-
  households[households$yob<=households$t0,]$t1-
  households[households$yob<=households$t0,]$t0
households[households$yob>households$t1,]$exp0<-0
sum(households$exp0) # 14561.54

## War-period
households$t0<-2003.167
households$t1<-2011.5

# Account for households formation
households$t0<-apply(households[,c("hh_f","t0")],1,max)
households$t1<-apply(households[,c("hh_f","t1")],1,max)

## Exposure
households$exp1<-households$t1-households$yob
households[households$yob<=households$t0,]$exp1<-
  households[households$yob<=households$t0,]$t1-
  households[households$yob<=households$t0,]$t0
sum(households$exp1) # 73282.96

#### Calculate exposure: deaths ####

## Year of birth and year household formed
deaths$yod<-deaths$yod+(deaths$mod-0.5)/12
deaths$hh_f<-deaths$year_hh_formed+.5

## Before war
deaths$t0<-2001
deaths$t1<-2003.167

## Account for household formation
deaths$t0<-apply(deaths[,c("hh_f","t0")],1,max)
deaths$t1<-apply(deaths[,c("hh_f","t1")],1,max)

## Exposure
deaths$exp0<-deaths$t1-deaths$yod
deaths[deaths$yod>deaths$t1,]$exp0<-deaths[deaths$yod>deaths$t1,]$t1-
  deaths[deaths$yod>deaths$t1,]$t0
deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$exp0<-
  deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$yod-
  deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$t0
sum(deaths$exp0) # 654.18

## During war
deaths$t0<-2003.167
deaths$t1<-2011.5

# Account for household formation
deaths$t0<-apply(deaths[,c("hh_f","t0")],1,max)
deaths$t1<-apply(deaths[,c("hh_f","t1")],1,max)

# Exposure
deaths$exp1<-deaths$t1-deaths$yod
deaths[deaths$yod<deaths$t0,]$exp1<-0
deaths[deaths$yod>deaths$t1,]$exp1<-deaths[deaths$yod>deaths$t1,]$t1-
  deaths[deaths$yod>deaths$t1,]$t0
deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$exp1<-
  deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$yod-
  deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$t0
sum(deaths$exp1) # 1227.82

#**************************************
#### ANALYSIS ####
#**************************************

#### Central estimates ####

## Person-years
py0<-sum(households$exp0)+sum(deaths$exp0) # Pre-war: 15215.72
py1<-sum(households$exp1)+sum(deaths$exp1) # During war: 74510.78 

## Death rates
cdr0<-1000*d.pre/py0 # Pre-war: 2.89  (Original 2.89)
cdr1<-1000*(d.during+d.war)/py1 # During war: 4.54 (Original 4.55)
excess=cdr1-cdr0 # 1.64

## Cumulative population
pop<-aggregate(population~year,population,sum)
py<-sum(pop[8:14,2])+9/12*pop[7,2]+.5*pop[15,2]

## Excess deaths: 402188 (Original: 405000)
excess*py/1000 

# Number of violent deaths: 249452
dr<-1000*d.war/py1
dr*py/1000 

#### BOOTSTRAP ESTIMATES ####

## Append all required data
d<-aggregate(cbind(col,norm_pre,norm_dum,exp0,exp1)~hh+cluster+gov,deaths,FUN=sum)
hh<-aggregate(cbind(exp0,exp1)~hh+cluster+gov,households,FUN=sum)
dat<-merge(hh,d,by=c("hh","cluster","gov"),all.x=TRUE)
dat[is.na(dat)]<-0

dat$exp0<-dat$exp0.x+dat$exp0.y
dat$exp1<-dat$exp1.x+dat$exp1.y
dat$exp0.x<-dat$exp0.y<-dat$exp1.x<-dat$exp1.y<-NULL

## Bootstrap hierarchy
cluster<-c("gov","cluster","hh")

## Bootstrap number of deaths

# violent deaths (15m7s)
set.seed(2014);system.time(v<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$col)))
# pre-war deaths (14m55s)
set.seed(2014);system.time(d0<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$norm_pre))) 
# during war deaths (15m)
set.seed(2014);system.time(d1<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$norm_dum)))

## Bootstrap person-years
# pre-war exposure (14m54s)
set.seed(2014);system.time(b.exp0<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$exp0))) 
# war exposure (15m5s)
set.seed(2014);system.time(b.exp1<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$exp1))) 

#### Uncertainty intervals ####

probs<-(1+c(-1,1)*0.95)/2 # 95% interval

## Excess deaths
a<-1000*(d1+v)/b.exp1-1000*d0/b.exp0
b<-a*py/1000
excess.deaths<-b
quantile(a,probs=probs) # 0.20; 3.09
quantile(b,probs=probs) # 50124; 756399

## Excluding violent deaths
a<-1000*d1/b.exp1-1000*d0/b.exp0
b<-a*py/1000
excess.normal<-b
quantile(a,probs=probs) # -0.80; 1.95
quantile(b,probs=probs) # -196019; 478079
sum(a>0)/length(a)*100 # 80.3%

## Violent deaths
a<-1000*v/b.exp1
b<-a*py/1000
violent.deaths<-b
quantile(a,probs=probs) # 0.72; 1.37
quantile(b,probs=probs) # 175266; 334831

#### Figure: Bootstrap estimates ####
library(PerformanceAnalytics)
library(psych)
options(scipen=4)     

d<-data.frame(exc=excess.deaths,norm=excess.normal,v=violent.deaths)
colnames(d)<-c("Excess deaths","Non-violent excess deaths","Violent deaths")
chart.Boxplot(d, main = "Bootstrap estimates \n (1000 replicates of clusters and households)",
xlab="", ylab="",element.color = "transparent", as.Tufte=TRUE,ylim=c(-500000,1100000))
