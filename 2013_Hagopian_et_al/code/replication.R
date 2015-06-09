#******************************************************************************
# This version:  21-11-2014
# First version: 29-11-2013
# R-script for replicating results from Iraq mortality study
# Script is based on original python script
#******************************************************************************

setwd("[SPECIFY DIR]")

## Load data
deaths<-read.csv("raw_data/hh_deaths.csv",header=TRUE,sep=",",row.names=NULL)
households<-read.csv("raw_data/hh_roster.csv",header=TRUE,sep=",",row.names=NULL)
population<-read.csv("raw_data/pop.csv",header=TRUE,sep=",",row.names=NULL)

#**************************************
#### Data preparation ####
#**************************************

## Drop cluster 47 and 73 
deaths<-deaths[deaths$cluster!=47 & deaths$cluster!=73,] # -2 obs
households<-households[households$cluster!=47 & households$cluster!=73,] # - 203 obs.

## Drop obs. where yod is less than year hh formed
deaths$drop<-as.numeric(deaths$yod<deaths$year_hh_formed)
table(deaths$drop) # 0 obs.
deaths$drop<-NULL

## Recode missing observations on gov
households[households$cluster==34,]$gov<-13
households[households$cluster==87,]$gov<-6
households[households$cluster==90,]$gov<-2
deaths[deaths$cluster==87,]$gov<-6
deaths[deaths$cluster==90,]$gov<-2

## Set NAs for mod to June
sum(is.na(deaths$mod)) # 26
deaths$mod[is.na(deaths$mod)]<-6

## Tally deaths per period
deaths$col<-as.numeric(deaths$war_death=="Y") # war deaths
deaths$norm<-as.numeric(deaths$war_death=="N") # normal deaths

## Create variable for date
deaths$day<-"01" 
deaths$date <- paste(deaths$yod, deaths$mod, deaths$day, sep="-")
deaths$date <- as.Date(deaths$date)
deaths$day<-NULL

## Pre-war deaths (all deaths before 03-2003)
deaths$norm_pre<-as.numeric(deaths$norm==1 & 
                              deaths$date<="2003-02-01") # pre-war

## During-war deaths (all deaths after 03-2003)
# NB: 1 obs. in August 2011
deaths$norm_dum<-as.numeric(deaths$norm==1 & 
                              deaths$date>"2003-02-01") # during war

d.pre<-sum(deaths$norm_pre) # Pre-war (44)
d.during<-sum(deaths$norm_dum) # Normal during war (262)
d.war<-sum(deaths$col) # Casualties (76)

## Calculate person-years:

# Tally person years observed for a row of hh roster for a given time period
households$yob<-2011.5-(households$age+0.5)
households$hh_f<-households$year_hh_formed+0.5

# Pre-war period
households$t0<-2001
households$t1<-2003.167

households$t0<-apply(households[,c(12,13)],1,max)
households$t1<-apply(households[,c(12,14)],1,max)

households$exp0<-households$t1-households$yob
households[households$yob<=households$t0,]$exp0<-
  households[households$yob<=households$t0,]$t1-
  households[households$yob<=households$t0,]$t0
households[households$yob>households$t1,]$exp0<-0

## This results in 14561.54 pre-war person-years for people still alive

# War-period
households$t0<-2003.167
households$t1<-2011.5

households$t0<-apply(households[,c(12,13)],1,max)
households$t1<-apply(households[,c(12,14)],1,max)

households$exp1<-households$t1-households$yob
households[households$yob<=households$t0,]$exp1<-
  households[households$yob<=households$t0,]$t1-
  households[households$yob<=households$t0,]$t0

## This results in 73282.96 person-years for people alive during the war

## Person-years for individuals who have died

# Before war
deaths$t0<-2001
deaths$t1<-2003.167
deaths$yod<-deaths$yod+(deaths$mod-0.5)/12 # isnan lines from script skipped (see line 22)

deaths$exp0<-deaths$t1-deaths$yod
deaths[deaths$yod>deaths$t1,]$exp0<-deaths[deaths$yod>deaths$t1,]$t1-
  deaths[deaths$yod>deaths$t1,]$t0
deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$exp0<-
  deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$yod-
  deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$t0

## This results in 773.7797 person-years before the war

# During war
deaths$t0<-2003.167
deaths$t1<-2011.5
deaths$exp1<-deaths$t1-deaths$yod
deaths[deaths$yod<deaths$t0,]$exp1<-0
deaths[deaths$yod>deaths$t1,]$exp1<-deaths[deaths$yod>deaths$t1,]$t1-
  deaths[deaths$yod>deaths$t1,]$t0
deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$exp1<-
  deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$yod-
  deaths[deaths$yod>=deaths$t0 & deaths$yod<=deaths$t1,]$t0

## This results in 1353.554 person-years during the war

#**************************************
##### Excess deaths ####
#**************************************

# Tally person-years
py0<-sum(households$exp0)+sum(deaths$exp0) # Pre-war (15335.32)
py1<-sum(households$exp1)+sum(deaths$exp1) # During war (74636.51)

# Calculate death rates
cdr0<-1000*d.pre/py0 # Pre-war (2.87 vs. original 2.891857)
cdr1<-1000*(d.during+d.war)/py1 # During war (4.53 vs. original 4.54971)
excess=cdr1-cdr0 # (1.66)

# Calculate excess deaths
pop<-aggregate(population~year,population,sum)
py<-sum(pop[8:14,2])+9/12*pop[7,2]+.5*pop[15,2]

excess*py/1000 # 405834.2 

# Number of violent deaths:
dr<-1000*d.war/py1
dr*py/1000 # 249031.5

#**************************************
#### Bootstrap resampling ####
# Code taken from: 
# http://biostatmatt.com/archives/2125
# NB - the code calls itself
#**************************************

## Bootstrap resample function
resample <- function(dat, cluster, replace) {
  
  # exit early for trivial data
  if(nrow(dat) == 1 || all(replace==FALSE))
    return(dat)
  
  # sample the clustering factor
  cls <- sample(unique(dat[[cluster[1]]]), replace=replace[1])
  
  # subset on the sampled clustering factors
  sub <- lapply(cls, function(b) subset(dat, dat[[cluster[1]]]==b))
  
  # sample lower levels of hierarchy (if any)
  if(length(cluster) > 1)
    sub <- lapply(sub, resample, cluster=cluster[-1], replace=replace[-1])
  
  # join and return samples
  do.call(rbind, sub)
}

# Append all required data
d<-aggregate(cbind(col,norm_pre,norm_dum,exp0,exp1)~hh+cluster+gov,deaths,FUN=sum)
hh<-aggregate(cbind(exp0,exp1)~hh+cluster+gov,households,FUN=sum)
dat<-merge(hh,d,by=c("hh","cluster","gov"),all.x=TRUE)
dat[is.na(dat)]<-0

dat$exp0<-dat$exp0.x+dat$exp0.y
dat$exp1<-dat$exp1.x+dat$exp1.y
dat$exp0.x<-dat$exp0.y<-dat$exp1.x<-dat$exp1.y<-NULL

cluster<-c("gov","cluster","hh")

## Bootstrap number of deaths

# violent deaths (15m36s)
set.seed(2014);system.time(v<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$col)))
# pre-war deaths (15m30s)
set.seed(2014);system.time(d0<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$norm_pre))) 
# during war deaths (15m38s)
set.seed(2014);system.time(d1<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$norm_dum)))

## Bootstrap person-years
# pre-war exposure (15m25s)
set.seed(2014);system.time(b.exp0<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$exp0))) 
# war exposure (15m36s)
set.seed(2014);system.time(b.exp1<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$exp1))) 

## Calculate death rates & excess deaths

probs<-(1+c(-1,1)*0.95)/2 # 95% interval

a<-1000*(d1+v)/b.exp1-1000*d0/b.exp0
b<-a*py/1000
quantile(a,probs=probs) # 0.23; 2.81
quantile(b,probs=probs) # 55000; 690000

# Excluding violent deaths
a<-1000*d1/b.exp1-1000*d0/b.exp0
b<-a*py/1000
quantile(a,probs=probs) # -0.87; 1.67
quantile(b,probs=probs) # -210000; 410000
sum(a>0)/length(a)*100 # 83.4%

#### Save output ####
save(list=c("py0","py1","cdr0","cdr1","excess","py","v","d0",
            "d1","b.exp0","b.exp1"),file="output/ReplicationOutput.Rdata")
