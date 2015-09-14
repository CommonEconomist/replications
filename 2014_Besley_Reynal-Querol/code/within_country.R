#******************************************************************************
# Replication of Besley and Reynal-Querrol (2014)
# "The Legacy of Historical Conflict: Evidence from Africa"
# Within-country estimation
# This version:  09-06-2015
# First version: 15-01-2014
#******************************************************************************

## Libraries
library(foreign)
library(countrycode)

## Load data
d<-read.dta("raw_data/table5.dta")
ged<-read.csv("raw_data/ucdp-ged15.csv",header=TRUE,sep=",",row.names=NULL)

#### Prepare UCDP GED data ####

## Subset on precision
ged<-ged[ged$where_prec<=2,]  # N=16426
ged<-na.omit(ged)             # Remove obs. with missing data (16424)

# Round coordinates to place in grid-cell
obs<-ged[,c(19,26,27,38)]
obs$lat<-floor(obs$lat)
obs$lon<-floor(obs$lon)

## Aggregate fatality data for each grid cell
dat<-aggregate(.~lat+lon+country,obs,FUN=sum)

#### Prepare data ####

## Add country code
dat$ccode<-countrycode(dat$country,"country.name","iso3c",warn=TRUE)
d$ccode<-countrycode(d$country,"country.name","iso3c",warn=TRUE)

colnames(dat)<-c("glat","glong","country","battle_deaths","ccode")
dat<-dat[,-3]

## Merge data
df<-merge(d,dat,all.x=TRUE)
df[is.na(df$battle_deaths),]$battle_deaths<-0
df$conflict<-as.numeric(df$battle_deaths>0)

#### Model estimation ####

m1<-lm(ConflictGrid~HistoricalConflictGrid+lpopdens90+factor(country),df)
m2<-lm(conflict~HistoricalConflictGrid+lpopdens90+factor(country),df)

## Placebo test
df$placebo<-rbinom(3546,1,51/3546)
m1<-lm(ConflictGrid~placebo+lpopdens90+factor(country),df);summary(m1)

