#******************************************************************************
# Create outcome variables country cross-section
# This version:  23-04-2015
# First version: 15-01-2014
#******************************************************************************

setwd("[SPECIFY DIR]")

## Libraries
library(arm)
library(countrycode)

## Load data
polity<-read.csv("raw_data/p4v2012.csv",sep=",",header=TRUE,row.names=NULL)
acd<-read.csv("raw_data/acd2013.csv",header=TRUE,sep=",",row.names=NULL)

#### Date of independence ####

## Correct country names
polity$country<-as.character(polity$country)
polity[polity$scode=="CON",]$country<-"Congo" # Republic of the Congo
polity[polity$scode=="ZAI",]$country<-"Democratic Republic of the Congo" 

## Subset to African countries
polity$continent<-countrycode(polity$country,"country.name","continent",warn=TRUE)
pol<-polity[polity$continent=="Africa",]

## Year of independence
pol$independence<-as.numeric(pol$change==99)
pol[is.na(pol$independence),]$independence<-0
pol<-pol[pol$independence==1,]

## Minor adjustments
pol$ccode<-countrycode(pol$country,"country.name","iso3c",warn=TRUE)
pol[pol$country=="South Sudan",]$ccode<-"SSD"
independence<-pol[,c(2,5)]
colnames(independence)<-c("ccode","independence.year")

## Years since independence
independence$p1<-2007-independence$independence.year # Period covered in paper
independence$p2<-2012-independence$independence.year # Period including most recent data

#### War spells ####

## Subset to African countries
acd<-acd[acd$region==4 | acd$country=="Egypt",] 
acd<-acd[,c(1,2,4,5:7)]
acd$ccode<-countrycode(acd$country,"country.name","iso3c",warn=TRUE) 
acd[acd$country=="South Sudan",]$ccode<-"SSD"

## Intensity type
acd$minor<-as.numeric(acd$int==1) # Conflict: between 25-999 battle-related deaths
acd$war<-as.numeric(acd$int==2)   # War: >=1000 battle-related deaths

## Conflict type
acd$colonial<-as.numeric(acd$type==1)            # Colonial war
acd$inter<-as.numeric(acd$type==2)               # Interstate war
acd$intra<-as.numeric(acd$type==3 | acd$type==4) # Intrastate conflict

## Calculate war prevalence
d<-merge(acd,independence) # France dropped

d$pre.independence<-as.numeric(d$year<d$independence.year) # Pre-independence conflict
d$p1.conflict<-as.numeric(d$year>=d$independence.year & d$year<=2007) # Till 2007
d$p2.conflict<-as.numeric(d$year>=d$independence.year & d$year<=2012) # Till 2012

# Number of wars during colonial period
d<-within(d,{col.prevalence=ave(pre.independence,ccode,FUN=sum)})

# Civil war incidence
d$p1.civilwar<-d$p1.conflict*d$war 
d$p2.civilwar<-d$p2.conflict*d$war 

# Civil conflict incidence
d$p1.civilconflict<-d$p1.conflict*d$minor 
d$p2.civilconflict<-d$p2.conflict*d$minor

# Prevalence of civil war/conflict
d<-within(d,{war.paper=ave(p1.civilwar,ccode,FUN=sum)}) # Independence - 2007
d<-within(d,{war.update=ave(p2.civilwar,ccode,FUN=sum)}) # Independence - 2012

d<-within(d,{conflict.paper=ave(p1.civilconflict,ccode,FUN=sum)}) # Independence - 2007
d<-within(d,{conflict.update=ave(p2.civilconflict,ccode,FUN=sum)}) # Independence - 2012

## Aggregate the data
df<-aggregate(. ~ccode+country+independence.year+p1+p2,d,FUN=max) 
df<-df[,c(1:5,19,24:27)]

# Add data
df<-merge(df,independence,all.y=TRUE)
df$country<-countrycode(df$ccode,"iso3c","country.name",warn=TRUE)
df[is.na(df)]<-0

## Save data
save(list="df",file="tidy_data/conflict_data.Rdata")
