## Clean data
# Use data from a number of different sources
# UCDP GED: http://ucdp.uu.se/downloads/
# Original Wikileaks data: https://archive.org/download/wikileaks-archive

#------------------------------------------------------------------------------
#### 1) Prepare UCDP-GED data ####
load("data_raw/ged171.Rdata")

## Subset the data to include individual events in Iraq since the start 
# of the war. 
# Use two periods: Wikileaks 2004-2009, Hagopian 2003-2011

# Period from Hagopian et al. (2013) survey
ged<-data.frame(ged171[ged171$country=="Iraq" 
                         & ged171$date_start>="2003-03-20" 
                         & ged171$date_start<="2011-06-30"
                         & ged171$event_clar==1,])

ged1<-ged$best
ged1<-ged1[ged1>0] # 2913 events, 20343 fatalities, median=2  

# Period from Wikileaks
ged<-data.frame(ged171[ged171$country=="Iraq" 
                         & ged171$date_start>="2004-01-01" 
                         & ged171$date_start<="2009-12-31"
                         & ged171$event_clar==1,])

ged2<-ged$best
ged2<-ged2[ged2>0] 

#### 2) Original Wikileaks data ####
# NB - This is a large file, so can take a while to load
irq<-read.csv("data/iraq_cleaned.csv",stringsAsFactors=FALSE,sep=",",
                header=TRUE)
kia<-as.vector(irq$friendlykia+irq$hostnationkia+irq$civiliankia+irq$enemykia)
wikileaks<-kia[kia>0] # Remove observations without fatalities

#------------------------------------------------------------------------------
rm(list=setdiff(ls(),c("ged1","ged2","wikileaks")))
