## Clean data oil price shocks
# Data sources:
# http://www-personal.umich.edu/~lkilian/reaupdate.txt
# https://data.bls.gov/cgi-bin/surveymost?cu
# https://www.eia.gov/totalenergy/data/browser/?tbl=T11.01B#/?f=M
# https://www.eia.gov/totalenergy/data/browser/?tbl=T09.01#/?f=M
setwd("~/Downloads/killian")
par(mar=c(5,5,2,0),mfrow=c(3,1),las=1,bty="n",cex.lab=2,cex.axis=2,cex.main=2)

#### Oil prices ####
p<-read.csv("MER_T09_01.csv",stringsAsFactor=FALSE)

# Subset data to acquisition cost
p<-p[p$Description=="Refiner Acquisition Cost of Crude Oil, Composite",]

# Drop annual averages
p$m<-substring(p$YYYYMM,5,6)
p<-p[p$m!=13,]

# Drop NAs
p<-p[p$Value!="Not Available",]
p$Value<-as.numeric(p$Value)

# Create time-series object
p<-p[order(p$YYYYMM),]
npo<-ts(p$Value,start=c(1974,1),frequency=12)

# Load CPI
cpi<-read.csv("cpi.csv",stringsAsFactor=FALSE)
cpi<-cpi[cpi$Period!="S01" & cpi$Period!="S02",]

cpi.ts<-ts(cpi$Value,start=c(1913,1),frequency=12)
cpi.ts<-window(cpi.ts,start=c(1974,1),end=c(2017,5))

## Calculate real oil price (log)
# https://www.dallasfed.org/research/basics/nominal.cfm
rpo<-log(npo/cpi.ts)
rpo<-window(rpo,start=c(1974,1),end=c(2016,12))

rm(list=setdiff(ls(), "rpo")) # Housekeeping
plot(rpo,lwd=2)

#### Oil production ####
p<-read.csv("MER_T11_01B.csv",stringsAsFactors=FALSE)

# Subset data to global production
p<-p[p$Description=="Crude Oil Production, World",]

# Drop month 13
p$m<-substring(p$YYYYMM,5,6)
p<-p[p$m!=13,]

# Create time-series object
p$Value<-as.numeric(p$Value)
p<-p[order(p$YYYYMM),]
p.ts<-ts(p$Value,start=c(1973,1),frequency=12)

# Caclulate change
d.prod<-(p.ts-lag(p.ts,-1))/lag(p.ts,-1)*100
d.prod=window(d.prod,start=c(1974,1),end=c(2016,12))

rm(list=setdiff(ls(), c("rpo","d.prod"))) # Housekeeping
plot(d.prod)

#### Index of real economic activity ####
act<-read.csv("reaupdate.txt",header=FALSE)

rea.ts<-ts(act[,1],start=c(1968,1),frequency=12)
rea<-window(rea.ts,start=c(1974,1),end=c(2016,12))


rm(list=setdiff(ls(), c("rpo","d.prod","rea"))) # Housekeeping
plot(rea)

##
oil<-ts.union(d.prod,rea,rpo)
