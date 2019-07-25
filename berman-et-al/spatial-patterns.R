#******************************************************************************
# This version:  21-07-2015
# First version: 04-11-2014
# Spatial analysis of conflict in Berman et al. data
#******************************************************************************

#### LOAD LIBRARIES AND DATA ####

setwd("[SPECIFY DIR]/Replications/2013_Berman_et_al")

## Load libraries
library(maptools)
library(foreign)
library(spdep)
library(plyr)
library(RColorBrewer)  
library(DataCombine)

## Load data
districts<-readShapeSpatial("iraq_districts.shp")  # District borders
berman<-read.dta("BermanetalAER2013replication.dta")

#### SPATIAL ANALYSIS ####

## Create contiguity matrix
districts<-districts[order(districts$ADM3NAME),]
k<-poly2nb(districts,queen=TRUE) # Neighbour list, queen contiguity
W<-nb2listw(k,style="B",zero.policy=TRUE) # Spatial weights (binary)
adj<-as.matrix(listw2mat(W)) # Adjacency matrix
row.names(adj)<-districts$ADM3NAME

## Calculate spatial lags using Kronecker product
T<-length(as.vector(unique(berman$halfyr)))
ident<-diag(1,nrow=T)

berman<-berman[order(berman$halfyr,berman$district),]
adj<-adj[order(row.names(adj)),]

## Check if they are indeed the same names
length(intersect(berman$district,row.names(adj)))

## Calculate spatial lag 
berman$w.p_S1<-as.numeric((ident%x%adj)%*%berman$p_S1) 
berman[is.na(berman$a_of_batt),]$a_of_batt<-0
berman$w.a_of_batt<-as.numeric((ident%x%adj)%*%berman$a_of_batt) 
berman$w.p_ms_cerp_small<-as.numeric((ident%x%adj)%*%berman$p_ms_cerp_small) 
berman$w.dis_usprt<-as.numeric((ident%x%adj)%*%berman$dis_usprt) 

## Calculate descriptive statistics
ds<-ddply(berman,.(district),summarize,
            mean.p_S1=mean(p_S1),
            mean.p_ms_cerp=mean(p_ms_cerp),
            a_of_batt=max(a_of_batt),
            sum.prt=sum(dis_usprt))
            
districts@data<-data.frame(districts@data,
                           p_attacks=ds$mean.p_S1,
                           p_aid=ds$mean.p_ms_cerp,
                           batt_dis=ds$a_of_batt,
                           prt_dist=ds$sum.prt)


##### Figure: attacks and aid per district ####

## Settings
par(mfrow = c(1, 2),
    oma = c(4, 0, 0, 0), 
    mar = c(0, 0, 2.5, 2), 
    mgp = c(0, 0, 0),    
    xpd = NA) 

## Average attacks per capita
brks<-c(0.1,0.25,.5,1,1.5,2,3,5,9)
col.regions=brewer.pal(9,"Greys")
plot(districts,
     col=col.regions[findInterval(districts$p_attacks,brks,all.inside=TRUE)],
     axes=F,cex.main=2,lwd=.5,
     main="Number of attacks per capita")
legend(39,31, legend=leglabs(brks), fill=col.regions, bty="n",cex=1.2,
       y.intersp=.5)

## Average aid per capita
brks<-c(0,2.5,5,10,15,30,40,50,200)
col.regions=brewer.pal(9,"Greys")
plot(districts,
     col=col.regions[findInterval(districts$p_aid,brks,all.inside=TRUE)],
     axes=F,cex.main=2,lwd=.5,
     main="Aid dollars per capita")
legend(39,31, legend=leglabs(brks), fill=col.regions, bty="n",cex=1.2,
       y.intersp=.5)


#### Spatial spillover test: Moran's statistic ####

moran.mc(ds$mean.p_S1,listw=W,nsim=9999)   
moran.mc(ds$mean.p_ms_cerp,listw=W,nsim=9999)   

##### Figure; Moran measures ####

## Moran plot
dev.off()
par(mar=c(5,4,4,4))
moran.plot(ds$mean.p_S1,listw=W,
           pch=16, col="black",cex=1.5,
           quiet=F,labels=as.character(ds$district),
           xlab="",ylab="",bty="n",axes=FALSE)
axis(1,at=seq(0,10,1),tck=0.02,cex.axis=1.2)
axis(1,at=seq(0,10,0.5),labels=F,tck=0.01)
axis(2,at=seq(0,55,5),las=1,tck=0.02,cex.axis=1.2)
axis(2,at=seq(0,55,1),labels=F,tck=0.01)
mtext("Conflict intensity (log)",side=1,cex=1.2,line=3)
mtext("Spatial lag conflict intensity",side=2,cex=1.2,line=2.5)


## Local moran
lm<-localmoran(ds$mean.p_S1,listw=W)
districts$lm<-abs(lm[,4]) ## Extract z-scores
brks<-c(0,.5,1,1.5,2,3,4,9)
col.regions=brewer.pal(8,"Greys")
par(mar = c(2, 0, 2.5, 2))
plot(districts,
     col=col.regions[findInterval(districts$lm,brks,all.inside=TRUE)],
     axes=F,cex.main=2,lwd=.5,
     main="Local Moran Z-score")
legend(38,32, legend=leglabs(brks), fill=col.regions, bty="n",cex=1.2,
       y.intersp=.5)


## Moran plot
par(mar=c(5,4,4,4))
moran.plot(ds$mean.p_ms_cerp,listw=W,
           pch=16, col="black",cex=1.5,
           quiet=F,labels=as.character(ds$district),
           xlab="",ylab="",bty="n",,axes=FALSE)
axis(1,at=seq(0,300,50),tck=0.02,cex.axis=1.2)
axis(1,at=seq(0,300,10),labels=F,tck=0.01)
axis(2,at=seq(0,400,50),las=1,tck=0.02,cex.axis=1.2)
axis(2,at=seq(0,400,10),labels=F,tck=0.01)
mtext("Aid",side=1,cex=1.2,line=3)
mtext("Spatial lag aid",side=2,cex=1.2,line=2.5)


## Local moran
lm<-localmoran(ds$mean.p_ms_cerp,listw=W)
districts$lm<-abs(lm[,4]) ## Extract z-scores
brks<-c(0,.5,1,1.5,2,3,5,6,7)
col.regions=brewer.pal(9,"Greys")
par(mar = c(2, 0, 2.5, 2))
plot(districts,
     col=col.regions[findInterval(districts$lm,brks,all.inside=TRUE)],
     axes=F,cex.main=2,lwd=.5,
     main="Local Moran Z-score")
legend(37,33, legend=leglabs(brks), fill=col.regions, bty="n",cex=1.2,
       y.intersp=.5)

