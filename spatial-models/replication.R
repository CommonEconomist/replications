# Replication Franzese & Hays (2007)
# "Spatial Econometric Models of Cross-Sectional Interdependence in 
# Political Science Panel and Time-Series-Cross-Section Data."
# Political Analysis 15:140-164
# https://academic.oup.com/pan/article-abstract/15/2/140/1564984
options(scipen=10)
require(MASS)

# Load data
fh<-read.csv('almMLire.csv',header=TRUE) 
adj<-as.matrix(read.csv('almWeights.csv',header=FALSE)) # Adjacency matrix

# Row-standardize adjacency matrix
noNeigh<-apply(adj,1,sum)
noNeigh[7]<-.01 # Greece has no neighbours; can't have denominator 0
wmat<-adj/noNeigh # Spatial weight matrix

# Calculate spatial lag of outcome variable, using Kronekcer product
N<-length(unique(fh$cc))
Ti<-length(unique(fh$year))
I<-diag(1,nrow=Ti) 
bigW<-(I%x%wmat)
fh$SpatLag<-(I%x%wmat)%*%fh$lnlmtue # Spatial lag

#### REPLICATE TABLE 4 ####

# Column 1. Simple OLS model
m1<-lm(lnlmtue~lnlmtue_1+DENSITY+DEIND+lngdp_pc+UR+TRADE+FDI+LLVOTE+LEFTC+
         TCDEMC+GOVCON+OLDAGE+as.factor(cc)+as.factor(year),data=fh)

summary(m1) # Gives roughly the same results as in the paper

# Column 2. OLS model including a spatial lag
m2<-lm(lnlmtue~lnlmtue_1+SpatLag+DENSITY+DEIND+lngdp_pc+UR+TRADE+FDI+
         LLVOTE+LEFTC+TCDEMC+GOVCON+OLDAGE+as.factor(cc)+as.factor(year),
              data=fh)
summary(m2) # Again roughly the same results

# Column 3. Spatial 2SLS

# Spatial lag of all the explanatory variables (probably a quicker way)
fh$l.lnlmtue_1<-(I%x%wmat)%*%fh$lnlmtue_1
fh$l.DENSITY<-(I%x%wmat)%*%fh$DENSITY
fh$l.DEIND<-(I%x%wmat)%*%fh$DEIND
fh$l.lngdp_pc<-(I%x%wmat)%*%fh$lngdp_pc
fh$l.UR<-(I%x%wmat)%*%fh$UR
fh$l.TRADE<-(I%x%wmat)%*%fh$TRADE
fh$l.FDI<-(I%x%wmat)%*%fh$FDI
fh$l.LLVOTE<-(I%x%wmat)%*%fh$LLVOTE
fh$l.LEFTC<-(I%x%wmat)%*%fh$LEFTC
fh$l.TCDEMC<-(I%x%wmat)%*%fh$TCDEMC
fh$l.GOVCON<-(I%x%wmat)%*%fh$GOVCON
fh$l.OLDAGE<-(I%x%wmat)%*%fh$OLDAGE

# First stage regression
stage.1<-lm(SpatLag~l.lnlmtue_1+l.DENSITY+l.DEIND+l.lngdp_pc+l.UR+l.TRADE+
              l.FDI+l.LLVOTE+l.LEFTC+l.TCDEMC+l.GOVCON+l.OLDAGE, data=fh)
fh$iv.Lag<-stage.1$fitted.values # Write predicted values to variable

# Second stage estimation
m3<-lm(lnlmtue~lnlmtue_1+iv.Lag+DENSITY+DEIND+lngdp_pc+UR+TRADE+FDI+LLVOTE+
         LEFTC+TCDEMC+GOVCON+OLDAGE+ as.factor(cc)+as.factor(year),
              data=fh)
summary(m3)
# Slightly different results, this is possibly due to the fact that not all Xs were modelled in the first stage. Close enough anyway.

# Column 4. Spatial Maximum Likelihood (GLM model)
m4<-glm(lnlmtue~lnlmtue_1+SpatLag+DENSITY+DEIND+lngdp_pc+UR+TRADE+FDI+LLVOTE+
          LEFTC+TCDEMC+GOVCON+OLDAGE+as.factor(cc)+as.factor(year),
        family=gaussian,data=fh)
summary(m4)

# Can see that there is quite some difference in the estimates
# Probably not the right ML estimator

# Try estimating the model with 'spml'
# NB - using fixed effect not possible
require(splm)
m4a<-spml(lnlmtue~lnlmtue_1+DENSITY+DEIND+lngdp_pc+UR+TRADE+FDI+LLVOTE+LEFTC+
         TCDEMC+GOVCON+OLDAGE,data=fh,index=c("cc","year"),
         listw=mat2listw(wmat),model="pooling",spatial.error="none",lag=TRUE)
summary(m4a) # Results are quite different
