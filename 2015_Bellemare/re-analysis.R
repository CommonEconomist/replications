# Replication "Rising Food Prices, Food Price Volatility, and Social Unrest"
# Bellemare (2015)
# Examining the use of instrumental variables.
# Commodity data taken from:
# http://www.imf.org/external/np/res/commod/index.aspx
# Manufactures Unit Value Index taken from:
# http://data.worldbank.org/data-catalog/MUV-index
# This version:  11-07-2016
# First version: 16-09-2015

#### Clean commodity price data
# Add data on other commodities to original data.
# Check whether the count of natural disasters could also be
# a good instrument for other commodity price series, 
# such as a metal index, oil prices.
# Additionally, check effect on coffee as it is a cash crop
require(foreign)
dta<-read.dta("2015_Bellemare/BellemareAJAEFoodRiots.dta") # Original data
imf<-read.csv("2015_Bellemare/imf.csv",header=TRUE) # IMF commodity prices
muv<-read.csv("2015_Bellemare/muv.csv",header=TRUE) # MUV index

# Coffee (average of Arabica and Robusta)
coffee<-ts(data=(imf$PCOFFOTM+imf$PCOFFROB)/2,frequency=12,start=c(1980,1))
coffee<-window(coffee,c(1990,1),c(2011,12))
dta$coffee<-c(coffee,NA,NA,NA,NA,NA)

# Metal index
metal<-ts(data=imf$PMETA,frequency=12,start=c(1980,1))
metal<-window(metal,c(1990,1),c(2011,12))
dta$metal.ind<-c(metal,NA,NA,NA,NA,NA)

# Oil
oil<-ts(data=imf$POILAPSP,frequency=12,start=c(1980,1))
oil<-window(oil,c(1990,1),c(2011,12))
dta$oil<-c(oil,NA,NA,NA,NA,NA)

## Add MUV data and deflate
dta<-merge(dta,muv,all.x=TRUE)
dta<-dta[order(dta$year,dta$month),]

## Deflate
dta$coffee_r<-dta$coffee/(dta$muv/100)
dta$oil_r<-dta$oil/(dta$muv/100)
dta$metal_r<-dta$metal.ind/(dta$muv/100)

## Calculate volatility
require(DataCombine)
dta<-dta[order(dta$t),]
dta<-slide(dta,Var="coffee_r",NewVar="l1",slideBy=-1)
dta<-slide(dta,Var="coffee_r",NewVar="l2",slideBy=-2)
dta$coffee_var<-rowMeans(dta[,c("coffee_r","l1","l2")])/
  sd(dta$coffee_r,na.rm=TRUE)

dta<-slide(dta,Var="metal_r",NewVar="l1",slideBy=-1)
dta<-slide(dta,Var="metal_r",NewVar="l2",slideBy=-2)
dta$metal_var<-rowMeans(dta[,c("metal_r","l1","l2")])/
  sd(dta$metal_r,na.rm=TRUE)

dta<-slide(dta,Var="oil_r",NewVar="l1",slideBy=-1)
dta<-slide(dta,Var="oil_r",NewVar="l2",slideBy=-2)
dta$oil_var<-rowMeans(dta[,c("oil_r","l1","l2")])/
  sd(dta$oil_r,na.rm=TRUE)

## Subset data removing NAs (N=262)
df<-na.omit(dta[,c("counts_all","food_r","cereals_r","count",
                   "coef_var_r3","counts_all_1","coef_var_cereals_r3",
                   "t","month","coffee","metal.ind","oil","metal_r","oil_r",
                   "coffee_r","coffee_var","metal_var","oil_var")])

#### Figure: time-series commodity prices
food<-ts(data=df$food_r,frequency=12,start=c(1990,1))
cereals<-ts(data=df$cereals_r,frequency=12,start=c(1990,1))
oil<-ts(data=df$oil_r,frequency=12,start=c(1990,1))
metal<-ts(data=df$metal_r,frequency=12,start=c(1990,1))
coffee<-ts(data=df$coffee_r,frequency=12,start=c(1990,1))
violence<-ts(data=df$counts_all,frequency=12,start=c(1990,1))

## Figure: Commodity prices over time
par(mar=c(4,4,1,4),las=1,mfrow=c(4,1),cex.axis=1.5)
plot(metal,ylim=c(20,220),type="l",lwd=3,axes=FALSE,xlab="",ylab="")
text(1991,180,"Metal price index",cex=1.7)
axis(2,las=1,tick=FALSE)
plot(oil,ylim=c(20,220),type="l",lwd=3,axes=FALSE,xlab="",ylab="")
text(1991,180,"Oil price index",cex=1.7)
axis(2,las=1,tick=FALSE)
plot(coffee,ylim=c(20,220),type="l",lwd=3,axes=FALSE,xlab="",ylab="")
text(1991,180,"Coffee price index",cex=1.7)
axis(2,las=1,tick=FALSE)
plot(food,ylim=c(20,220),type="l",lwd=3,axes=FALSE,xlab="",ylab="")
lines(cereals,lty=2,lwd=2)
text(1993,180,"Food and cereal (dashed) price index",cex=1.7)
axis(2,las=1,tick=FALSE)
axis(1,at=seq(1990,2015,2),tick=FALSE, cex.axis=1.7)

## Figure: Violence versus commodity prices
par(mar=c(3,5,2,2),las=1,mfrow=c(1,1),cex.axis=1.5)
plot(violence,ylim=c(0,450),type="h",lwd=3,axes=FALSE,
     xlab="",ylab="",col="grey60")
lines(cereals,lwd=2,lty=2)
lines(oil,col="black",lwd=2)

# Axis
axis(1,at=seq(1990,2012,2),tick=FALSE,cex.axis=1.7)
axis(2,las=1,tick=FALSE)
text(1994,400,
     "Food related civil unrest and \n oil/cereal (dashed) price index",cex=1.7)

#### Replication ####
# Able to replicate the results in R
# at the reported number of decimals 
# and same level of statistical significance.

## Table 2; OLS estimation
m1<-lm(counts_all~food_r+coef_var_r3+counts_all_1+t+factor(month),df)
summary(m1)
m2<-lm(counts_all~cereals_r+coef_var_cereals_r3+counts_all_1+t+factor(month),df)
summary(m2)

## Table 3: IV-2SLS estimation
s1<-lm(food_r~count+coef_var_r3+counts_all_1+t+factor(month),df)
df$yhat<-fitted(s1)
summary(s1)
s2<-lm(counts_all~yhat+coef_var_r3+counts_all_1+t+factor(month),df)
summary(s2)

s3<-lm(cereals_r~count+coef_var_cereals_r3+counts_all_1+t+factor(month),df)
df$yhat<-fitted(s3)
s4<-lm(counts_all~yhat+coef_var_cereals_r3+counts_all_1+t+factor(month),df)
summary(s4)

#### Check instruments ####
## Metal price index
a1<-lm(metal_r~count+metal_var+counts_all_1+t+factor(month),df)
summary(a1)
df$yhat<-fitted(a1)
a2<-lm(counts_all~yhat+coef_var_r3+counts_all_1+t+factor(month),df)
summary(a2)

## Oil prices
a3<-lm(oil_r~count+oil_var+counts_all_1+t+factor(month),df)
summary(a3)
df$yhat<-fitted(a3)
a4<-lm(counts_all~yhat+oil_var+counts_all_1+t+factor(month),df)
summary(a4)

## Coffee prices
a5<-lm(coffee_r~count+coffee_var+counts_all_1+t+factor(month),df)
summary(a5)
df$yhat<-fitted(a5)
a6<-lm(counts_all~yhat+coffee_var+counts_all_1+t+factor(month),df)
summary(a6)
