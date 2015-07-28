#******************************************************************************
# This version:  18-07-2015
# First version: 24-06-2015
# Replication Brückner & Ciccone (2010)
# International Commodity Prices, Growth and 
# the Outbreak of Civil War in Sub-Saharan Africa
# Updating and extending the data
#******************************************************************************

setwd("[SET DIR]/Replications/2010_Bruckner_Ciccone")
options(scipen=7)

## Libraries
library(foreign)
library(stargazer)

## Load data and functions
load("tidy_data/newData.Rdata")
df<-read.dta("raw_data/data.dta")

source("code/clse.R") # For robust standard clustered errors

mse <- function(sm) { 
    mse <- mean(sm$residuals^2)
    return(mse)
}

#### Descriptive statistics ####
stargazer(df.New[,c("war.onset","onset","ind","index.g","gdp.g")],
          type="text",digits=2,median=TRUE)

## Conflict-onset years
onset<-df.New[,c("country","year","war.onset")]
onset<-na.omit(onset[onset$war.onset==1,])

## Create data frames
t1<-na.omit(df.New[,c(1,2,7:10,20)]) # Civil war
t2<-na.omit(df.New[,c(1,2,7:10,18)]) # Civil conflict
t3<-na.omit(df.New[,c(1,2,7:11,20)]) # Civil war, income
t4<-na.omit(df.New[,c(1,2,7:11,18)]) # Civil conflict, income
t0<-t1[t1$year<=2006,]               # Original period, most recent data

#### Commodity price shocks and civil war onset ####
# Table in annex

# Replication Table 2 column 1 1981-2006
m1o<-lm(war.onset~index.g+index.g.l+index.g.l2+
            factor(ccode)+factor(year)+factor(ccode)*year,t0)
clse(m1o,1,m1o$model[,5])
summary(m1o);mse(m1o)

# Replication Table 2 column 5 1981-2006
m5o<-lm(war.onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t0)
clse(m5o,1,m5o$model[,3])
summary(m5o);mse(m5o)

# Replication Table 2 column 1 1981-2013
m1<-lm(war.onset~index.g+index.g.l+index.g.l2+
            factor(ccode)+factor(year)+factor(ccode)*year,t1)
clse(m1,1,m1$model[,5])
summary(m1);mse(m1)

# Replication Table 2 column 5 1981-2013
m5<-lm(war.onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t1)
clse(m5,1,m5$model[,3])
summary(m5);mse(m5)

# Replication Table 2 column 1 1981-2013, civil conflict
m1a<-lm(onset~index.g+index.g.l+index.g.l2+
            factor(ccode)+factor(year)+factor(ccode)*year,t2)
clse(m1a,1,m1a$model[,5])
summary(m1a);mse(m1a)

# replication Table 2 column 5 1981-2013, civil conflict
m5a<-lm(onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t2)
clse(m5a,1,m5a$model[,3])
summary(m5a);mse(m5a)

#### Table: Economic growth and civil war onset 1981-2013 ####
# IV-2SLS, in main text

# Column 1
w1<-lm(gdp.g~ind+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(w1,1,w1$model[,3])
summary(w1);mse(w1)

# Column 2
w2<-lm(war.onset~gdp.g+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(w2,1,w2$model[,3])
summary(w2);mse(w2)

# Column 3
w3<-lm(war.onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(w3,1,w3$model[,3])
summary(w3);mse(w3)

# Column 4
t3$yhat<-predict(w1)
iv1<-lm(war.onset~yhat+factor(ccode)+factor(year)+factor(ccode)*year,t3)
clse(iv1,1,iv1$model[,3])
summary(iv1);mse(iv1)

#### Table: Economic growth and civil conflict onset ####
# IV-2SLS with civil conflict

# Column 1
c1<-lm(gdp.g~ind+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(c1,1,c1$model[,3])
summary(c1);mse(c1)

# Column 2
c2<-lm(onset~gdp.g+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(c2,1,c2$model[,3])
summary(c2);mse(c2)

# Column 3
c3<-lm(onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(c3,1,c3$model[,3])
summary(c3);mse(c3)

# Column 4
t4$yhat<-predict(c1)
iv2<-lm(onset~yhat+factor(ccode)+factor(year)+factor(ccode)*year,t4)
clse(iv2,1,iv2$model[,3])
summary(iv2);mse(iv2)


#### UPDATE: 13-07-2015 ####

## Check changes in war data
a<-df[df$war_prio_on==1,]
table(a$country,a$year)

a<-df.New[df.New$war.onset==1,]
table(a$country,a$year)

# Estimate model checking influence of data changes in 
# outcome and explanatory variables
outcome<-df.New[,c("year","ccode","war.onset","gdp.g","ind")]
colnames(outcome)[2]<-"countryisocode"
colnames(outcome)[5]<-"ind3"
df2<-merge(df,outcome,all.x=TRUE)

#### New outcome variable ####
d<-df2[!is.na(df2$war.onset) & !is.na(df2$ind),]

# First stage estimation
n1<-lm(gdp_g~ind+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(n1,1,n1$model[,3])
summary(n1);mse(n1)

# Economic growth on war onset
n2<-lm(war.onset~gdp_g+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(n2,1,n2$model[,3])
summary(n2);mse(n2)

# Reduced form
n3<-lm(war.onset~ind+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(n3,1,n3$model[,3])
summary(n2);mse(n2)

# Second stage
d$yhat<-predict(n1)
iv4<-lm(war.onset~yhat+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(iv4,1,iv4$model[,3])
mse(iv4)

#### New income data ####
d<-df2[!is.na(df2$war_prio_on) & !is.na(df2$ind) & !is.na(df2$gdp.g),]

# First stage estimation
i1<-lm(gdp.g~ind+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(i1,1,i1$model[,3])
summary(i1);mse(i1)

# Economic growth on war onset
i2<-lm(war_prio_on~gdp.g+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(i2,1,i2$model[,3])
summary(i2);mse(i2)

# Reduced form
i3<-lm(war_prio_on~ind+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(i3,1,i3$model[,3])
summary(i3);mse(i3)

# Second stage
d$yhat<-predict(i1)
iv5<-lm(war_prio_on~yhat+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(iv5,1,iv5$model[,3])
mse(iv5)

#### New index data ####
d<-df2[!is.na(df2$war_prio_on) & !is.na(df2$ind3),]

# First stage estimation
p1<-lm(gdp_g~ind3+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(p1,1,p1$model[,3])
summary(p1);mse(p1)

# Economic growth on war onset
p2<-lm(war_prio_on~gdp.g+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(p2,1,p2$model[,3])
summary(p2);mse(p2)

# Reduced form
p3<-lm(war_prio_on~ind3+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(p3,1,p3$model[,3])
summary(p3);mse(p3)

# Second stage
d$yhat<-predict(p1)
iv6<-lm(war_prio_on~yhat+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(iv6,1,iv6$model[,3])
mse(iv6)
