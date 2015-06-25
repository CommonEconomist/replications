#******************************************************************************
# This version:  24-06-2015
# First version: 21-06-2015
# Replication Brückner & Ciccone (2010)
# International Commodity Prices, Growth and 
# the Outbreak of Civil War in Sub-Saharan Africa
#******************************************************************************

setwd("[SET DIR]/2010_Bruckner_Ciccone")
options(scipen=7)

## Libraries
library(foreign)
library(AER)
library(MASS)
library(mfx)
library(Zelig)

## Load data and functions
df<-read.dta("data.dta")
source("clse.R") # For robust standard clustered errors
mse <- function(sm) { 
    mse <- mean(sm$residuals^2)
    return(mse)
}

## Data for IV-estimation and vector for clustered errors
d<-df[!is.na(df$war_prio_on) & !is.na(df$ind),]

#### Table 2: Commodity price shocks and civil war onset ####

# Model 1
m1<-lm(war_prio_on~index_g+index_g_l+index_g_l2+
            factor(ccode)+factor(year)+factor(ccode)*year,df)
clse(m1,1,m1$model[,5])
summary(m1);mse(m1)

# Model 2 
m2<-lm(war_prio_on~gpcp_g+gpcp_g_l+gpcp_g_l2+
            factor(ccode)+factor(year)+factor(ccode)*year,df[!is.na(df$ind),])
clse(m2,1,m2$model[,5])
summary(m2);mse(m2)

# Model 3
m3<-lm(war_prio_on~index_g+index_g_l+index_g_l2+gpcp_g+gpcp_g_l+gpcp_g_l2+
            factor(ccode)+factor(year)+factor(ccode)*year,df[!is.na(df$ind),])
clse(m3,1,m3$model[,5])
summary(m3);mse(m3)

# Model 4
m4<-lm(war_prio_on~index_g+index_g_l+index_g_l2+lgpcp_l+lgpcp_l2+lgpcp_l3+
            factor(ccode)+factor(year)+factor(ccode)*year,df[!is.na(df$ind),])
clse(m4,1,m4$model[,5])
summary(m4);mse(m4)

# Model 5
m5<-lm(war_prio_on~ind+factor(ccode)+factor(year)+factor(ccode)*year,df)
clse(m5,1,m5$model[,5])
summary(m5);mse(m5)

# Model 6
m6<-lm(war_prio_on~ind+rain3+factor(ccode)+factor(year)+factor(ccode)*year,df)
clse(m6,1,m6$model[,5])
summary(m6);mse(m6)

#### Table 3: Limited dependent variable estimates ####

# Model 1 
l1<-lm(war_prio_on~ind,df)
clse(l1,1,d$ccode)

# Model 2 
l2<-glm(war_prio_on~ind,df,family=binomial(link = "probit"))
ml2<-probitmfx(war_prio_on~ind,df);ml2
clse(ml2$fit,1,d$ccode)

# Model 3
l3<-glm(war_prio_on~ind,df,family=binomial(link ="logit"))
ml3<-logitmfx(war_prio_on~ind,df);ml3
clse(ml3$fit,1,d$ccode)

# Model 4
l4<-glm(war_prio_on~ind,df,family=binomial(link ="logit"))
clse(l4,1,d$ccode)

# Model 5
l5<-relogit(war_prio_on~ind,df)
summary(l5)
clse(l5,1,d$ccode)

# Model 6 
l6<-glm(war_prio_on~ind+factor(ccode),df,family=binomial(link ="logit"))
clse(l6,1,d$ccode)

# Model 7
l7<-glm(war_prio_on~ind+factor(ccode)+factor(year)+factor(ccode)*year,
        df,family=binomial(link ="logit"))
clse(l7,1,d$ccode)

#### Table 4: Economic growth and civil war onset ####

# Model 1
g1<-lm(gdp_g~ind+factor(ccode)+factor(year)+factor(ccode)*year,
       df[!is.na(df$war_prio_on),])
clse(g1,1,g1$model[,3])
summary(g1);mse(g1)

# Model 2
g2<-lm(war_prio_on~gdp_g+factor(ccode)+factor(year)+factor(ccode)*year,
       df[!is.na(df$ind),])
clse(g2,1,g2$model[,3])
summary(g2);mse(g2)

# Model 3 --> Doesn't work properly
df$temp<-df$ccode
iv<-ivreg(war_prio_on~gdp_g+factor(ccode)+factor(year)+factor(temp)*year,
          ~ind+factor(ccode)+factor(year)+factor(temp)*year,df)
clse(iv,1,iv$model[,3])

# Model 3, alternative method
d$yhat<-predict(g1)
iv1<-lm(war_prio_on~yhat+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(iv1,1,iv1$model[,3])
summary(iv1);mse(iv1)

#### Table 5: Export demand, economic growth, and civil war onset ####

# Column 1
e1<-lm(gdp_g~ind+oecd_export+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(e1,1,e1$model[,4])
summary(e1);mse(e1)

# Column 2
e2<-lm(war_prio_on~ind+oecd_export+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(e2,1,e2$model[,4])
summary(e2);mse(e2)

# Column3
d$yhat<-predict(e1)
iv2<-lm(war_prio_on~yhat+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(iv2,1,iv2$model[,3])
mse(iv2)

#### Table 6: Economic growth and civil conflict onset ####
d<-df[!is.na(df$any_prio_on) & !is.na(df$ind),]

# Column 1
c1<-lm(gdp_g~ind+oecd_export+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(c1,1,c1$model[,4])
summary(c1);mse(c1)

# Column 2
c2<-lm(any_prio_on~ind+oecd_export+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(c2,1,c2$model[,4])
summary(c2);mse(c2)

# Column 3
c3<-lm(any_prio_on~gdp_g+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(c3,1,c3$model[,3])
summary(c3);mse(c3)

# Column 4
d$yhat<-predict(c1)
iv3<-lm(any_prio_on~yhat+factor(ccode)+factor(year)+factor(ccode)*year,d)
clse(iv3,1,iv3$model[,3])
mse(iv3)
