#******************************************************************************
# This version:  09-06-2015
# Replication Berman et al. 2013
#******************************************************************************

#### Load libraries and data ####

setwd("[SPECIFY DIR]")

## Load libraries
library(maptools)
library(foreign)
library(spdep)
library(plyr)
library(RColorBrewer)  
library(DataCombine)

## Load data
berman<-read.dta("BermanetalAER2013replication.dta")

## Functions
source("clse.R") # Robust clustered standard errors

#### Data preparation ####

## Clean up data according to original file

# Change troop levels from missing to zero in Dibis:
berman[is.na(berman$a_of_batt),]$a_of_batt<-0

# Drop Karkh
df<-berman[berman$district!="Karkh",]

## Calculate interactions

# Troop strength
df$cerp_small_batt<-df$p_ms_cerp_small*df$a_of_batt
df$cerp_large_batt<-df$p_ms_cerp_large*df$a_of_batt
df$csp_batt<-df$p_ms_csp*df$a_of_batt

# PRT
df$cerp_small_dis_usprt<-df$p_ms_cerp_small*df$dis_usprt
df$cerp_large_dis_usprt<-df$p_ms_large*df$dis_usprt
df$csp_dis_usprt<-df$p_ms_csp*df$dis_usprt

## Create lags
df<-df[order(df$district,df$halfyr),]

df<-slide(df,Var="p_S1",GroupVar="district",NewVar="l.p_S1",slideBy=-1)

# Aid 
df<-slide(df,Var="p_ms_cerp_small",
          GroupVar="district",NewVar="l.p_ms_cerp_small",slideBy=-1)
df<-slide(df,Var="p_ms_cerp_large",
          GroupVar="district",NewVar="l.p_ms_cerp_large",slideBy=-1)
df<-slide(df,Var="p_ms_small_non_cerp_recon",
          GroupVar="district",NewVar="l.p_ms_small_non_cerp_recon",slideBy=-1)
df<-slide(df,Var="p_ms_large_non_cerp_recon",
          GroupVar="district",NewVar="l.p_ms_large_non_cerp_recon",slideBy=-1)
df<-slide(df,Var="p_ms_cap",GroupVar="district",NewVar="l.p_ms_cap",slideBy=-1)
df<-slide(df,Var="p_ms_csp",GroupVar="district",NewVar="l.p_ms_csp",slideBy=-1)
df<-slide(df,Var="p_ms_usaid",GroupVar="district",NewVar="l.p_ms_usaid",slideBy=-1)

# Troops
df<-slide(df,Var="a_of_batt",GroupVar="district",NewVar="l.a_of_batt",slideBy=-1)

# Interaction troops
df<-slide(df,Var="cerp_small_batt",
          GroupVar="district",NewVar="l.cerp_small_batt",slideBy=-1)
df<-slide(df,Var="cerp_large_batt",
          GroupVar="district",NewVar="l.cerp_large_batt",slideBy=-1)
df<-slide(df,Var="csp_batt",
          GroupVar="district",NewVar="l.csp_batt",slideBy=-1)

# Interaction PRT
df<-slide(df,Var="dis_usprt",GroupVar="district",NewVar="l.dis_usprt",slideBy=-1)
df<-slide(df,Var="cerp_small_dis_usprt",
          GroupVar="district",NewVar="l.cerp_small_dis_usprt",slideBy=-1)
df<-slide(df,Var="cerp_large_dis_usprt",
          GroupVar="district",NewVar="l.cerp_large_dis_usprt",slideBy=-1)
df<-slide(df,Var="csp_dis_usprt",
          GroupVar="district",NewVar="l.csp_dis_usprt",slideBy=-1)

## Calculate first differences

# Outcome
df$d.p_S1<-df$p_S1-df$l.p_S1

# Aid
df$d.p_ms_cerp_small<-df$p_ms_cerp_small-df$l.p_ms_cerp_small
df$d.p_ms_cerp_large<-df$p_ms_cerp_large-df$l.p_ms_cerp_large
df$d.p_ms_small_non_cerp_recon<-df$p_ms_small_non_cerp_recon-
  df$l.p_ms_small_non_cerp_recon
df$d.p_ms_large_non_cerp_recon<-df$p_ms_large_non_cerp_recon-
  df$l.p_ms_large_non_cerp_recon
df$d.p_ms_cap<-df$p_ms_cap-df$l.p_ms_cap
df$d.p_ms_csp<-df$p_ms_csp-df$l.p_ms_csp
df$d.p_ms_usaid<-df$p_ms_usaid-df$l.p_ms_usaid


# Troops and interaction troops
df$d.a_of_batt<-df$a_of_batt-df$l.a_of_batt

df$d.cerp_small_batt<-df$cerp_small_batt-df$l.cerp_small_batt
df$d.cerp_large_batt<-df$cerp_large_batt-df$l.cerp_large_batt
df$d.csp_batt<-df$csp_batt-df$l.csp_batt

# PRT and interation PRT
df$d.dis_usprt<-df$dis_usprt-df$l.dis_usprt
df$d.cerp_small_dis_usprt<-df$cerp_small_dis_usprt-df$l.cerp_small_dis_usprt
df$d.cerp_large_dis_usprt<-df$cerp_large_dis_usprt-df$l.cerp_large_dis_usprt
df$d.csp_dis_usprt<-df$csp_dis_usprt-df$l.csp_dis_usprt


# Lag of outcome variable and troops
df<-df[order(df$district,df$halfyr),]
df<-slide(df,Var="d.p_S1",GroupVar="district",NewVar="dl.p_S1",slideBy=-1)
df<-slide(df,Var="d.a_of_batt",GroupVar="district",NewVar="dl.a_of_batt",slideBy=-1)

vec<-na.omit(df)

#### Regressions ####

#### Table 1 ####

## Column 1
a1<-lm(d.p_S1~d.p_ms_cerp_small+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,
       df,weights=df$POP)
clse(a1,1,vec$district)

## Column 2
a2<-lm(d.p_S1~d.p_ms_cerp_large+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(a2,1,vec$district)

## Column 3
a3<-lm(d.p_S1~d.p_ms_large_non_cerp_recon+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(a3,1,vec$district)

## Column 4
a4<-lm(d.p_S1~d.p_ms_small_non_cerp_recon+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(a4,1,vec$district)

## Column 5
a5<-lm(d.p_S1~d.p_ms_csp+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(a5,1,vec$district)

## Column 6
a6<-lm(d.p_S1~d.p_ms_cap+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(a6,1,vec$district)

## Column 7
a7<-lm(d.p_S1~d.p_ms_usaid+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(a7,1,vec$district)


#### Table 2 ####

## Column 1
b1<-lm(d.p_S1~d.p_ms_cerp_small+d.cerp_small_batt+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(b1,1,vec$district)

## Column 2
b2<-lm(d.p_S1~d.p_ms_cerp_large+d.cerp_large_batt+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(b2,1,vec$district)

## Column 3
b3<-lm(d.p_S1~d.p_ms_csp+d.csp_batt+dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(b3,1,vec$district)

#### Table 3 ####

## Column 1
c1<-lm(d.p_S1~d.p_ms_cerp_small+d.cerp_small_dis_usprt+d.dis_usprt+
         dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(c1,1,vec$district)

## Column 2
c2<-lm(d.p_S1~d.p_ms_cerp_large+d.cerp_large_dis_usprt+d.dis_usprt+
         dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(c2,1,vec$district)

## Column 3
c3<-lm(d.p_S1~d.p_ms_csp+d.csp_dis_usprt+d.dis_usprt+
         dl.p_S1+d.a_of_batt+dl.a_of_batt+
         yr3+yr4+yr5+su_vyr3+su_vyr4+su_vyr5,df,weights=df$POP)
clse(c3,1,vec$district)








