## AUC PLOT

setwd("~/Dropbox/Sandbox/geriatrix")

## Libraries
library(pROC)
library(foreign)
library(lmtest)
library(sandwich)

## Load data
d<-read.dta("raw_data/table2.dta")

## Estimate model
# Table 2, model 2 (OLS)
m2<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+region_nNUNN+
         region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+f_french+f_spain+
         f_pothco+f_dutch+f_belg+f_italy+f_germ+abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+ln_coastline_areaNUNN+
         island_dumNUNN+islam+legor_frNUNN+ln_avg_gold_pop+ln_avg_oil_pop+
         ln_avg_all_diamonds_pop+ETHPOL+yellow+rugged, data=d);summary(m2)

# Extract the t-values (robust)
t<-coeftest(m2,vcov.=vcovHC(m2,type="HC1"))
t.values<-t[2:27,3]

# Extract data
obs<-m2$model
y<-as.numeric(obs$CivilWarIncidence>0)
auc(y,fitted(m2))

#### Estimate model permutations ####

a1<-lm(CivilWarIncidence~lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a2<-lm(CivilWarIncidence~WarPrevalence14001700+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a3<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a4<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a5<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a6<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a7<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a8<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a9<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a10<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a11<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a12<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a13<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a14<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a15<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a16<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a17<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a18<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a19<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a20<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a21<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a22<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow+rugged, data=d)
a23<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+
         ETHPOL+yellow+rugged, data=d)
a24<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         yellow+rugged, data=d)
a25<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+rugged, data=d)
a26<-lm(CivilWarIncidence~WarPrevalence14001700+lrgdpl2631970+
         region_nNUNN+region_sNUNN+region_wNUNN+region_eNUNN+region_cNUNN+
         f_french+f_spain+f_pothco+f_dutch+f_belg+f_italy+f_germ+
         abs_latitudeNUNN+longitudeNUNN+
         rain_minNUNN+humid_maxNUNN+low_tempNUNN+
         ln_coastline_areaNUNN+island_dumNUNN+
         islam+legor_frNUNN+
         ln_avg_gold_pop+ln_avg_oil_pop+ln_avg_all_diamonds_pop+
         ETHPOL+yellow, data=d)

#### Calculate AUC ####
auc<-vector(mode="numeric", length=0)

auc[1]<-auc(y,fitted(m2))
auc[2]<-auc(y,fitted(a1))
auc[3]<-auc(y,fitted(a2))
auc[4]<-auc(y,fitted(a3))
auc[5]<-auc(y,fitted(a4))
auc[6]<-auc(y,fitted(a5))
auc[7]<-auc(y,fitted(a6))
auc[8]<-auc(y,fitted(a7))
auc[9]<-auc(y,fitted(a8))
auc[10]<-auc(y,fitted(a9))
auc[11]<-auc(y,fitted(a10))
auc[12]<-auc(y,fitted(a11))
auc[13]<-auc(y,fitted(a12))
auc[14]<-auc(y,fitted(a13))
auc[15]<-auc(y,fitted(a14))
auc[16]<-auc(y,fitted(a15))
auc[17]<-auc(y,fitted(a16))
auc[18]<-auc(y,fitted(a17))
auc[19]<-auc(y,fitted(a18))
auc[20]<-auc(y,fitted(a19))
auc[21]<-auc(y,fitted(a20))
auc[22]<-auc(y,fitted(a21))
auc[23]<-auc(y,fitted(a22))
auc[24]<-auc(y,fitted(a23))
obs<-a24$model
y2<-as.numeric(obs$CivilWarIncidence>0)
auc[25]<-auc(y2,fitted(a24))
auc[26]<-auc(y,fitted(a25))
auc[27]<-auc(y,fitted(a26))

## Calculate AUC difference
z<-vector(mode="numeric", length=0)
z[1]<-auc[1]-auc[2]
z[2]<-auc[1]-auc[3]
z[3]<-auc[1]-auc[4]
z[4]<-auc[1]-auc[5]
z[5]<-auc[1]-auc[6]
z[6]<-auc[1]-auc[7]
z[7]<-auc[1]-auc[8]
z[8]<-auc[1]-auc[9]
z[9]<-auc[1]-auc[10]
z[10]<-auc[1]-auc[11]
z[11]<-auc[1]-auc[12]
z[12]<-auc[1]-auc[13]
z[13]<-auc[1]-auc[14]
z[14]<-auc[1]-auc[15]
z[15]<-auc[1]-auc[16]
z[16]<-auc[1]-auc[17]
z[17]<-auc[1]-auc[18]
z[18]<-auc[1]-auc[19]
z[19]<-auc[1]-auc[20]
z[20]<-auc[1]-auc[21]
z[21]<-auc[1]-auc[22]
z[22]<-auc[1]-auc[23]
z[23]<-auc[1]-auc[24]
z[24]<-auc[1]-auc[25]
z[25]<-auc[1]-auc[26]
z[26]<-auc[1]-auc[27]

t.values<-abs(t.values) 

#### Plot the data ####
par(mar=c(5,7,1,1))
plot(t.values,z,type="p",bty="n",xlab="",ylab="",
     pch=19,cex=1.5,axes=FALSE)

# Label points
h.adj<-c(0.03,0.03,-0.1,0.03,0.03,
         0.03,0.03,0.03,0.03,0.03,
         0.03,0.03,0.03,0.03,0.03,
         0.03,-0.23,0.03,0.03,0.03,
         0.03,0.03,0.03,0.03,0.03,0.03)
v.adj<-c(0,0,-0.002,0,0,
         0,0,0,0,0,
         0,0,0,0,0,
         -0.002,0,0,0,0,
         0,0,0,0,0,0)

var.names<-c("Historical conflict","GDP per cap. 1970",
             "North region","South region","West region","East region",
             "Former French","Former Portugues","Former Belgian",
             "Former Italian","Former German","Latitude","Longitude","Rain",
             "Humidity","Temperature","Coastline","Island",
             "Muslims","Legal origin","Gold","Oil","Diamonds",
             "Ethnicity","Yellow fever","Rugged terrain")



text(t.values+h.adj,z+v.adj,var.names,adj=0)

# Axes
axis(1,las=1,at=seq(0,3,0.5),tck=0.02,cex.axis=1.2)
axis(2,las=1,at=seq(-0.05,0.05,0.01),tck=0.02,cex.axis=1.2)
axis(1,at=seq(0,3,0.5),tck=0.01,labels=FALSE)
mtext("Change in predictive power (AUC)",side=2,cex=1.2,line=4)
mtext("Absolute t-values (robust)",side=1,cex=1.2,line=3)

# Fit regression line
abline(lm(z~t.values-1),lty=2,lwd=2)
text(2.7,0.0065,labels=c("Best fit line through 0"),srt=3,pos=3,cex=1.1)

