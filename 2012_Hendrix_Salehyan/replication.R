#### Replication Hendrix & Salehyan (2012) ####
# http://jpr.sagepub.com/content/49/1/35.abstract
# Estimation results show that the conditional fixed effects models 
# cannot be replicated in R.
# However, note that the estimates for these models are likely inconsistent
# due to the inclusion of the lagged outcome variable.
# See:
# http://www.jstor.org/stable/2235494
# http://www.sciencedirect.com/science/article/pii/S0304407601001087

## Load data
require(foreign)
dta<-read.dta("~/Desktop/H_S_JPR_491_Replication_Revised.dta")
options(scipen=4)                     

#### Table 2: Effect of rainfall on conflict onset ####

## Prepare data
test<-na.omit(dta[,c("conflict_onset","incidence_l","GPCP_precip_mm_deviation_sd",
       "GPCP_precip_mm_deviation_sd_sq","GPCP_precip_mm_deviation_sd_l",
       "GPCP_precip_mm_deviation_sd_l_sq","polity2_l","polity2_sq_l",
       "log_pop_pwt_l","log_pop_pwt_fd_rescaled_l","log_rgdpch_pwt_l",
       "grgdpch_pwt_l","peaceyears","peaceyears2","peaceyears3","ccode")])

## Estimate model
m1<-glm(conflict_onset~.-ccode,test,family=binomial(link="logit"))
clse(m1,1,test$ccode)

#### Table 3: Effect of rainfall on different types of unrest ####

## Prepare data
require(MASS)
dta2<-dta[dta$ccode!=520,] # Remove Somalia

## Include variables of interest (N=765)
test<-na.omit(dta2[,c("events_no_onset","events_no_onset_l",
                      "non_violent_events","non_violent_events_l",
                      "violent_events_no_onset","violent_events_no_onset_l",
                      "gov_targeted_events_no_onset","gov_targeted_events_no_onset_l",
                      "nongov_targeted_events","nongov_targeted_events_l",
                      "GPCP_precip_mm_deviation_sd","GPCP_precip_mm_deviation_sd_sq",
                      "GPCP_precip_mm_deviation_sd_l","GPCP_precip_mm_deviation_sd_l_sq",
                      "polity2","polity2_sq","log_pop_pwt","log_pop_pwt_fd",
                      "log_rgdpch_pwt","grgdpch_pwt","incidence",
                      "ttrend","year","ccode")])
## Total events
m2<-glm.nb(events_no_onset~events_no_onset_l+GPCP_precip_mm_deviation_sd+
              GPCP_precip_mm_deviation_sd_sq+GPCP_precip_mm_deviation_sd_l+
              GPCP_precip_mm_deviation_sd_l_sq+polity2+polity2_sq+log_pop_pwt+
              log_pop_pwt_fd+log_rgdpch_pwt+grgdpch_pwt+incidence+ttrend+factor(year),
            test);clse(m2,1,test$ccode)

m3<-glm.nb(events_no_onset~events_no_onset_l+GPCP_precip_mm_deviation_sd+
              GPCP_precip_mm_deviation_sd_sq+GPCP_precip_mm_deviation_sd_l+
              GPCP_precip_mm_deviation_sd_l_sq+polity2+polity2_sq+log_pop_pwt+
              log_pop_pwt_fd+log_rgdpch_pwt+grgdpch_pwt+incidence+ttrend+
              factor(year)+factor(ccode),test);summary(m3)

## Non-violent events
m4<-glm.nb(non_violent_events~non_violent_events_l+GPCP_precip_mm_deviation_sd+
              GPCP_precip_mm_deviation_sd_sq+GPCP_precip_mm_deviation_sd_l+
              GPCP_precip_mm_deviation_sd_l_sq+polity2+polity2_sq+log_pop_pwt+
              log_pop_pwt_fd+log_rgdpch_pwt+grgdpch_pwt+incidence+ttrend+factor(year),
            test);clse(m4,1,test$ccode)

m5<-glm.nb(non_violent_events~non_violent_events_l+GPCP_precip_mm_deviation_sd+
              GPCP_precip_mm_deviation_sd_sq+GPCP_precip_mm_deviation_sd_l+
              GPCP_precip_mm_deviation_sd_l_sq+polity2+polity2_sq+log_pop_pwt+
              log_pop_pwt_fd+log_rgdpch_pwt+grgdpch_pwt+incidence+ttrend+
              factor(year)+factor(ccode),test);summary(m5)

## Violent events
m6<-glm.nb(violent_events_no_onset~violent_events_no_onset_l+GPCP_precip_mm_deviation_sd+
              GPCP_precip_mm_deviation_sd_sq+GPCP_precip_mm_deviation_sd_l+
              GPCP_precip_mm_deviation_sd_l_sq+polity2+polity2_sq+log_pop_pwt+
              log_pop_pwt_fd+log_rgdpch_pwt+grgdpch_pwt+incidence+ttrend+factor(year),
            test);clse(m6,1,test$ccode)

m7<-glm.nb(violent_events_no_onset~violent_events_no_onset_l+GPCP_precip_mm_deviation_sd+
              GPCP_precip_mm_deviation_sd_sq+GPCP_precip_mm_deviation_sd_l+
              GPCP_precip_mm_deviation_sd_l_sq+polity2+polity2_sq+log_pop_pwt+
              log_pop_pwt_fd+log_rgdpch_pwt+grgdpch_pwt+incidence+ttrend+
              factor(year)+factor(ccode),test);summary(m7)

## Government-targeted events
m8<-glm.nb(gov_targeted_events_no_onset~gov_targeted_events_no_onset_l+
             GPCP_precip_mm_deviation_sd+GPCP_precip_mm_deviation_sd_sq+
             GPCP_precip_mm_deviation_sd_l+GPCP_precip_mm_deviation_sd_l_sq+
             polity2+polity2_sq+log_pop_pwt+log_pop_pwt_fd+log_rgdpch_pwt+
             grgdpch_pwt+incidence+ttrend+factor(year),test);clse(m8,1,test$ccode)

m9<-glm.nb(gov_targeted_events_no_onset~gov_targeted_events_no_onset_l+
             GPCP_precip_mm_deviation_sd+GPCP_precip_mm_deviation_sd_sq+
             GPCP_precip_mm_deviation_sd_l+GPCP_precip_mm_deviation_sd_l_sq+
             polity2+polity2_sq+log_pop_pwt+log_pop_pwt_fd+log_rgdpch_pwt+
             grgdpch_pwt+incidence+ttrend+factor(year)+factor(ccode),test);summary(m9)

## Non-government-targeted events
m10<-glm.nb(nongov_targeted_events~nongov_targeted_events_l+
             GPCP_precip_mm_deviation_sd+GPCP_precip_mm_deviation_sd_sq+
             GPCP_precip_mm_deviation_sd_l+GPCP_precip_mm_deviation_sd_l_sq+
             polity2+polity2_sq+log_pop_pwt+log_pop_pwt_fd+log_rgdpch_pwt+
             grgdpch_pwt+incidence+ttrend+factor(year),test);clse(m10,1,test$ccode)

m11<-glm.nb(nongov_targeted_events~nongov_targeted_events_l+
             GPCP_precip_mm_deviation_sd+GPCP_precip_mm_deviation_sd_sq+
             GPCP_precip_mm_deviation_sd_l+GPCP_precip_mm_deviation_sd_l_sq+
             polity2+polity2_sq+log_pop_pwt+log_pop_pwt_fd+log_rgdpch_pwt+
             grgdpch_pwt+incidence+ttrend+factor(year)+factor(ccode),test);summary(m11)