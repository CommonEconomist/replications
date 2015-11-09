tab ccode, gen(Iccode)

local i=1
while (`i'< 40){
quietly gen Iccyear`i' = Iccode`i'*year
label variable Iccyear`i' "Country-Specific Time Trend for Iccode`i'"
local i = `i' + 1
}

**generate time fixed effects**

tab year, gen(time)

*Table 1
sum any_prio any_prio_on any_prio_off war_prio war_prio_on war_prio_off ind oecd_export gdp_g if ind!=.

*Table 2
 
ivreg2 war_prio_on index_g index_g_l index_g_l2 Iccode* Iccyear* time*, cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 war_prio_on gpcp_g gpcp_g_l gpcp_g_l2 Iccode* Iccyear* time* if ind!=., cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 war_prio_on index_g index_g_l index_g_l2 gpcp_g gpcp_g_l gpcp_g_l2 Iccode* Iccyear* time* if ind!=., cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 war_prio_on index_g index_g_l index_g_l2 lgpcp_l lgpcp_l2  lgpcp_l3 Iccode* Iccyear* time* if ind!=., cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 war_prio_on ind Iccode* Iccyear* time*, cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 war_prio_on ind rain3 Iccode* Iccyear* time*, cluster(ccode) partial(Iccode* Iccyear* time*)

*Table 3
dprobit war_prio_on ind
logit war_prio_on ind 
xtlogit war_prio_on ind  , fe
xtlogit war_prio_on ind Iccyear* time* , fe

*Table 4
ivreg2 gdp_g ind  Iccode* Iccyear* time*  if war_prio_on!=. , cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 war_prio_on gdp_g Iccode* Iccyear* time* if ind!=., cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 war_prio_on Iccode* Iccyear* time* (gdp_g=ind), cluster(ccode) partial(Iccode* Iccyear* time*) endogtest(gdp_g) ffirst 

*Table 5
ivreg2 gdp_g ind oecd_export Iccode* Iccyear* time* if war_prio_on!=., cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 war_prio_on ind oecd_export Iccode* Iccyear* time* , cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 war_prio_on Iccode* Iccyear* time* (gdp_g=ind oecd_export), cluster(ccode) partial(Iccode* Iccyear* time*) ffirst 


*Table 6
ivreg2 gdp_g ind oecd_export Iccode* Iccyear* time* if any_prio_on!=., cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 any_prio_on ind oecd_export Iccode* Iccyear* time* if any_prio_on!=., cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 any_prio_on gdp_g Iccode* Iccyear* time* if ind!=., cluster(ccode) partial(Iccode* Iccyear* time*)
ivreg2 any_prio_on Iccode* Iccyear* time* (gdp_g=ind oecd_export), cluster(ccode) partial(Iccode* Iccyear* time*) ffirst 

