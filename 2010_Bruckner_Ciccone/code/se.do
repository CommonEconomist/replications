* Replication test standard errors

* Generate country indicators and country-specific time trend
tab ccode, gen(Iccode)

local i=1
while (`i'< 40){
quietly gen Iccyear`i' = Iccode`i'*year
label variable Iccyear`i' "Country-Specific Time Trend for Iccode`i'"
local i = `i' + 1
}

* Generate year indicators

tab year, gen(time)


* Effect of small sample adjustments on standard errors

* Model as reported
ivreg2 war_prio_on index_g index_g_l index_g_l2 Iccode* Iccyear* time*, cluster(ccode) partial(Iccode* Iccyear* time*)

* Using small sample adjustment
ivreg2 war_prio_on index_g index_g_l index_g_l2 Iccode* Iccyear* time*, cluster(ccode) small partial(Iccode* Iccyear* time*)

* Compare with normal reg function
reg war_prio_on index_g index_g_l index_g_l2 Iccode* Iccyear* time*,robust cluster(ccode)

* This gives identical results to those obtained in R

* Another example:
ivreg2 war_prio_on ind Iccode* Iccyear* time*, cluster(ccode) small partial(Iccode* Iccyear* time*)
reg war_prio_on ind Iccode* Iccyear* time*,robust cluster(ccode)
