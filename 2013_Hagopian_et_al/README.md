Mortality in Iraq
==============

These are the replication files for the study by Hagopian et al. (2013) which was pubslihed in [PLOS Medicine](http://www.plosmedicine.org/article/info%3Adoi%2F10.1371%2Fjournal.pmed.1001533).
The study attempts to estimate the number of excess deaths in Iraq between 2003-2011 as a result of the U.S. invasion. 
Based on survey data from a 2,000 household cluster-survey the authors found for the period between March 2003 and June 2011, Iraq had a crude death rate of 4.55 per 1,000 person-years with a 95% interval of 3.74–5.27. 
This corresponds with a central estimate of 405,000 excess deaths (95% interval 48,000–751,000).

As part of a study on war and mortality , together with M. Spagat, I have tried to replicate their results using R. 
The replication comes close to the original results although not exactly. 
I find a central estimate for the crude death rate of 4.53 per 1,000 person-years which corresponds with about 406,000 excess deaths. 
The 95% interval is slightly different ranging from 55,000 to 690,000. 
This is likely due to random differences in the randomisation process in R compared to their estimation in Python.

This repository contains the original data and a replication script (`replication.R`) for generating the results similar to those in the paper. 

The original data is stored in the following files:

* `hh_deaths.csv` contains the mortality data
* `hh_roster.csv` contains the survey data
* `pop.csv` contains population estimates for Iraq

