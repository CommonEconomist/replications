International Commodity Prices, Growth and the Outbreak of Civil War in Sub-Saharan Africa
==============

**Replication status:** *Successful*

In a 2010 article in the [Economic Journal](http://onlinelibrary.wiley.com/doi/10.1111/j.1468-0297.2010.02353.x/abstract), Brückner and Ciccone examine the link between economic performance and conflict, focussing on the effect of international commodity prices on the outbreak of civil war. 
Using data for 39 countries in Sub-Sahara Africa between 1981-2006, they find that there is a higher probability of civil war onset following downturns in the international prices of the countries’ main export commodities. 
This relation holds across a number of different model specifications and estimations methods. 
Their results also show that the outbreak of civil war is more likely following recessions in the main OECD export destinations. 

I tried to reproduce their results in `R` carrying it out a direct replication as well as updating and extending the data to see whether their findings hold for more recent data and can be generalised. 
For the direct replication I found that most results can be reproduced although there are differences in the values of the t-values. 
These differences are caused by a small-sample adjustment in the calculations of the robust clustered standard errors.
I wasn't able to replicate the logit model including country and year fixed effects and country-specific time trends. 
Updating the data and extending data to cover the period 1981-2013 I did not find a statistically significant link between international commodity prices and conflict in Sub-Sahara Africa, as shown in the figure below. 

![](http://i.imgur.com/6bnDoBn.png)

The following folders and files are included for the replication:

`raw_data` contains all the original data

* `data.dta` is the original dataset
* `imf.csv` IMF international commodity prices time series (covers msot of the commodities)
* `gem.csv` Global Economic Monitor international commodity prices time series (covers gold, phosphates, and tobacco)
* `ucdpConflict.rdata` is version 4-2014a of the UCDP/PRIO Armed Conflict Dataset
* `DOTS.csv` contains the direction of trade statistics for exports from Sub-Saharan African countries to OECD-member states

`code` contains all the `R`-scripts needed to run the replication. This includes creating the new data
* `functions.R` code to calculate robust standard errors and mean squared error
* `narrow_replication.R` direct replciation of the results. Uses `data.dta`
* `scientific_replication.R` re-estimates the model using newer data and extending the time period. This requires `new_data.R` to be run first. 
* `new_data.R` creates the dataset for the scientific replication. Creates `newData.Rdata` which can be found in `tidy_data`





