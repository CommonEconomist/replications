The Legacy of Historical Conflict: Evidence from Africa
==============

**Replication status:** *Successful*

In 2013 I attended a talk at the Royal Economic Society annucal conference in the political economy of African development session.
The presentation was about a paper by Besley and Reynal-Querol on the historical legacy of conflict in Africa which was published in 2013 in the [ASPR](http://dx.doi.org/10.1017/S0003055414000161). 
The authors use the dataset from [Brecke](http://www.cgeh.nl/data) on historical conflict to examine the link between 
historical conflict prior to colonisation between 1400-1700 and, amongst others, current levels of conflict. 
They find a correlation between historical conflict and current conflict both between and within countries. 
This study fits within a recent trend in economics where historical data is used to explain current outcomes, and although I found the approach very intersting, I had some healthy skepticism concerning the results. 
These concerns were mainly based on the data they used. 
At the country-level they used an outdated dataset on conflict which meant that they only cover the period from decolonisation until 2007 I believe.
This is not entirely clear from the article. 
For the within-country estimation they use data from 1997 to 2010 but from a dataset that, although it is widely used, does not have the best [reputation for accuracy](http://cac.sagepub.com/content/47/1/124.abstract) in its geoprecision.
I therefore decided to re-analyse their result using better data. 

Additionally, I had some concerns with regard to the argument that historical conflict correlates with post-colonial conflict but that they ignore the levels of violence experienced during decolonisation. 
Surely these more recent historical conflicts should have a larger effect?
Much to my surprise I actually found that using more recent data and adding these controls, that the effect becomes stronger. 
And it is not just a statistically significant results, the historical conflict variable also carries some predictive power compared to the other variables in the model as shown in the figure below for the between-country estimation.
Although one can wonder why 30 variables are needed in a dataset with just 48 observations of course. 
I might want to extend some of the test concerning the predictions, specifically for the within-country estimation, but that is probably for future work. 

![](http://i.imgur.com/Vpy7eKk.png)

Since this replication also includes an extension, there are some more files involved.
The `raw_data` folder contains all the original data:

* `table2.dta` is the original data needed to replicate the results in table 2 (Quelle surprise)
* `table5.data` is the original data needed to replciate the results in table 5
* `acd2013.csv` is an updated version of the UCDP/PRIO dataset on conflicts
* `ucdp-ged15.csv` is the UCDP Georeferenced Event Dataset
* `p4v2012.csv` is the Polity IV dataset needed for the years of independence

The `code` folder contains all the R-scripts necessary for the replication and the re-analysis

* `conflict_data.R` prepares the most recent conflict data at the country level and creates `conflict_data.Rdata` in `tidy_data`
* `between_country.R` replicates the results for table 2 focused on conflict
* `within-country.R` replicates the results for table 5 focused on conflict
* `fig_auc.R` creates the figure








