Using Power Laws to Estimate Conflict Size
==============

**Replication status:** *Successful*

Estimating war severity is a difficult task and sometime fraught with controversy. 
One particular challenging issue in estimating the number of fatalities is that the data record is often incomplete. 
Friedman ([2014](http://journals.sagepub.com/doi/abs/10.1177/0022002714530430)) offers a potential solution for dealing with incomplete data using the empirical regularities in conflict events sizes to infer missing conflict events. 
A number of studies have shown that violent events tend to follow a power-law distribution ([1](https://www.nature.com/articles/srep03463),[2](http://www.nature.com/nature/journal/v462/n7275/full/nature08631.html),[3](http://journals.sagepub.com/doi/abs/10.1177/0022002706296157),[4](https://doi.org/10.1371/journal.pone.0048633)) which describes the frequency and severity of events. 
Friedman uses this relationship to draw inferences about what the true distribution of events might look like, using incomplete data.<br>

Using the number of Native American and US fatalities in the American Indian Wars (1776-1890) as an example. 
The `R` code here is a "translation" of his original code, which I think was written for Matlab. 
To estimate the expected number of events of size X one needs the Hurwitz zeta function, since the data series is discrete. 
In `R` there is only seems to be the Riemann zeta function, so I coded the Hurwitz zeta function myself. 
Note that I have no idea on how to actually sum the entire infinite series, so the function used is only an approximation. 

Using my code I am able to replicate the original results although some small discrepancies exist. 
Nonetheless, all estimates are within the same order of magnitude and very close to the reported values. 
Using the replication data I did spot some differences in the reported number of fatalities in the table in the paper and in the dataset. Again, these differences are very small, likely the result of typos.<br>

**Update**
Although using power laws to estimate conflict size seems like an elegant an innovative solution to an important problem, there are some caveats associated with this approach illustrated here using data from the war in Iraq. 

This short example relies on two different data sources:
* Georeferenced Event Dataset (GED), provided by [UCDP](http://ucdp.uu.se)
* SIGACTS, which is data recorded by the US military

The GED is probably the most commonly used conflict event dataset. 
The information on the location and severity of events relies mainly on media reports, although it is supplemented with information from case studies and reports from non-governmental organisations amongst others. 
In contrast, SIGACTS is recorder by the US military and a such provides a rather unique dataset which is arguably a better representation of the true number of events. 
For instance, a shortcoming of the GED data is that events are only included if the actors perpetrating the violence can be identified, which probably leads to under reporting. 
Indeed, comparing the number of events for the years 2004-2009, the period covered by the SIGACTS data, the GED reports 2369 events compared to 52305 for SIGACTS.
Assuming that SIGACTS indeed provides a more comprehensive data records, we can use the GED to interpolate the number of fatalities and compare this with SIGACTS data to see if the method works. 
Where SIGACTS reports 109302 fatalities, GED only reports 16638, a discrepancy that is likely the result of the coding procedure. 
However, we can fit a power law to the GED data and interpolate the missing events. 
Doing this shows that the power law has an alpha parameter of 2.39 and Xmin of 11, i.e. the power law holds from event size 11 onwards. 
Following Friedman the power-law is used to predict the number of events below event size 11 which results in 28123 additional fatalities, totalling 39091 with the already reported fatalities. 
This number is a far cry from the fatalities reported in the SIGACTS data. 
Additionally, checking the fit of the model shows a p-value of 0.02 for the GED data, and 0 for the SIGACTS data, which entails that a power-law might not fit the data well. 

There are some other issues as well which I still need to analyse such as the sensitivity of the results to the estimated alpha parameter. 
Although research has shown that violence tends to exhibit empirical regularities concerning the frequency of event sizes, there might be limits to what extend we can use this relation to make inferences about conflict severity which still largely pertains to data quality. 
