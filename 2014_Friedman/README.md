Using Power Laws to Estimate Conflict Size
==============

**Replication status:** *Successful*

Friedman [(2014)](http://jcr.sagepub.com/content/59/7/1216) illustrates how power laws can be used to estimate conflict size (see also the `2011_Johnson_et_al` folder),
using the number of Native American and US fatalities in the American Indian Wars (1776-1890) as an example. 
The `R` code here is a "translation" of his original code, which I think was written for Matlab. 
To estimate the expected number of events of size X one needs the Hurwitz zeta functtion, since the data series is discrete. 
In `R` there is only seems to be the Riemaan zeta function, so I coded the Hurwitz zeta function myself. 
Note that I have no idea on how to actually sum the entire infinite series, so the function used is only an approximation. 

Using my code I am able to replicate the original results although some small discrepancies exist. 
Nonetheless, all estimates are within the same order of magnitude and very close to the reported values. 
Using the replication data I did spot some differences in the reported number of fatalities in the table in the paper and in the dataset. Again, these differences are very small, likely the result of typos. 

The following files are included:

* `American Indian Wars event data.dta` is the original dataset
* `replication.R` is the replication script. 






