Using Power Laws to Estimate Conflict Size
==============

**Replication status:** *Successful*

Friedman [(2014)](http://jcr.sagepub.com/content/59/7/1216) illustrates how power laws can be used to estimate conflict size,
using the number of Native American and US fatalities in the American Indian Wars (1776-1890) as an example. 
The `R` code here is a translation of his original code. Note that the Hurwitz Zeta function is an approximation as it doesn't sum to infinity due to computational limitations. 
Using the code I am able to replicate the original results although some small discrepancies exist. 
One of which is a difference in the reported number of fatalities in the paper which is incorrect. 

The following files are included:

* `American Indian Wars event data.dta` is the original dataset
* `replication.R` is the replication script. 






