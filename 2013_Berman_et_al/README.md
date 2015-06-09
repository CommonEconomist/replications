Violence-reducing effects of development spending in Iraq
==============

The study by Berman et al. (2013) published in the [AER](https://www.aeaweb.org/articles.php?doi=10.1257/aer.103.3.512) was one of the 
first studies that looked at the effect of foreign aid on conflict at the sub-national level.
Using panel data on development spending at the district level for Iraq, the study showed the potential 
violence-reducing effects of development assistance are larger when projects are small and combined with high troop strenght
and professional development expertise (such as in a Provincial Reconstruction Team). 

Initially interested in the possible effect of spatial spillover effects on the effectiveness of aid (I found none), I 
attempted to replicate their results. 
The original analysis was done in Stata and I "translated" the data preparation and regressions into R. 
For most table I am able to replicate their results save some very minor discrepancies. 
Only for table 3, which looks at the interaction between aid money and the availability of a PRT, I haveb't been able
to replicate the results. 

Included are the following files:

* `BermanetalAER2013replication.dta` is the original dataset
* `clse.R` is the code to calculate robust standard errors
* `replication.R` replicates the results, at least some of them

