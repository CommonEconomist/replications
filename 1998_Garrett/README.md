Partisan Politics in the Global Economy
==============

**Replication status:** *Partially successful*

This is a bit of a deviation from the standard replications in this repository as it focuses not so much on the original work by Garrett [(1998)](http://tinyurl.com/z94mgvf) but on a small re-analysis of the work by Beck (2004) in some lecture slides that are available [online](http://pages.ucsd.edu/~tkousser/Beck%20Notes/longitude20041short.pdf).
In table 1 of the linked document, Beck shows the results of a cross-validation exercise where the fit of a pooled model is analysed based on the mean absolute error leaving out one country at a time from the Garrett data. 
I tried to replicate this table mainly as an exercise to efficiently code the cross-validation method, which is fairly straightforward to do. I wasn't able to exactly replicate the original results (see figure), for reasons yet unknown. 

![](http://i.imgur.com/XklkCnj.png)


The following files are included:

* `garrett1998.RData` is the dataset
* `replication.R` is the replication script. 






