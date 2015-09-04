Warming increases the risk of civil war in Africa
==============

**Replication status:** *Successful*

Within the academic literature on conflict there is a heated debate on the link between climate and conflict. 
One of the seminal works within the literature is the study by Burke et al. (2009) published in [PNAS](http://www.pnas.org/content/106/49/20670)
which has been cited 385 times (according to Google Scholar April 2015). 
The paper looks at the effect of climate change on civil war focussing on Sub-Sahara Africa and find that between
1981-2002 there is a strong relation where a  1°C increase in the average annual temperature corresponds with a 4.5% increase in the probability of civil war in the current year, and an additional 0.9% in the following year. 
There is still an ongoing debate on the validity of these results and this replication was part of a paper I have 
[written](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2550228) which focuses on the general *modus operandi* in the conflict literature.
I think that in general my skepticism concerning the claims made in the paper are visually summarised by the following figure which I made using the data:
![](http://i.imgur.com/xd0XpFw.png)


The replication focuses on the regression analysis presented in table 1 of their paper, and I included the code for the figure above. 
The original regressions were done in Stata, I wrote the code to do it in R.

The following files are needed for replication:

* `climate_conflict.dta` is the original dataset
* `clse.R` is the code to calculate robust standard errors
* `replication.R` replicates the results

