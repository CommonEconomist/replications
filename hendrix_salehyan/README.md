Climate change, rainfall, and social conflict in Africa
==============

**Replication status:** *Partially succesful*

The study by Hendrix & Salehyan ([2012](http://jpr.sagepub.com/content/49/1/35.abstract)) was published in a special issue of the JPR on climate change and conflict. Their study is one of the first that looks at a broader definition of conflict, to include riots, strikes, and protests next to violent armed conflict. 
They claim to find a robust link between weather variation and unrest: extreme deviations in rainfall are associated with all types of social unrest but particularly with violent ones. 

Using the revised replication data ([background](http://www.fight-entropy.com/2012/02/carefully-interpreting-climate-conflict.html)) I'm able to replicate the results save for the conditional fixed effects models. 
This issue cropped up in earlier replication attempts and is one which I haven't been able to solve yet. 
An additional concern with regard to the fixed effects model is that they inlcude both fixed effects and the lagged outcome variable. 
For estimation methods such as OLS and Logit the induced bias is small when the number of repeated observations is sufficiently large. However, for negative binomial model, such as this one, there is a risk that the estimates are inconsistent as discussed [here](http://www.jstor.org/stable/2235494) and [here](http://www.sciencedirect.com/science/article/pii/S0304407601001087).

The following files are included:
* `H_S_JPR_491_Replication_Revised.dta`, the revised replication data
* `replication.R`, the replication code for in R
