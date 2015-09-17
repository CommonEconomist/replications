Rising Food Prices, Food Price Volatility, and Social Unrest
==============

**Replication status:** *Successful*

In the past year or so, a number of papers have been published focussing on the link between food prices and unrest. 
One of the earlier works, of which a working papers has been in circulation since 2012, is the study by Bellemare (2015) published in the [AJAE](http://ajae.oxfordjournals.org/content/97/1/1). In contrast with other work, the author focuses on the link between food prices and social unrest at the global level. Using monthly data on food prices and food related social unrest the author finds that betwee 1990-2011 food price increases have led to increases in social unrest. 
Due to the use of global data there is the risk that food prices and social unrest are jointly determined and therefore the author uses the count of natural disasters in a month to instrument for food prices. 

Having worked on [this topic](http://ssrn.com/abstract=2418973) I thought that both the use of global data and the chosen instrument where a bit peculiar. Over the past two decades, food prices have had a very similar trend to other commodity prices as shown in the figure below. Does that entail that we can instrument for instance oil and metal prices as well with natural disasters? The extension to this replication seem to suggest we can as both the first and second stage results are very similar to those reported in the paper. Unless I am overlooking something here.  

![](http://i.imgur.com/SqU7kKX.png)
![](http://i.imgur.com/IWuvfgN.png)


The replication focuses on the regression analysis presented in table 2 and 3 of the paper. 
The original regressions were done in Stata, I wrote the code to do it in R (`re-analysis.R`).



