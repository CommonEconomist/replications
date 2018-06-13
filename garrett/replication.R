## Replication "Partisan Politics in the Global Economy"
# Original work by Garrett (1998), replication focuses
# on table from Beck (2004)
# http://pages.ucsd.edu/~tkousser/Beck%20Notes/longitude20041short.pdf
# Original book: http://tinyurl.com/z94mgvf
load("garrett1998.RData")

## 1) Adjust country names 
require(countrycode)
x$country_n<-countrycode(x$country,"cown","country.name",warn=TRUE)
x$country_n<-ifelse(x$country_n=="Federal Republic of Germany","Germany",
                    x$country_n)

## 2) Fit model 
m1<-lm(gdp~gdpl+oild+demand+leftlab*corp,data=x)
summary(m1)
mean(abs(fitted(m1)-x$gdp))

## 3) Prepare data for cross-validation
country<-sort(unique(x$country_n))
J<-length(country)
error<-c()

## 4) Fit models

for (i in 1:J){
  df<-x[x$country_n!=country[i],]  # Exclude one country at a time
  ous<-x[x$country_n==country[i],] # Data for out-of-sample testing
  
  m<-lm(gdp~gdpl+oild+demand+leftlab*corp,data=df) # Fit model 
  yhat<-predict(m,ous) # Generate predictions
  
  error[i]=mean(abs(yhat-ous$gdp)) # Get prediction error
}

## 5) Plot results
fig<-data.frame(country=country,mae=error,
                reported=c(1.3,1.6,1.7,1.7,
                           2.0,1.2,1.4,1.7,
                           3.2,1.6,1.5,1.2,
                           1.7,1.9))
fig<-fig[order(fig$mae),]
fig$country<-factor(fig$country,levels=fig$country)

## Figure
par(mar=c(4.5,6.2,1,0),pty="s",las=1,cex.axis=1.5,cex.lab=1.5)
plot(fig$mae,fig$country,xlab="Mean absolute error",ylab="",main="",axes=FALSE,
     pch=19,cex=1.5,xlim=c(0,3.25))
segments(rep(0,nrow(fig)),1:nrow(fig),fig$reported,lend=2,lty=2,lwd=.75)
points(fig$reported,fig$country,pch=0,cex=1.5)
axis(1,tick=FALSE)
axis(2,at=1:nrow(fig),label=fig$country,tick=FALSE,las=1)

legend(1.8,3,c("Reported","Estimated"),cex=1.5,pch=c(0,19),
       bty="n",y.intersp=c(0.3))
