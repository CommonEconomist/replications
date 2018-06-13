## Montgomery, Hollendbach, and Ward
## Replication files necessary for replicating
## Table 3,  Footnote 21, Table 4, and  Figure 3
## Last edited: Jacob M. Montgomery
## Date: 12-19-2011

library(foreign)
library(EBMAforecast) #This should be available on CRAN
library(ICEWS2) #This is necessary for the predict.lmer() function.
#This function shoudld be included in the EBMAforecast package when it is distributed
library(separationplot)

# You will want to re-set the working directory
setwd("/Users/jmontgomery/Dropbox/EBMA/PRESIDENTIAL DATA for Jacob/Final submission")

# Read in icews data
icews<-read.csv("icews.csv")

#Put together the SAE dataset and make identifiers consistent across datasets
for.SAE <- data.frame(as.character(icews$country), icews$year, icews$month)
names(for.SAE) <- c("Country", "year", "month")
SAE.data <- read.dta("SAEdata.dta")
SAE.data$Country<-toupper(SAE.data$country)
for.SAE$Country<-toupper(for.SAE$Country)

# Replace some country names to merge ICEWS and SAE data
my.replace.fun<-function(BAD, GOOD){
  SAE.data$Country[SAE.data$Country ==BAD]<<-GOOD
  for.SAE$Country[for.SAE$Country ==BAD]<<-GOOD
 }
my.replace.fun("NEW_ZEALAND", "NEW ZEALAND")
my.replace.fun("NORTH_KOREA", "NORTH KOREA")
my.replace.fun("PAPUA_NEW_GUINEA", "PAPUA NEW GUINEA")
my.replace.fun("SOLOMIN_ISLANDS", "SOLOMIN ISLANDS")
my.replace.fun("SOLOMIN IS.", "SOLOMIN ISLANDS")
my.replace.fun("SOUTH_KOREA", "SOUTH KOREA")
my.replace.fun("SRI_LANKA", "SRI LANKA")
my.replace.fun("BURMA", "MYANMAR")
my.replace.fun("SOLOMON_ISLANDS", "SOLOMIN ISLANDS")
my.replace.fun("SOLOMON IS.", "SOLOMIN ISLANDS")
SAE.data <- merge(SAE.data, for.SAE, by=c("Country", "year", "month"))

# Make the three sets for the SAE and non-SAE models
SAE.training.sample<-SAE.data[ SAE.data$year>=1999 & SAE.data$year<=2007,] 
SAE.valid.sample<-SAE.data[SAE.data$year>=2008 & SAE.data$year<=2009,] 
SAE.test.sample<-SAE.data[SAE.data$year>=2010 & SAE.data$year<=2010,] 

training.sample <- icews[icews$year>=1999 & icews$year<=2007,]  # note that this includes 1998 unlike SAE
validation.sample <- icews[icews$year>=2008 & icews$year<=2009,] 
test.sample <- icews[icews$year>=2010 & icews$year<=2010,] 
 
# The "GLM" model used in the paper
model.glm.simple<-glm(insurgency~  gdpgrowth.l3 +pop.l3 + nminorities + anoc , family="binomial",data=training.sample)
summary(model.glm.simple)
training.glm <- fitted(model.glm.simple) # Fitted values for training period
validation.glm <- predict(model.glm.simple, type="response", newdata=validation.sample) #Predictions for validation period
test.glm <- predict(model.glm.simple, type="response", newdata=test.sample) #Predictions for test period

# Repeat precess for the lmer model
model.lmer<-lmer(insurgency~recentonsets.insurgency + lastelection +mil.conf.l3 + W.bl.std.crisisdomestic.l3 + (gdppc.l3 + USAconf.l3|country), 
                      family="binomial", data=training.sample)
summary(model.lmer)
training.lmer <- fitted(model.lmer)
validation.lmer <- predict.lmer(model.lmer, newdata=validation.sample)
test.lmer <- predict.lmer(model.lmer, newdata=test.sample)

###### SAE model
SAE.model <- glm(GTDSinsurgency ~ L6muslimsScooptotals + L6allmuslimsSviolence + L6Sallmuslimsviolence +
           L6separatistsAllhosttotals + L6JIAllhosttotals + L6GDhosttotals + L6Ginsurgentshosttotals +
           L6Ginsurgentshosttotals2 + L6intrapolpartycooptotals + L6intrapolpartycooptotals2 +
           L6orgmuslimsAllhosttotals + L6orgmuslimsAllhosttotals2 + L6insurgentsAllhosttotals +
           L6insurgentsAllhosttotals2 + L12margroups + L12state + L12state2 + L12ethnfrac + L12ethnfracstate +
           L12ethnfracstate2 + L12imp_GDPcurUS + L12imp_GDPcurUS2 + L12lnpop + L12imp_relfrac + L12imp_relfrac2 +
           L12imp_relfracstate + L12imp_relfracstate2,
           data = SAE.training.sample, family=binomial(link="logit"), na.action="na.exclude")
summary(SAE.model)
training.SAE <- fitted(SAE.model)
valid.SAE <- predict(SAE.model, newdata = SAE.valid.sample, type="response")
test.SAE <- predict(SAE.model, newdata = SAE.test.sample, type="response")


# Make dataset of lmer and glm validation/test period predictions
Duke.in.pred <- data.frame(validation.glm, validation.lmer,
                           as.character(validation.sample$country), validation.sample$year, validation.sample$month,
                           validation.sample$insurgency, stringsAsFactors=FALSE)
names(Duke.in.pred)<-c("glm", "lmer",  "Country", "Year", "Month", "Insurgency")
Duke.out.pred <- data.frame(test.glm, test.lmer, as.character(test.sample$country), test.sample$year, test.sample$month, test.sample$insurgency, stringsAsFactors=FALSE)
names(Duke.out.pred)<-c("glm", "lmer", "Country", "Year", "Month", "Insurgency")


# Make SAE data frames
SAE.in.pred <- data.frame(valid.SAE, SAE.valid.sample$country, SAE.valid.sample$year, SAE.valid.sample$month, stringsAsFactors=FALSE)
names(SAE.in.pred) <- c("SAE", "Country", "Year", "Month")
SAE.out.pred <- data.frame(test.SAE, SAE.test.sample$country, SAE.test.sample$year, SAE.test.sample$month, stringsAsFactors=FALSE)
names(SAE.out.pred) <- c("SAE", "Country", "Year", "Month")


# Merge them together
SAE.in.pred$Country<-toupper(SAE.in.pred$Country)
Duke.in.pred$Country<-toupper(Duke.in.pred$Country)
SAE.out.pred$Country<-toupper(SAE.out.pred$Country)
Duke.out.pred$Country<-toupper(Duke.out.pred$Country)
my.replace.fun<-function(BAD, GOOD){
  SAE.in.pred$Country[SAE.in.pred$Country ==BAD]<<-GOOD
  SAE.out.pred$Country[SAE.out.pred$Country ==BAD]<<-GOOD
  Duke.in.pred$Country[Duke.in.pred$Country ==BAD]<<-GOOD
  Duke.out.pred$Country[Duke.out.pred$Country ==BAD]<<-GOOD
 }
my.replace.fun("NEW_ZEALAND", "NEW ZEALAND")
my.replace.fun("NORTH_KOREA", "NORTH KOREA")
my.replace.fun("PAPUA_NEW_GUINEA", "PAPUA NEW GUINEA")
my.replace.fun("SOLOMIN_ISLANDS", "SOLOMIN ISLANDS")
my.replace.fun("SOLOMIN IS.", "SOLOMIN ISLANDS")
my.replace.fun("SOUTH_KOREA", "SOUTH KOREA")
my.replace.fun("SRI_LANKA", "SRI LANKA")
my.replace.fun("BURMA", "MYANMAR")
my.replace.fun("SOLOMON_ISLANDS", "SOLOMIN ISLANDS")
my.replace.fun("SOLOMON IS.", "SOLOMIN ISLANDS")
Insample <- merge(Duke.in.pred, SAE.in.pred, by=c("Country", "Year", "Month"))
Outsample <- merge(Duke.out.pred, SAE.out.pred, by=c("Country", "Year", "Month"))

# A mini-dataset of just the validation period predictions and observed outcomes
my.pred <- cbind(Insample$lmer, Insample$SAE , Insample$glm)
colnames(my.pred) <- c("LMER", "SAE" , "GLM")
my.y <- as.vector(cbind(Insample$Insurgency))

# Fit the EBMA model using the validation period
EBMA.fit <- Ensemble.logit(y=my.y, pp.raw=my.pred, tol=.0001, exp=3, max.iter=25000)

# Table 1
print.Ensemble.logit(EBMA.fit)

# Plot 1
plot.Ensemble.logit(EBMA.fit)

# A mini-dataset for the test-period predictions and observed outcomes
my.pred.out <- cbind(Outsample$lmer, Outsample$SAE , Outsample$glm)
my.y.out <- as.vector(cbind(Outsample$Insurgency))

# Make forecasts for the test-sample periods
EBMA.pred <- predict.Ensemble.logit(obj=EBMA.fit, newdata=my.pred.out, y.out=my.y.out)


# Table 2
print.Ensemble.logit(EBMA.pred)

# Plot 2
plot.Ensemble.logit(EBMA.pred)

#Percent that are "no insurvency" in validaation and test set
table(my.y)/sum(table(my.y))
table(my.y.out)/sum(table(my.y.out))

# Footnote 14: Comparing average with EBMA

average <- rowMeans(my.pred)
my.pred.av <-  cbind(my.pred, average)
colnames(my.pred.av) <- c("LMER", "SAE", "GLM", "AVE")
EBMA.fit.av <- Ensemble.logit(y=my.y, pp.raw=my.pred.av, tol=.001, exp=3)
print.Ensemble.logit(EBMA.fit.av)

# Now for test-period: This is only done because the function calculates AUC, PRE, etc.  We are *not* using these EBMA outputs.
my.pred.out.av <- cbind(my.pred.out, rowMeans(my.pred.out))
EBMA.pred <- predict.Ensemble.logit(obj=EBMA.fit.av, newdata=my.pred.out.av, y.out=my.y.out)
print.Ensemble.logit(EBMA.fit.av)

