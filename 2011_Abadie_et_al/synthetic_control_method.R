# Synthetic Control Method
# Application in R following: https://www.jstatsoft.org/article/view/v042i13
# Replicating figures

# Load data
library("Synth")
data("basque")


#### Fit model for Basque country
X<-dataprep(foo=basque,
            predictors= c("school.illit","school.prim",
                          "school.med", "school.high",
                          "school.post.high","invest"),
                        predictors.op=c("mean"),
                        dependent=c("gdpcap"),
                        unit.variable=c("regionno"),
                        time.variable = c("year"),
                        special.predictors = list(
                          list("gdpcap",1960:1969,c("mean")),
                          list("sec.agriculture",seq(1961,1969,2),c("mean")),
                          list("sec.energy",seq(1961,1969,2),c("mean")),
                          list("sec.industry",seq(1961,1969,2),c("mean")),
                          list("sec.construction",seq(1961,1969,2),c("mean")),
                          list("sec.services.venta",seq(1961,1969,2),c("mean")),
                          list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
                          list("popdens",1969,c("mean"))),
                        treatment.identifier=17,
                        controls.identifier=c(2:16,18),
                        time.predictors.prior=c(1964:1969),
                        time.optimize.ssr=c(1960:1969),
                        unit.names.variable=c("regionname"),
                        time.plot=c(1955:1997)) 

# Find synthetic control (can take about half a minute)                          
s<-synth(data.prep.obj=X,method="BFGS") 

# Identified gap; save as time-series
basque.ts<-ts(X$Y1plot-(X$Y0plot %*% s$solution.w),start=c(1955,1))

# Plot figure 1
path.plot(synth.res=s,dataprep.res=X,
          Ylab="Real per-capita GDP (1986 USD, thousand)",
          Xlab="year",Ylim=c(0,12),
          Legend=c("Basque country","synthetic Basque country"),
          Legend.position="bottomright")


# Plot figure 2
gaps.plot(synth.res=s,dataprep.res=X,
          Ylab="Gap in real per-capita GDP (1986 USD, thousand)",
          Xlab="year",Ylim=c(-1.5,1.5),Main=NA)


#### Placebo test with Catalunia ####
X2<-dataprep(foo=basque,
             predictors=c("school.illit","school.prim","school.med",
                          "school.high","school.post.high","invest"),
             predictors.op=c("mean"),dependent=c("gdpcap"),
             unit.variable=c("regionno"),
             time.variable = c("year"),
             special.predictors = list(
               list("gdpcap",1960:1969,c("mean")),
               list("sec.agriculture",seq(1961,1969,2),c("mean")),
               list("sec.energy",seq(1961,1969,2),c("mean")),
               list("sec.industry",seq(1961,1969,2),c("mean")),
               list("sec.construction",seq(1961,1969,2),c("mean")),
               list("sec.services.venta",seq(1961,1969,2),c("mean")),
               list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
               list("popdens",1969,c("mean"))),
             treatment.identifier=10,
             controls.identifier=c(2:9,11:16,18),
             time.predictors.prior=c(1964:1969),
             time.optimize.ssr=c(1960:1969),
             unit.names.variable=c("regionname"),
             time.plot=c(1955:1997)) 

s2<-synth(data.prep.obj=X2,method="BFGS")
# This throws an error code that needs looking into

# Plot figure 3
# NB - There is a slight difference compared to the figure in the paper
path.plot(synth.res=s2,dataprep.res=X2,
          Ylab="Real per-capita GDP (1986 USD, thousand)",
          Xlab="year",Ylim=c(0,12),
          Legend=c("Catalunya","Synthetic Catalunya"),
          Legend.position="bottomright")

#### Permutation test ####
region<-c(2:16,18)
region.ts<-list()
N=16
mspe=5*s$rgV.optim$value

for(i in 1:N){
  print(i)

  # Placebo and control regions
  treated<-region[i]
  control<-region[region!=treated]
  
  # Prepare data
  D<-dataprep(foo=basque,
             predictors=c("school.illit","school.prim","school.med",
                          "school.high","school.post.high","invest"),
             predictors.op=c("mean"),dependent=c("gdpcap"),
             unit.variable=c("regionno"),
             time.variable = c("year"),
             special.predictors = list(
               list("gdpcap",1960:1969,c("mean")),
               list("sec.agriculture",seq(1961,1969,2),c("mean")),
               list("sec.energy",seq(1961,1969,2),c("mean")),
               list("sec.industry",seq(1961,1969,2),c("mean")),
               list("sec.construction",seq(1961,1969,2),c("mean")),
               list("sec.services.venta",seq(1961,1969,2),c("mean")),
               list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
               list("popdens",1969,c("mean"))),
             treatment.identifier=treated,
             controls.identifier=control,
             time.predictors.prior=c(1964:1969),
             time.optimize.ssr=c(1960:1969),
             unit.names.variable=c("regionname"),
             time.plot=c(1955:1997)) 
  
  # Fit model 
  s.out<-synth(data.prep.obj=D,method="BFGS")
  
  # Discard regions where the MPSE is five time larger than the Basque country
  if (s.out$rgV.optim$value>mspe){
    region.ts[[i]]<-NA
  }else{
  region.ts[[i]]<-ts(D$Y1plot-(D$Y0plot %*% s.out$solution.w),start=c(1955,1))
  }
}

## Figure 4
# NB - There are some small differences compared to figure in paper
par(mar=c(5,5,2,2),pty="s",las=1,bty="n",cex.lab=2,cex.axis=2)
plot(basque.ts,ylim=c(-2,2),axes=FALSE,
     ylab="Gap in real per-capita GDP (1986 USD, thousand)",xlab="",type="n")

for(i in 1:N){ lines(region.ts[[i]],col="grey50") }
lines(basque.ts,lwd=2)

axis(1,tick=FALSE);axis(2,tick=FALSE)
abline(h=0,lty=2);abline(v=1970,lty=3)


