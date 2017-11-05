# Functions for analysis
# Fit model, interpolate data

InterPL<-function(x){
  
  # 0) Print summary of data
  print(paste("Number of events=",length(x)))
  print(paste("Reported fatalities=",sum(x)))
  print(paste("Median fatality number=",median(x)))
  
  # 1) Fit powerlaw to data
  require(poweRlaw)
  
  pl=displ$new(x) 
  est=estimate_xmin(pl)
  pl$setXmin(est)
  Xmin<-pl$xmin
  alpha<-pl$pars
  
  # Bootstrap p-value
  #bts_p=bootstrap_p(pl,no_of_sims=1000,seed=42,threads=3)
  
  # Give summary
  print(paste("alpha=",round(alpha,digits=2)))
  print(paste("Xmin=",Xmin))
  
  # 2) Interpolate data
  # Indices
  a=1:max(x)
  n=1:max(x)
  
  # Calculate frequency per event size
  for(i in 1:length(n)){
    n[i]=sum(x==i,na.rm=TRUE)
    }
  
  # Set parameters (can take a few moments)
  if (Xmin>1){
    S=cumsum(n)
    lambda=S[length(S)]-S[Xmin-1]
    K<-99999999
    Z<-sum(1/(0:K+Xmin)^alpha) # Hurwitz zeta normalisation constant
    }
  
  # Estimate number of incidents and fatalities
  Proj<-c() 
  for (j in 1:Xmin-1){
    Proj[j]=lambda*a[j]^((-1)*alpha)*1/Z
    }
  
  R<-a[1:length(Proj)]*Proj # Number of fatalities
  O<-a[1:Xmin-1]*n[1:Xmin-1]
  
  # Give summary
  print(paste("Interpolated fatalities=",round(sum(R),digits=2)))
  print(paste("Total fatalities=",round(sum(x[x>=Xmin])+sum(R),digits=2)))
  
  # 3) Plot figures
  par(mar=c(5,5.5,2,1),mfrow=c(2,2),cex.lab=1.5,cex.axis=1.5,las=1)
  
  # a)
  plot(density(x,from=0.000001),log='x',xlim=c(1,max(x)),bty="n",axes=FALSE,
       main="(a) Density fatailities",xlab="Magnitude",ylab="")
  axis(1,tick=FALSE);axis(2,tick=FALSE)
  
  # b)
  plot(pl,bty="n",main=" (b) Power law fit", ylab="",xlab="Magnitude",
       axes=FALSE)
  lines(pl,col="red",lwd=2)
  axis(1,tick=FALSE)
  axis(2,tick=FALSE,at=c(.001,.01,.05,.1,.5,1),label=c(.001,.01,.05,.1,.5,1))
  
  # c)
  plot(a[1:Xmin-1],n[1:Xmin-1],ylim=c(1,max(Proj)),log="y",pch=0,type="b",
       bty="n",main="(c) Number of events of size X",xlab="Event size",ylab="",
       axes=FALSE)
  points(a[1:Xmin-1],Proj,pch=19,type="b");text(1.8,Proj[1],"Projected")
  axis(1,tick=FALSE);axis(2,tick=FALSE)
  
  # d)
  plot(a[1:Xmin-1],O[1:Xmin-1],ylim=c(40,max(R)),log="y",pch=0,type="b",bty="n",
       main="(d) Number of fatalities per event size",xlab="Event size",ylab="",
       axes=FALSE)
  points(a[1:Xmin-1],R,pch=19,type="b");text(1.8,Proj[1],"Projected")
  axis(1,tick=FALSE);axis(2,tick=FALSE)
}