## Peru
d<-read.csv("data/peru_1980_2000.csv")
d<-d[,c("Muertos","Muertos.1","Muertos.2","Muertos.3")]
x<-rowSums(d[,1:4],na.rm=TRUE)
x<-x[x!=0] # N=5569


InterPL(f) # No joy as Xmin=1
