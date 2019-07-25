# Population is the main driver of war group size and conflict casualties
# https://www.pnas.org/content/114/52/E11101
# data created 2019.07.25
# last update  2019.07.25
setwd("~/github/replications/oka-et-al")

# FIG.2A
d<-read.csv("data/S1.csv")
plot(war_group_size ~ population, d , log="xy", 
     xlim=c(200, 2e9), ylim=c(1e-4, 1e8))
points(W_P ~ population, d)

# FIG.2B
d<-read.csv("data/S2.csv")
plot(C ~ W, d , log="xy", xlim=c(10, 1e9), ylim=c(1e-3, 1e8))
points(C_W ~ W, d)

# FIG.2C
d<-read.csv("data/S4.csv")
d<-d[1:65,]
plot(casualties ~ population, d , log="xy", 
     xlim=c(1000, 1e9), ylim=c(1e-5, 1e8))
points(C_P ~ population, d)

## FIN