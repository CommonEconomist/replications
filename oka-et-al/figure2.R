# data created 2019.07.25
# last update  2020.01.22
setwd("~/github/replications/oka-et-al")

# FIGURE 2
par(mar = c(5, 5, 2, 5), mfrow = c(3,1), bty = "n", las = 1)

# A
d<-read.csv("data/sd01.csv")
plot(war_group_size ~ population, d , log="xy", 
     pch = 5, cex = 2,
     xlim=c(200, 2e9), ylim=c(1e-4, 1e8), axes = FALSE,
     main = "A", xlab = "Population", ylab = "W and W/P")
abline(lm(log(war_group_size) ~ log(population), d), lwd = 2)

points(W_P ~ population, d, pch = 0, cex = 2)
abline(lm(log(W_P) ~ log(population), d), lwd = 2)

axis(1, tick =FALSE)
axis(2, tick = FALSE)


# B
d<-read.csv("data/sd02.csv")
plot(C ~ W, d , log="xy", xlim=c(10, 1e9), ylim=c(1e-3, 1e8), axes = FALSE,
     pch = 5, cex = 2, 
     main = "B", xlab = "War group size (W)", ylab = "C and C/W")
abline(lm(log(C) ~ log(W), d), lwd = 2)

points(C_W ~ W, d, pch = 0, cex = 2)
abline(lm(log(C_W) ~ log(W), d), lwd = 2)

axis(1, tick =FALSE)
axis(2, tick = FALSE)

# C
d<-read.csv("data/sd04.csv")
d<-d[1:65,]
plot(casualties ~ population, d , log="xy", axes = FALSE, pch = 5, cex = 2,
     xlim=c(1000, 1e9), ylim=c(1e-5, 1e8), main = "C",
     xlab = "Population (P)", ylab = "G and G/P")
abline(lm(log(casualties) ~ log(population), d), lwd = 2)

points(C_P ~ population, d, pch = 0, cex = 2)
abline(lm(log(C_P) ~ log(population), d), lwd = 2)

axis(1, tick =FALSE)
axis(2, tick = FALSE)
