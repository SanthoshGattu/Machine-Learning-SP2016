#install.packages("gendata")
library(gendata)
sdata<-genmvnorm(cor=c(.7,.2,.3),k=3,n=10,seed=12345)
cor(sdata)
summary(sdata)
#note: data are in z scores
s2<-dtrans(sdata,c(0,100,50),c(1,15,10),rnd=FALSE)
summary(s2)
sd(s2[,1])
sd(s2[,2])
sd(s2[,3])
#note: variables X2 and X3 are now rescaled with the appropriate means and standard deviations.
head(s2)
s2<-dtrans(sdata,c(0,100,50),c(1,15,10),rnd=TRUE)
#at times, you may want a dataset to not have decimals. use rnd=T.
head(s2)
s2

s2.dist=dist(s2)
library(ade4)
dist1 <- dist.quant(s2, 1, diag = TRUE, upper = TRUE)
dist1
head(s2.dist)
write.csv(s2, file = "C:\\Users\\Rakesh\\Desktop\\ML\\Home Work\\HW3\\MyData.csv")
library(scatterplot3d)
scatterplot3d(s2$X1, s2$X2, s2$X3, highlight.3d = TRUE, angle = 120,
              col.axis = "blue", col.grid = "lightblue", cex.axis = 1.3,
              cex.lab = 1.1, main = "Helix", pch = 20)



"initMDS" <- function(x, k=2)
{
  nr <- attributes(x)$Size
  res <- runif(nr*k, min(x), max(x))
  dim(res) <- c(nr,k)
  res
}
library(MASS)
s2.dist <- dist(unique(s2)) #which uses euclidean distance by default 
# Provide sammon function a random initial configuration, 
# a lower tolerance and magic value
s2.samm <- sammon(s2.dist, initMDS(s2.dist),k = 2,tol=1e-10, magic=0.01)


dist2 <- dist.quant(s2.samm$points, 1, diag = TRUE, upper = TRUE)
dist2
plot(s2.samm$points,xlab="x", col="blue", ylab="y")

