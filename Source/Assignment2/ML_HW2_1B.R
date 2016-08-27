## Machine Learning Home work-2


## Q.1.b

x <-c(0,1)
weight<-c(0.4,0.6)
for (i in 1:5) {
CoinToss<-sample(x,10,replace=TRUE,weight)
print(CoinToss)
}