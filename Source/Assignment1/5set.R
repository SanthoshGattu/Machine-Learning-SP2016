my4Data = read.csv("C:/Users/Rakesh/Desktop/ML/trainingdata5.csv")
my4Data[ncol(my4Data)]<-NULL
#my4Data
my4matrix=data.matrix(my4Data)
mean4Data=as.data.frame( t(sapply(my4Data, function(cl) list(means=mean(cl,na.rm=TRUE))) ))
mean4Data<-data.matrix(mean4Data)
#mean4Data
x4covmatrix<-cov(my4matrix)
#x4covmatrix
test<-c(9,79,68,75,25,34,38,2,28)
library("mnormt")
valuex1=pmnorm(test,mean4Data,x4covmatrix)

valuex1