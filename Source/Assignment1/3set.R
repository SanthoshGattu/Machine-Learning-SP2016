my3Data = read.csv("C:/Users/Rakesh/Desktop/ML/trainingdata33.csv")
my3Data[ncol(my3Data)]<-NULL
#my3Data
my3matrix=data.matrix(my3Data)
meanData=as.data.frame( t(sapply(my3Data, function(cl) list(means=mean(cl,na.rm=TRUE))) ))
meanData<-data.matrix(meanData)
#meanData
xcovmatrix<-cov(my3matrix)
#xcovmatrix
test<-c(9,79,68,75,25,34,38,2,28)
library("mnormt")
valuex1=pmnorm(test,meanData,xcovmatrix)

valuex1