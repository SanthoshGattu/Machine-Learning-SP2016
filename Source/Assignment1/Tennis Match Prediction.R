myData = read.csv("C:/Users/Santhosh/Desktop/Training data_Set 3,4,5.csv")
myDataForWin = subset(myData, Result=="W")
myDataForLoss = subset(myData, Result=="L")
myDataForWin[ncol(myDataForWin)]<-NULL
myDataForLoss[ncol(myDataForLoss)]<-NULL
myDataForWin
myDataForLoss
test<-c(12,68,35,65,33,33,40,5,65)
print(test)
windata=as.data.frame( t(sapply(myDataForWin, function(cl) list(means=mean(cl,na.rm=TRUE),sds=sd(cl,na.rm=TRUE))) ))
lossdata=as.data.frame( t(sapply(myDataForLoss, function(cl) list(means=mean(cl,na.rm=TRUE),sds=sd(cl,na.rm=TRUE))) ))
windata
lossdata
lhvaluewin<-1

for(i in 1:7)
{
  
  lhvaluewin<-lhvaluewin*dnorm(test[i],as.numeric(windata[i,1]),as.numeric(windata[i,2]))
}
lhvaluewin<-lhvaluewin*(1-dnorm(test[8],as.numeric(windata[8,1]),as.numeric(windata[8,2])))*(1-dnorm(test[9],as.numeric(windata[9,1]),as.numeric(windata[9,2])))

lhvalueloss<-1
for(j in 1:7)
{
  lhvalueloss<-lhvalueloss*dnorm(test[j],as.numeric(lossdata[j,1]),as.numeric(lossdata[j,2]))
}
lhvalueloss<-lhvalueloss*(1-dnorm(test[8],as.numeric(lossdata[8,1]),as.numeric(lossdata[8,2])))*(1-dnorm(test[9],as.numeric(lossdata[9,1]),as.numeric(lossdata[9,2])))

probW=(0.5*lhvaluewin)
probL=(0.5*lhvalueloss)
if (probW>probL) {
  print("Wining stats")
}else{
  print("Losing stats")
}
probW
probL


