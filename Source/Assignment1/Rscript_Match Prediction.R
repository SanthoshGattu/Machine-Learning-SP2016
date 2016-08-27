test<-c(10,84,62,67,38,33,47,4,36,4)
set=test[10]
if(set==3){
  myData = read.csv("C:/Users/Rakesh/Desktop/ML/trainingdata33.csv")
  myDataForWin = subset(myData, Result=="W")
  myDataForLoss = subset(myData, Result=="L")
  myDataForWin[ncol(myDataForWin)]<-NULL
  myDataForLoss[ncol(myDataForLoss)]<-NULL
  myDataForWin
  myDataForLoss
  
  print("Inside 3sets")
  #rm(test)
  #test<-scan()
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
    print("Player won with this stats")
    #probW
  }else{
    print("Player Loss with this stats")
    #probL
  }
 
  
}else if(set==4)
{
  my4Data = read.csv("C:/Users/Rakesh/Desktop/ML/trainingdata4.csv")
  my4DataForWin = subset(my4Data, Result=="W")
  my4DataForLoss = subset(my4Data, Result=="L")
  my4DataForWin[ncol(my4DataForWin)]<-NULL
  my4DataForLoss[ncol(my4DataForLoss)]<-NULL
  my4DataForWin
  my4DataForLoss
  print("Inside 4sets")
  
  #rm(test)
  #test<-scan()
  print(test)
  win4data=as.data.frame( t(sapply(my4DataForWin, function(cl) list(means=mean(cl,na.rm=TRUE),sds=sd(cl,na.rm=TRUE))) ))
  loss4data=as.data.frame( t(sapply(my4DataForLoss, function(cl) list(means=mean(cl,na.rm=TRUE),sds=sd(cl,na.rm=TRUE))) ))
  win4data
  loss4data
  lhvaluewin<-1
  
  for(i in 1:7)
  {
    
    lhvaluewin<-lhvaluewin*dnorm(test[i],as.numeric(win4data[i,1]),as.numeric(win4data[i,2]))
  }
  lhvaluewin<-lhvaluewin*(1-dnorm(test[8],as.numeric(win4data[8,1]),as.numeric(win4data[8,2])))*(1-dnorm(test[9],as.numeric(win4data[9,1]),as.numeric(win4data[9,2])))
  
  lhvalueloss<-1
  for(j in 1:7)
  {
    lhvalueloss<-lhvalueloss*dnorm(test[j],as.numeric(loss4data[j,1]),as.numeric(loss4data[j,2]))
  }
  lhvalueloss<-lhvalueloss*(1-dnorm(test[8],as.numeric(loss4data[8,1]),as.numeric(loss4data[8,2])))*(1-dnorm(test[9],as.numeric(loss4data[9,1]),as.numeric(loss4data[9,2])))
  
  prob4W=(0.5*lhvaluewin)
  prob4L=(0.5*lhvalueloss)
  if (prob4W>prob4L) {
    print("Player won with this stats")
    #probW
  }else{
    print("Player Loss with this stats")
    #probL
  }
  
  
}else
{
  myData = read.csv("C:/Users/Rakesh/Desktop/ML/trainingdata5.csv")
  myDataForWin = subset(myData, Result=="W")
  myDataForLoss = subset(myData, Result=="L")
  myDataForWin[ncol(myDataForWin)]<-NULL
  myDataForLoss[ncol(myDataForLoss)]<-NULL
  myDataForWin
  myDataForLoss
  print("Inside 5sets")
  
  #rm(test)
  #test<-scan()
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
    print("Player won with this stats")
    #probL
  }else{
    print("Player Loss with this stats")
    #probW
  }
  
}

