#Read the Data from the Local Machine
myData = read.csv("C:/Users/Rakesh/Desktop/ML/HW2/data3.csv")
myData
# function to normalize the data for every column between 0-1 values
doit <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                 min(x, na.rm=TRUE))}
# use lapply to apply doit() to every column in a data frame
normed <- as.data.frame(lapply(myData, doit))
# very that the range of all is [0, 1]
lapply(normed, range)
normed
# Partitioning Data into Train and Test datasets in 70:30
install.packages("caret")
install.packages("doParallel")
install.packages("ggplot2")
library(caret)
library(doParallel)
library(ggplot2)
set.seed(3500)
p=0.7
createDataPartitionTest = unlist(foreach(i = 1:6000) %do% {
  set.seed(i)
  index = createDataPartition(normed$Result, p = p)$Resample1
  t.test(normed[index,]$Result, normed[-index,]$Result)$p.value  
  tennis.train <- normed[index, ]
  tennis.test <- normed[-index, ]
  
})
tennis.train
tennis.test
results = data.frame(p.value = c(createDataPartitionTest),
                     class = c(rep("createDataPartition", length(createDataPartitionTest))))
ggplot(results, aes(x = results$p.value)) +
  theme_bw() +
  geom_density(aes(fill = class), alpha = 0.5)

# Fit a Single Hidden Layer Neural Network using Least Squares
library(nnet)
train.nnet<-nnet(Result~.,tennis.train,size=3,rang=(1/max(data[,])),Hess=FALSE,decay=15e-4,maxit=250)
test.nnet<-predict(train.nnet,tennis.test)
table(tennis.test$Result,test.nnet)
plot(test.nnet)

attach(tennis.train)
library(neuralnet)
nn <- neuralnet(Result~ Aces+First.Serve.points+Second.Serve.points+Net.points+Break.points+Receiving.points+Winners+Double.Faults+Unforced.Errors,data=myData,hidden=8,threshold=0.01,err.fct="sse",linear.output=FALSE,likelihood=TRUE,stepmax=1e+05,rep=1,startweights=NULL,learningrate.limit=list(0.1,1.5),learningrate.factor=list(minus=0.5,plus=1.5),learningrate=0.5,lifesign="minimal",lifesign.step=1000,algorithm="backprop",act.fct="logistic",exclude=NULL,constant.weights=NULL)
summary(nn)
gwplot(nn,selected.covariate="Aces")
plot(nn,rep="best")
prediction (nn)
print(nn)
plot(nn)
columns=c("Aces","First.Serve.points","Second.Serve.points","Net.points","Break.points","Receiving.points","Winners","Double.Faults","Unforced.Errors")
testdata2<-subset(tennis.test,select=columns)
testnn<-compute(nn,testdata2,rep=1)
## Confusion matrix
table(tennis.test$Result,testnn$net.result)
cbind(tennis.test$Result,testnn$net.result)
print(testnn)


