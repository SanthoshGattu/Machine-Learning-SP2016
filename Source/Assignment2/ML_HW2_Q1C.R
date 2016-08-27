## Machine Learning Home work-2

## Q.1.C

library(combinat)
require("stats4")
x <-c(0,1)
weight<-c(0.4,0.6)
n <- 10

Heads <- numeric(5)
Tails <- numeric(5)
LHAHead = numeric(5)
LHBHead = numeric(5)
PAHead = numeric(5)
PBHead = numeric(5)
TotAHeads = numeric(5)
TotBHeads = numeric(5)
TotATails = numeric(5)
TotBTails = numeric(5)
crow <- numeric(10)
thetaA <- 0.0000
thetaB <- 0.0000
newthetaA <- 0.6000
newthetaB <- 0.5000
iteration = 0
cp <- 4

CoinToss <- matrix(0, 5, 10)
CoinToss <- apply(CoinToss, c(1,2), function(x) sample(c(0,1),1,replace=TRUE,weight))

while((round(newthetaA,cp) != round(thetaA,cp)) & (round(newthetaB,cp) != round(thetaB,cp)))
{
   thetaA <- newthetaA
   thetaB <- newthetaB
   
for (i in 1:5) {
  
  crow <- CoinToss[i,]
  Heads[i] = sum(crow==1)
  Tails[i] = sum(crow==0)
  LHAHead[i] = combn(n,Heads[i])*(thetaA^Heads[i])*((1-thetaA)^(n-Heads[i]))
  LHBHead[i] = combn(n,Heads[i])*(thetaB^Heads[i])*((1-thetaB)^(n-Heads[i]))
  PAHead[i] = LHAHead[i] / (LHAHead[i] + LHBHead[i])
  PBHead[i] = LHBHead[i] / (LHAHead[i] + LHBHead[i])
  TotAHeads[i]= Heads[i]*PAHead[i]
  TotBHeads[i]= Heads[i]*PBHead[i]
  TotATails[i]= Tails[i]*PAHead[i]
  TotBTails[i]= Tails[i]*PBHead[i]
}

AHeads = sum(TotAHeads)
ATails = sum(TotATails)
BHeads = sum(TotBHeads)
BTails = sum(TotBTails)

FinalHeadProbA = AHeads/(AHeads + ATails)
FinalHeadProbB = BHeads/(BHeads + BTails)

newthetaA <- FinalHeadProbA
newthetaB <- FinalHeadProbB
iteration <- iteration + 1

}

CoinToss
newthetaA
newthetaB
iteration

