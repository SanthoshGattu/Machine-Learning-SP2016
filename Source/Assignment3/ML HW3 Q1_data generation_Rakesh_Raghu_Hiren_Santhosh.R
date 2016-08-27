library(xlsx)

#The number of samples from the mixture distribution
Num = 5000                 

#Sample N random uniforms U
Unif =runif(Num)

#Variable to store the samples from the mixture distribution                                             
rand.samples = rep(NA,Num)

#Sampling from the mixture
for(i in 1:N){
  if(Unif[i]<.3){
    rand.samples[i] = rnorm(1,0,1)
  }else if(Unif[i]<.8){
    rand.samples[i] = rnorm(1,10,1)
  }else{
    rand.samples[i] = rnorm(1,3,.1)
  }
}

write.xlsx(rand.samples, "C:/Users/Santhosh/Desktop/GausssanMixtures.xlsx")

#Density plot of the random samples
plot(density(rand.samples),main="Density Estimate of the Mixture Model")

#Plotting the true density as a sanity check
x = seq(-20,20,.1) 
truth = .3*dnorm(x,0,1) + .5*dnorm(x,10,1) + .2*dnorm(x,3,.1)
plot(density(rand.samples),main="Density Estimate of the Mixture Model",ylim=c(0,.2),lwd=2)
lines(x,truth,col="red",lwd=2)

legend("topleft",c("True Density","Estimated Density"),col=c("red","black"),lwd=2)