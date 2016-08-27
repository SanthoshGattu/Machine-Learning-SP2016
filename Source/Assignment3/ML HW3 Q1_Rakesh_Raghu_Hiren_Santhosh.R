library("ggplot2")
library("dplyr")
library("reshape2")

##Initialization: Determining the Initial GMM Parameters

randomdata <- read.xlsx("C:/Users/Santhosh/Desktop/GausssanMixtures.xlsx", 1 )

newdata <- randomdata[1:4500, ]  

wait <- newdata$number

wait.kmeans <- kmeans(wait, 3) 
wait.kmeans.cluster <- wait.kmeans$cluster 

wait.df <- data.frame(x = wait, cluster = wait.kmeans.cluster) 

wait.df %>%
  mutate(num = row_number()) %>%
  ggplot(aes(y = num, x = x, color = factor(cluster))) +
  geom_point() +
  ylab("Values") +
  ylab("Data Point Number") +
  scale_color_discrete(name = "Cluster")
  
  ## Calculating mean and S.D
  
  wait.summary.df <- wait.df %>%
    group_by(cluster) %>%
    summarize(mu = mean(x), std = sd(x), size = n())
  
  wait.summary.df %>%
    select(cluster, mu, std)
  
  ## generate the initial mixing weights
  
  wait.summary.df <- wait.summary.df %>%
    mutate(alpha = size / sum(size))
  
  wait.summary.df %>%
    select(cluster, size, alpha)
  
  ## Calculating the "Soft Labels" of Each Data Point (E-step)
  
  comp1.prod <- 
    dnorm(66, wait.summary.df$mu[1], wait.summary.df$std[1]) *
    wait.summary.df$alpha[1]
  
  comp2.prod <- 
    dnorm(66, wait.summary.df$mu[2], wait.summary.df$std[2]) *
    wait.summary.df$alpha[2]
  
  comp3.prod <- 
    dnorm(66, wait.summary.df$mu[3], wait.summary.df$std[3]) *
    wait.summary.df$alpha[3]
  
  comp1.prod
  comp2.prod
  comp3.prod
  
  normalizer <- comp1.prod + comp2.prod + comp3.prod
  
  
  comp1.post <- comp1.prod / normalizer
  comp2.post <- comp2.prod / normalizer
  comp3.post <- comp3.prod / normalizer
  
  
  
  
  comp1.n <- sum(comp1.prod)
  comp2.n <- sum(comp2.prod)
  comp3.n <- sum(comp3.prod)
  
  
  comp1.mu <- 1/comp1.n * sum(comp1.post * wait)
  comp2.mu <- 1/comp2.n * sum(comp2.post * wait)
  comp3.mu <- 1/comp3.n * sum(comp3.post * wait)
  
  
  comp1.var <- sum(comp1.post * (wait - comp1.mu)^2) * 1/comp1.n
  comp2.var <- sum(comp2.post * (wait - comp2.mu)^2) * 1/comp2.n
  comp3.var <- sum(comp2.post * (wait - comp3.mu)^2) * 1/comp3.n
  
  
  comp1.alpha <- comp1.n / length(wait)
  comp2.alpha <- comp2.n / length(wait)
  comp3.alpha <- comp3.n / length(wait)
  
  
  comp.params.df <- data.frame(comp = c("comp1", "comp2","comp2"),
                               comp.mu = c(comp1.mu, comp2.mu,comp3.mu),
                               comp.var = c(comp1.var, comp2.var, comp3.var),
                               comp.alpha = c(comp1.alpha, comp2.alpha, comp3.alpha),
                               comp.cal = c("self", "self","self"))
  
  ## Already calculate component responsibilities for each data point from above
  sum.of.comps <- comp1.prod + comp2.prod + comp3.prod
  sum.of.comps.ln <- log(sum.of.comps, base = exp(1))
  sum(sum.of.comps.ln)
  
  #' Expectation Step of the EM Algorithm
  #'
  #' Calculate the posterior probabilities (soft labels) that each component
  #' has to each data point.
  
  e_step <- function(x, mu.vector, sd.vector, alpha.vector) {
    comp1.prod <- dnorm(x, mu.vector[1], sd.vector[1]) * alpha.vector[1]
    comp2.prod <- dnorm(x, mu.vector[2], sd.vector[2]) * alpha.vector[2]
    comp3.prod <- dnorm(x, mu.vector[3], sd.vector[3]) * alpha.vector[3]
    
    sum.of.comps <- comp1.prod + comp2.prod + comp3.prod 
    comp1.post <- comp1.prod / sum.of.comps
    comp2.post <- comp2.prod / sum.of.comps
    comp3.post <- comp3.prod / sum.of.comps
    
    
    sum.of.comps.ln <- log(sum.of.comps, base = exp(1))
    sum.of.comps.ln.sum <- sum(sum.of.comps.ln)
    
    list("loglik" = sum.of.comps.ln.sum,
         "posterior.df" = cbind(comp1.post, comp2.post, comp3.post))
  }
  
  
  #' Maximization Step of the EM Algorithm
  #'
  #' Update the Component Parameters
  
  m_step <- function(x, posterior.df) {
    comp1.n <- sum(posterior.df[, 1])
    comp2.n <- sum(posterior.df[, 2])
    comp3.n <- sum(posterior.df[, 3])
    
    comp1.mu <- 1/comp1.n * sum(posterior.df[, 1] * x)
    comp2.mu <- 1/comp2.n * sum(posterior.df[, 2] * x)
    comp3.mu <- 1/comp2.n * sum(posterior.df[, 3] * x)
    
    
    comp1.var <- sum(posterior.df[, 1] * (x - comp1.mu)^2) * 1/comp1.n
    comp2.var <- sum(posterior.df[, 2] * (x - comp2.mu)^2) * 1/comp2.n
    comp3.var <- sum(posterior.df[, 3] * (x - comp3.mu)^2) * 1/comp3.n
    
    
    comp1.alpha <- comp1.n / length(x)
    comp2.alpha <- comp2.n / length(x)
    comp3.alpha <- comp3.n / length(x)
    
    list("mu" = c(comp1.mu, comp2.mu, comp3.mu),
         "var" = c(comp1.var, comp2.var, comp3.var),
         "alpha" = c(comp1.alpha, comp2.alpha, comp3.alpha))
  }
  
  for (i in 1:50) {
    if (i == 1) {
      # Initialization
      e.step <- e_step(wait, wait.summary.df[["mu"]], wait.summary.df[["std"]],
                       wait.summary.df[["alpha"]])
      m.step <- m_step(wait, e.step[["posterior.df"]])
      cur.loglik <- e.step[["loglik"]]
      loglik.vector <- e.step[["loglik"]]
    } else {
      # Repeat E and M steps till convergence
      e.step <- e_step(wait, m.step[["mu"]], sqrt(m.step[["var"]]), 
                       m.step[["alpha"]])
      m.step <- m_step(wait, e.step[["posterior.df"]])
      loglik.vector <- c(loglik.vector, e.step[["loglik"]])
      
      loglik.diff <- abs((cur.loglik - e.step[["loglik"]]))
      if(loglik.diff < 1e-6) {
        break
      } else {
        cur.loglik <- e.step[["loglik"]]
      }
    }
  }
  loglik.vector
  
  m.step
  
  plot_mix_comps <- function(x, mu, sigma, lam) {
    lam * dnorm(x, mu, sigma)
  }
  
  data.frame(x = wait) %>%
    ggplot() +
    geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", 
                   fill = "white") +
    stat_function(geom = "line", fun = plot_mix_comps,
                  arg = list(m.step$mu[1], sqrt(m.step$var[1]), 
                             lam = m.step$alpha[1]),
                  colour = "black", lwd = 1.5) +
    stat_function(geom = "line", fun = plot_mix_comps,        
                  arg = list(m.step$mu[2], sqrt(m.step$var[2]), 
                             lam = m.step$alpha[2]),
                  colour = "red", lwd = 1.5) +
    stat_function(geom = "line", fun = plot_mix_comps,
                  arg = list(m.step$mu[3], sqrt(m.step$var[3]), 
                             lam = m.step$alpha[3]),
                  colour = "green", lwd = 1.5) +
    
    ylab("Density") +
    xlab("Values")