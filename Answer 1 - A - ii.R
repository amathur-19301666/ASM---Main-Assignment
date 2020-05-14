# Importing the necessary packages
library(ggplot2)
library(dplyr)
library(ggpubr)
library(MCMCpack)

# Reading the data and processing it
wines = read.csv("winemag-data-130k-v2.csv")
wines = filter(wines, (country == "South Africa" & variety == "Sauvignon Blanc" & price == 15) | (country == "Chile" & variety == "Chardonnay" & price == 15))

# Converting the country column to numerical format for ease in computation
wines$country[wines$country == "Chile"] <- 1
wines$country[wines$country == "South Africa"] <- 2

# Gibbs Sampler Function
  compare_2_gibbs <- function(y, ind, mu0 = 85.66667, tau0 = 1/400, del0 = 0, gamma0 = 1/400,
                            a0 = 1, b0 = 85.66667, maxiter = 5000)
{
  y1 <- y[ind == 1]
  y2 <- y[ind == 2]
  n1 <- length(y1)
  print(y)
  n2 <- length(y2)
  print(y2)
  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  for(s in 1 : maxiter)
  {
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    ##update mu
    taun <- tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    ##update del
    gamman <- tau0 + tau*(n1 + n2)
    deln <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  return(mat_store)
}

# Using the gibbs sampler and analyzing the results
fit <- compare_2_gibbs(wines$points, as.factor(wines$country))
plot(as.mcmc(fit))
raftery.diag(as.mcmc(fit))
acf(as.mcmc(fit))

apply(fit, 2, mean)
apply(fit, 2, sd)
mean(1/sqrt(fit[, 3]))
sd(1/sqrt(fit[, 3]))

# Simulating the values to find probability 
y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))
mean(y1_sim < y2_sim)

# Visualizing the simulations
ggplot(data.frame(y_sim_diff = y1_sim - y2_sim)) + stat_bin(aes(y_sim_diff))
ggplot(data.frame(y1_sim, y2_sim)) + geom_point(aes(y1_sim, y2_sim), alpha = 0.3) + geom_abline(slope = 1, intercept = 0)
