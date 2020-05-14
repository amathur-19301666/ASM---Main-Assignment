# Importing the necessary packages
library(ggplot2)
library(dplyr)
library(ggpubr)
library(MCMCpack)

# Reading the data and processing it
original_wines = read.csv("winemag-data-130k-v2.csv")
filtered_wines = filter(original_wines, (country == "Italy"  & price < 20))
italian_wines = filter(original_wines, (country == "Italy"))
filtered_wines = filter(filtered_wines, (region_1 !="Sardinia"))
counter = table(filtered_wines$region_1)
counter = as.data.frame(counter)
counter = counter[-1,]
counter = counter %>% filter(Freq > 3)
filtered_wines <- filtered_wines[filtered_wines$region_1 %in% counter$Var1, , drop = FALSE]
filtered_wines$region_index <- as.numeric(factor(filtered_wines$region_1))
reorder(filtered_wines$region_1, filtered_wines$region_1, length)
tapply(filtered_wines$region_1, filtered_wines$region_1, length)

mean(filtered_wines$points)

# Visualizations
ggplot(filtered_wines) + geom_boxplot(aes(x = reorder(region_1, points, median), points, fill = reorder(region_1, points, median)), show.legend=FALSE)
ggplot(filtered_wines, aes(x = reorder(region_1, region_1, length))) + stat_count()
ggplot(filtered_wines, aes(points)) + stat_bin()
ggplot(data.frame(size = tapply(filtered_wines$points, filtered_wines$region_1, length), mean_score = tapply(filtered_wines$points, filtered_wines$region_1, mean)), aes(size, mean_score)) + geom_point()

# Gibbs Sampler Function
compare_m_gibbs <- function(y, ind, mu0 = 86.7, tau0 = 1/400,
                            a0 = 1, b0 = 86.7, alpha0 =1, beta0 = 86.7, maxiter = 5000)
{
  ### weakly informative priors
  a0 <- 1/2 ; b0 <- 50 ## tau_w hyperparameters
  alpha0 <-1/2 ; beta0 <- 50 ## tau_b hyperparameters
  mu0<-50 ; tau0 <- 1/25
  ###
  ### starting values
  m <- nlevels(factor(ind))
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  alphan <- alpha0 + sum(n_m)/2
  ###
  ### setup MCMC
  theta_mat <- matrix(0, nrow = maxiter, ncol = m)
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  ###
  ### MCMC algorithm
  for(s in 1:maxiter)
  {
    # sample new values of the thetas
    for(j in 1:m)
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    betan <- beta0 + ss/2
    tau_w <- rgamma(1, alphan, betan)
    #sample a new value of mu
    taum <- m * tau_b + tau0
    mum <- (mean(theta) * m * tau_b + mu0 * tau0) / taum
    mu <- rnorm(1, mum, 1/ sqrt(taum))
    # sample a new value of tau_b
    am <- a0 + m/2
    bm <- b0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, am, bm)
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}

# Using the gibbs sampler and analyzing the results
fit <- compare_m_gibbs(filtered_wines$points, filtered_wines$region_index)
apply(fit$params, 2, mean)
apply(fit$params, 2, sd)
mean(1/sqrt(fit$params[, 3]))
sd(1/sqrt(fit$params[, 3]))
theta_hat=apply(fit$theta,2,mean)
fit_n <- as.mcmc(fit2$params)
acf(fit_n)
raftery.diag(fit_n)

# Finding regions better than the average italian wine
mean(filtered_wines$points)
result=data.frame(size = tapply(filtered_wines$points, filtered_wines$region_1, length), theta_hat = theta_hat)
result$region = row.names(result)
result = filter(result, (theta_hat > 86.58507))

# evaluating the error 
theta_ci <- apply(fit$theta, 2, quantile, prob = c(0.025, .975)) ## upper/lower bounds for thetas
df_error <- data.frame(lower = theta_ci[1, ], upper = theta_ci[2, ], mean = theta_hat, region = factor(1:161))
theta_df <- data.frame(samples = as.numeric(fit$theta), region = rep(1:ncol(fit$theta), each = nrow(fit$theta)))

# Visualizing
ggplot(data.frame(size = tapply(filtered_wines$points, filtered_wines$region_1, length), theta_hat = theta_hat), aes(size, theta_hat)) + geom_point()
ggplot(df_error, aes(x = reorder(region, mean), mean)) + geom_errorbar(aes(ymin = lower, ymax = upper))
ggplot(theta_df) + geom_boxplot(aes(x = reorder(region, samples, median), samples, fill = reorder(region, samples, median)), show.legend=FALSE)