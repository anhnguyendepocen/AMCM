### Homework - Example 4.4 from Lecture Notes (Gibbs Sampler) ###
# Monte Carlo Methods - EMAp
# Raul Guarini

# The goal is to estimate P(X1 >= 0, X2 >= 0) where X1,X2 follow a bivariate Gaussian distribution using
# the Systematic Sweep Gibbs Sample (Algorithm 4.1) from page 40.

library(stats)
library(ggplot2)    # just to make a nice plot!

#---- Parameters ----
mu1 <- 0
mu2 <- 0
sigma11 <- 1
sigma22 <- 1
sigma12 <- 0.3

var_size <- 2
x1_0 <- rnorm(1, 0, 10)
x2_0 <- rnorm(1, 0, 10)
initial_var <- c(x1_0, x2_0)    # Initial condition for Gibbs sampler

iterations <- seq(from = 0 , to = 10000, by = 10)
iterations[1] <- 10

estimates <- numeric(length(iterations))
upper_bound <- numeric(length(iterations))
lower_bound <- numeric(length(iterations))

# ---- Sampling ----
for (i in 1:length(iterations)) {
  h <- numeric(i)
  for (t in 1:i) { 
    x1_new <- rnorm(1, mu1 + sigma12/((sigma22^2)*(x2_0 - mu2)), sigma11^2 - (sigma12)^2/sigma22^2)
    x2_new <- rnorm(1, mu2 + sigma12/((sigma11^2)*(x1_new - mu1)), sigma22^2 - (sigma12)^2/sigma22^2)
    if (x1_new >= 0 & x2_new >= 0) {
      h[t] <- 1
    }
    # Updating the loop
    x1_0 <- x1_new
    x2_0 <- x2_new
  }
  
  estimates[i] <- sum(h)/i
  
}


# ---- Plotting results ----
plot(iterations, estimates, type = 'l', xlab = "number of iterations", ylab = 'Estimate')
final <- tail(estimates, n = 1)
print(paste("Final estimate: ", final))


