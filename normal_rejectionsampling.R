### Homework 2 - Example of Rejection Sampling ###
# Monte Carlo Methods - EMAp
# Raul Guarini

# Using a Cauchy distribution and Rejection Sampling method to sample from
# a normal distribution
library(stats)

sample_size <- as.numeric(readline(prompt = "Sample size: "))
mu_sim <- as.numeric(readline(prompt = "Mean: "))
sigma2_sim <- as.numeric(readline(prompt = "Variance: "))

M <- sqrt(2*pi*exp(-1/2))   # Best constant, from lecture notes
norm_sample <- numeric(sample_size)

# Building a function that extracts one sample from the desired N(mu, sigma2)
N_sampler <- function(mu = 0, sigma2 = 1, M) {
  test <- 0
  while (test != 1) {
    x <- rcauchy(1)
    threshold <- dnorm(x)/(M * dcauchy(x) )
    test <- rbinom(n = 1, size = 1, prob = threshold) 
  }
  reshape <- (x + mu) * sigma2
  return(reshape)
}

for (i in 1:sample_size) {
  norm_sample[i] <- N_sampler(mu = mu_sim, sigma2 = sigma2_sim, M)
}

cat("Desired normal sample: ", norm_sample)

graph <- hist(norm_sample)
graph$density <- graph$counts/sum(graph$counts)
plot(graph, freq = FALSE, col = "blue")





