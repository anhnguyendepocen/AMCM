### Homework 1 - Using rain to estimate pi ###
# Monte Carlo Methods - EMAp
# Raul Guarini

sample_size <- seq(from = 0, to = 2000, by = 10)
sample_size[1] = 10

estimates <- numeric(length(sample_size))
upper_bound <- numeric(length(sample_size))
lower_bound <- numeric(length(sample_size))

i <- 1

for (n in sample_size) {
  # make it rain
  x_coord <- runif(n, min = -1, max = 1)
  y_coord <- runif(n, min = -1, max = 1)
  test <- x_coord^2 + y_coord^2 < 1     # TRUE means inside the circle
  
  # couting raindrops
  pi_hat <- 4*sum(test)/n
  estimates[i] <- pi_hat
  upper_bound[i] <- pi_hat + 1.96*sqrt(pi_hat*(4 - pi_hat)/n) 
  lower_bound[i] <- pi_hat - 1.96*sqrt(pi_hat*(4 - pi_hat)/n) 
  i <- i + 1
}

# Plotting results
library(ggplot2)
graphics.off()

df <- data.frame(sample_size, estimates, upper_bound, lower_bound)

graph <- ggplot(data = df, aes(x = sample_size)) +
         geom_line(aes(y = upper_bound), colour = "gray") + 
         geom_line(aes(y = lower_bound), colour = "gray") +
         geom_line(aes(y = estimates)) +
         geom_line(aes(y = pi), colour = "red") + 
         xlab("n") + ylab("Estimativa")

print(graph)

