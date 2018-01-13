### Homework 2 - Example 2.5 from Lecture Notes (Importance Sampling) ###
# Monte Carlo Methods - EMAp
# Raul Guarini

# The goal is to estimate E(|X|), where X is a t-Student(3). Three possibilities are explored:
# 1) Sampling directly from t-Student(3) an using a naive Monte Carlo SLGN estimator
# 2) Using t-Student(1) as an instrumental distribution
# 3) Using N(0,1) as an instrumental distribution

library(stats)
library(ggplot2)

up_limit <- 1500
granular <- 10

iterations <- seq(from = 0, to = up_limit, by = granular)
iterations[1] <- 1

replications <- 100

path <- numeric(replications)  # stores the estimates for each replication
max <- numeric(length(iterations)) # stores the maximal estimate for each iteration
min <- numeric(length(iterations)) # stores the minimal estimatio for each iteration
mu <- numeric(length(iterations))  # stores the estimate for each iteration

choice <- as.numeric(readline(prompt = "Which method? 1,2 or 3?: "))

if (choice == 1) {
  # ---- Method 1 ----
  
  for (n in 1:length(iterations)) {
    for (count in 1:replications) {
      realizations <- abs(rt(n, df = 3, ncp = 0))
      path[count] <- mean(realizations)
    }
    max[n] <- max(path)
    min[n] <- min(path)
    mu[n] <- mean(path)
  }
  
  # Plotting a nice graph with ggplot2
  df <- data.frame(iterations, mu, max, min)
  
  graph_1 <- ggplot(data = df, aes(x = iterations)) + 
    geom_line(aes(y = max), colour = "gray") +
    geom_line(aes(y = min), colour = "gray") +
    geom_line(aes(y = mu)) +
    xlab("Sample size n") +
    ylab("Estimate - Method 1") +
    xlim(0, 1600) +
    ylim(0, 3)
  
  print(graph_1)

  } else if (choice == 2) {

  # ---- Method 2 ----
  
  for (n in 1:length(iterations)) {
    for (count in 1:replications) {
      X <- rt(n, df = 1, ncp = 0)
      f <- dt(X, df = 3, ncp =0)
      g <- dt(X, df = 1, ncp =0)
      w <- f/g
      h <- abs(X)
      realizations <- w*h
      path[count] <- mean(realizations)
    }
    max[n] <- max(path)
    min[n] <- min(path)
    mu[n] <- mean(path)
  }
  
  # Plotting a nice graph with ggplot2
  df <- data.frame(iterations, mu, max, min)
  
  graph_2 <- ggplot(data = df, aes(x = iterations)) + 
    geom_line(aes(y = max), colour = "gray") +
    geom_line(aes(y = min), colour = "gray") +
    geom_line(aes(y = mu)) +
    xlab("Sample size n") +
    ylab("Estimate - Method 2") +
    xlim(0, 1600) +
    ylim(0, 3)
  
  print(graph_2)

} else if (choice == 3) {
  
  # ---- Method 3 ----
  
  for (n in 1:length(iterations)) {
    for (count in 1:replications) {
      X <- rnorm(n)
      f <- dt(X, df = 3, ncp =0)
      g <- dnorm(X)
      w <- f/g
      h <- abs(X)
      realizations <- w*h
      path[count] <- mean(realizations)
    }
    max[n] <- max(path)
    min[n] <- min(path)
    mu[n] <- mean(path)
  }
  
  # Plotting a nice graph with ggplot2
  df <- data.frame(iterations, mu, max, min)
  
  graph_3 <- ggplot(data = df, aes(x = iterations)) + 
    geom_line(aes(y = max), colour = "gray") +
    geom_line(aes(y = min), colour = "gray") +
    geom_line(aes(y = mu)) +
    xlab("Sample size n") +
    ylab("Estimate - Method 3") + 
    xlim(0, 1600) +
    ylim(0, 3)
  
  print(graph_3)
  
} else {
  print("Wrong method choice! Please, choose 1,2 or 3.")
}



