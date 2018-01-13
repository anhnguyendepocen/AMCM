### Homework 2 - Sampling from the Exponential distribution using the Uniform ###
# Monte Carlo Methods - EMAp
# Raul Guarini

# Sampling from an exponential distribution using an uniform one

sample_size <- as.numeric(readline(prompt = "Sample size: "))
lambda <- as.numeric(readline(prompt = "Exponential distribution parameter: "))

uniform <- runif(sample_size)
expo_sim <- (-1/2)*log(uniform)/lambda

cat("Exponential sample: ", expo_sim)

graph <- hist(expo_sim)
graph$density <- graph$counts/sum(graph$counts)
plot(graph, freq = FALSE, col = "blue")