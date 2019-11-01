# sample from a grid-approximation posterior
samples_grid <- function(size = 10000, W = 6, L = 3) {
  p_grid <- seq(from = 0, to = 1, length.out = 1000)
  
  prob_p = rep(1, length(p_grid))
  prob_data <- dbinom(W, W + L, prob = p_grid)
  posterior <- prob_data * prob_p
  posterior <- posterior / sum(posterior)
  
  samples <- sample(p_grid, prob = posterior, size = size, replace = T)
  
  # plot(samples)
  dens(samples)
}

samples_grid()

# ------------------ Point estimate
# In order to decide upon a point estimate, a single-value summary of the
# posterior distribution, we need to pick a loss function. Different loss
# functions nominate different estimates.
# 
# The two most common examples are the absolute loss |d - p|
# and the quadratic loss (d - p)^2.
# 
# 1. The absolute loss leads to the median as the point estimate
# 2. The quadratic loss leads to the posterior as the point estimate

W <- 3
L <- 0
p_grid <- seq(from = 0, to = 1, length.out = 100)
prob_p = rep(1, length(p_grid))
prob_data <- dbinom(W, W + L, prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

# ----- Absolute loss point estimate
# Posterior
plot(p_grid, posterior, type = 'l', col = 'blue')
# Loss distribution of p = 0.5
loss <- posterior * abs((p_grid - 0.5))
plot(p_grid, loss, type = 'l', col = 'blue')
# Average loss of p = 0.5
sum(loss)

# Average loss distribution
loss_abs <- sapply(p_grid, function(d) sum(posterior * abs(p_grid - d)))
plot(p_grid, loss_abs, type = 'l')

# Minimun of average loss
p_grid[which.min(loss_abs)]

# ----- Quadratic loss point estimate
loss_quadratic <- sapply(p_grid, function(d) sum(posterior * (p_grid - d)^2))
plot(p_grid, loss_quadratic, type = 'l')
p_grid[which.min(loss_quadratic)]
