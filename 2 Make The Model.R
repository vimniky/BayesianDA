library(rethinking)

# Grid Approximation
globa.grid <- function(W = 6, L = 3, n = 100) {
  grid <- seq(from = 0, to = 1, length.out = n)
  
  prior <- rep(1, length(grid))
  likelihood <- dbinom(W, size = W + L, prob = grid) * prior
  posterior <-  likelihood / sum(likelihood)
  
  plot(x = grid, y = posterior, type = 'l')
}

globa.grid()

# Quadratic Approximation
globa.qa <- function(W = 6, L = 3) {
  qa <- quap(
    alist(
      W ~ dbinom(W + L, p),
      p ~ dunif(0, 1)
    ),
    data = list(W = W, L = L)
  )
  
  m = precis(qa)
  
  curve(dbeta(x, W, L), from = 0, to = 1, col = 'blue')
  curve(dnorm(x, m$mean, m$sd), add = T, lty = 2)
}

globa.qa(24, 12)

# Stupid Monte Carlo Approximation
globa.mcmc <- function(W = 6, L = 3, n_samples = 1000) {
  p <- rep(NA, n_samples)
  p[1] = 0.5
  
  for(i in 2:n_samples) {
    p_new <- rnorm(1, p[i-1], 0.1)
    if (p_new < 0) p_new <- abs(p_new)
    if (p_new > 1) p_new <- 2 - p_new
    q0 <- dbinom(W, W + L, p[i-1])
    q1 <- dbinom(W, W + L, p_new)
    p[i] <- ifelse(runif(1) < q1 / q0, p_new, p[i-1])
  }
  
  dens(p, xlim=c(0, 1), col = 'blue')
  curve(dbeta(x, W + 1, L + 1), lty = 2, add = T)
}

globa.mcmc(24, 12, 20000)

