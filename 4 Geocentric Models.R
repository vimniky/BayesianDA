library(rethinking)

# Why Normal distribution are normal

# The process of adding together fluctuations:
# Repeatedly adding finite fluctuations results
# in a distribution of sums that have shed all 
# information about the underlying process, aside
# from mean and spread.

# When all we know or are willing to say about a 
# distribution of measures is their mean and variance,
# then the Gaussian distribution arises as the most 
# consistent with our assumptions.

# The Gaussian is a member of a family of fundamental
# natural distributions known as the exponential family.

# The Gaussian distribution is the most natural expression of 
# our state of ignorance, because if all we are willing to assume 
# is that a measure has finite variance, the Gaussian distribution 
# is the shape that can be realized in the largest number of ways and 
# does not introduce any new assumptions. It is the least surprising 
# and least informative assumption to make. In this way, the Gaussian 
# is the distribution most consistent with our assumptions. Or rather, 
# it is the most consistent with our golem’s assumptions. If you don’t 
# think the distribution should be Gaussian, then that implies that you 
# know something else that you should tell your golem about, something 
# that would improve inference.

# Normal by addition
flips <- 16
# values <- replicate(1000, 2 * sum(rbinom(flips, 1, prob = 0.5)) - flips)
values <- replicate(1000, sum(runif(16, -1, 1)))
simplehist(values)

# Multiplying small numbers is approximately the same as addition.
growth <- replicate(10000 , prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)

big <- replicate(10000 , prod(1 + runif(12, 0, 0.5)))
dens(big, norm.comp = TRUE)
dens(log(big), norm.comp = TRUE)

small <- replicate(10000 , prod(1 + runif(12, 0, 0.1)))
dens(small, norm.comp = TRUE)
dens(log(small), norm.comp = TRUE)

# Normal by multiplication
values <- replicate(10000, prod(1 + runif(16, 0, 0.5)))
dens(log(values), norm.comp = TRUE)

# $$f(x|u, \sigma^2) = {1\over{\sqrt{2\pi}\sigma}}e^{-(x-\mu)^2/2\sigma^2}$$


data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

curve(dnorm(x, 170, 20), from = 130, to = 210)
curve(dunif(x, 0, 50), from = -10, to = 60)

n <- 1e4
sample_mu <- rnorm(n, 178, 20)
sample_sigma <- runif(n, 0, 50)
prior_h <- rnorm(n, sample_mu, sample_sigma)
dens(prior_h, norm.comp = T)


mu.list <- seq(from = 150,
               to = 160,
               length.out = 100)
sigma.list <- seq(from = 7,
                  to = 9,
                  length.out = 100)
post <- expand.grid(mu = mu.list, sigma = sigma.list)
post$LL <-
  sapply(1:nrow(post), function(i)
    sum(dnorm(d2$height, post$mu[i], post$sigma[i], log = TRUE)))

post$prod <- post$LL + dnorm(post$mu , 178 , 20 , TRUE) +
  dunif(post$sigma , 0 , 50 , TRUE)
post$prob <- exp(post$prod - max(post$prod))

contour_xyz(post$mu , post$sigma, post$prob)
image_xyz(post$mu , post$sigma , post$prob)

sample.rows <- sample(1:nrow(post),
                      size = 1e4,
                      replace = TRUE,
                      prob = post$prob)

sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot(
  sample.mu,
  sample.sigma,
  cex = 0.5,
  pch = 16,
  col = col.alpha(rangi2, 0.1)
)

dens(sample.mu)
dens(sample.sigma)

# Quadratic Approximation
m <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 20),
    sigma ~ dunif(0, 50)
  ),
  start = list(
    mu = mean(d2$height),
    sigma = sd(d2$height)
  ),
  data = d2
)

post <- extract.samples(m, n = 1e4)
head(post)
precis(post)

plot(d2$height, d2$weight)

# What is “regression”? Many diverse types of models are called “regression.” The term has come to mean using one or more predictor variables to model the distribution of one or more outcome variables. The original use of term, however, arose from anthropologist Francis Galton’s (1822–1911) observation that the sons of tall and short men tended to be more similar to the population mean, hence regression to the mean.

N <- 100
a <- rnorm(N, 178, 20)
b <- rlnorm(N, 0, 1)

plot(
  NULL ,
  xlim = range(d2$weight) ,
  ylim = c(-100, 400) ,
  xlab = "weight" ,
  ylab = "height"
)
abline(h = 0 , lty = 2)
abline(h = 272 , lty = 1 , lwd = 0.5)
mtext("b ~ dnorm(0,10)")
xbar <- mean(d2$weight)
for (i in 1:N)
  curve(
    a[i] + b[i] * (x - xbar),
    from = min(d2$weight),
    to = max(d2$weight),
    add = TRUE,
    col = col.alpha("black", 0.2)
  )

# Statistical models are machines for inference. Many machines
# will work, but some work better than others. Priors can be wrong, 
# but only in the same sense that a kind of hammer can be wrong for 
# building a table.

# Making choices tends to make novices nervous. There’s an illusion 
# sometimes that default procedures are more objective than procedures 
# that require user choice, such as choosing priors. If that’s true, then all
# “objective” means is that everyone does the same thing. It carries no 
# guarantees of realism or accuracy.

xbar <- mean(d2$weight)

m2 <- quap(alist(
  height ~ dnorm(mu , sigma) ,
  mu <- a + exp(log_b) * (weight - xbar),
  a ~ dnorm(178 , 100) ,
  log_b ~ dnorm(0 , 1) ,
  sigma ~ dunif(0 , 50)
  ),
  data = d2
)

plot(height ~ weight , data = d2 , col = rangi2)
post <- extract.samples(m2)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map * (x - xbar) , add = TRUE)
