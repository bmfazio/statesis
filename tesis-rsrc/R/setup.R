# Libraries
library(drake)
library(tidyverse)
library(bayesplot)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# Inverse logistic
invlogit <- function(x)(1/(1+exp(-x)))

# Softmax
softmax <- function(x)exp(x)/sum(exp(x))

# Beta distribution mean-dispersion reparametrizer
  # mu, rho -> shape1, shape2 (rbeta named parameters)
    # 0 < mu < 1
    # 0 < rho
repar.beta <- function(mu, rho) {
  if (all(0 < mu & mu < 1 & 0 < rho)) {
    list(alpha = mu / rho,
         beta = (1 - mu) / rho)
  } else {
    stop("Parameters are out of bounds")
  }
}

# Draw from reparametrized beta
rbeta.repar <- function(n, mu, rho) {
    with(repar.beta(mu, rho),
       rbeta(n, alpha, beta))
}

# Draw from beta-binomial (uses reparametrized beta)
rbbin <- function(n = 1000,  # samples
                  tries = 10,# binomial attempts
                  mu = 0.5, rho = 1) {
  rbinom(n, tries, rbeta.repar(n, mu, rho))
}

# Label variables have a 'labels' attribute
putlabel <- function(x) {
  if(is.null(attr(x, "labels"))) {
    stop("No 'labels' attribute")
  } else {
    return(factor(x, levels = attr(x, "labels"), labels = names(attr(x, "labels"))))
  }
}

# Cumulative normal-based probability vector
cumu.norm <- function(mu, s){
  c(
    pnorm( (0 - mu)/s ),
    pnorm( (1 - mu)/s ) - pnorm( (0 - mu)/s ),
    1 - pnorm( (1 - mu)/s )
  ) %>% matrix(ncol = 3)
}

# Simulate data for EIBB regression model estimation
eibb.sim <- function(N,      # Number of observations
                     n,      # Binomial size
                     bx, bz, # Coefficients (beta mean, latent normal mean)
                     rho, s, # Dispersion (beta, normal)
                     sx, sz, # Dispersion for covariate generation
                     seed=1, # Random seed
                     fullinfo = FALSE){
  set.seed(seed)
  
    # Covariate generation
  Kx <- length(bx) - 1
  x <- cbind(rep(1, N),
             sapply(rep(N, Kx), rnorm, sd = sx)) %>% unlist %>% matrix(nrow = Kx) %>% t
  Kz <- length(bz) - 1
  z <- cbind(rep(1, N),
             sapply(rep(N, Kz), rnorm, sd = sz)) %>% unlist %>% matrix(nrow = Kz) %>% t
  
    # Linear predictors
  mu.beta <- invlogit(x %*% bx)
  mu.norm <- z %*% bz
    # Mixture probabilities
  p <- cumu.norm(mu.norm, s)

    # Latent mixture component selector
  Z <- apply(p, 1, function(prob.vector) sample(1:3, size = 1, prob = prob.vector))

  y <- ifelse(Z == 1, 0,
              ifelse(Z == 2, rbbin(N, n, mu.beta, rho), 
                     ifelse(Z == 3, n, NA))
              )

  if (fullinfo) {
    list(
      x = x,
      z = z,
      y = y,
      p = p,
      Z = Z,
      mu.norm = mu.norm,
      mu.beta = mu.beta) %>% return
  } else {
    list(x = x,
         z = z,
         y = y) %>% return
  }
}

eibb.sim(N = 10, n = 100, bx = c(0, 1, -1), bz = c(0.5,1,2), rho = 0.1, s = 0.1, sx = 0.3, sz = 0.3, seed = 1, fullinfo = F)