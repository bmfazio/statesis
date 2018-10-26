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