# Libraries
library(drake)
library(tidyverse)
library(bayesplot)
library(rstan)
library(loo)
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
                     bx = 0, bz = 0.5,    # Coefficients (beta mean, latent normal mean) - default: centered intercept
                     rho = 0.0001, s = 0, # Dispersion (beta, normal) - default: no beta overdispersion/endpoint inflation
                     sx = 0.5, sz = 0.3,  # Dispersion for covariate generation
                     seed = 1, # Random seed
                     fullinfo = FALSE){
  set.seed(seed)
  
    # Covariate generation
  Kx <- length(bx)
  x <- cbind(rep(1, N),
             sapply(rep(N, Kx - 1), rnorm, sd = sx)) %>% unlist %>% matrix(ncol = Kx)
  Kz <- length(bz)
  z <- cbind(rep(1, N),
             sapply(rep(N, Kz - 1), rnorm, sd = sz)) %>% unlist %>% matrix(ncol = Kz)
  
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
    list(N = N, Kx = Kx, Kz = Kz,
         n = ifelse(length(n) == 1, rep(n, N), n),
         y = y,
         x = x, z = z,
         p = p,
         Z = Z,
         mu.norm = mu.norm,
         mu.beta = mu.beta) %>% return
  } else {
    list(N = N, Kx = Kx, Kz = Kz,
         n = if(length(n)==1){rep(n,N)}else{n},
         y = y,
         x = x, z = z) %>% return
  }
}

tab_looic_divergent <- function(fit){
  data.frame(model = attr(fit, "model_name"),
             N = attr(fit, "sim")$dims_oi$log_lik,
             looic = loo(fit)$estimates[3,1],
             waic = waic(extract(fit, "log_lik")$log_lik)$estimates[3,1],
             divergent = get_num_divergent(fit))
}

gather_plan <- function (plan = NULL, target = "target", gather = "list") {
    command <- paste(plan$target, "=", plan$command)
    command <- paste(command, collapse = ", ")
    command <- paste0(gather, "(", command, ")")
    tibble(target = target, command = command)
}
