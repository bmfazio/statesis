setwd("D:/gitrepos/tesis-pucpstat/tesis-rsrc/Stan")
library(cowplot)
library(ggplot2)
library(reshape)
library(plyr)
library(rstan)
library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# funcs
logit <- function(x)log(x/(1-x))
invlogit <- function(x)1/(1+exp(-x))
# Draw from reparametrized beta
rbeta.repar <- function(x, mu, rho) with(repar.beta(mu, rho),rbeta(x, alpha, beta))
# Reparametrized beta
repar.beta <- function(mu, rho) {
  if (all(0 < mu & mu < 1 & 0 < rho))
    return(list(alpha = mu / rho, beta = (1 - mu) / rho))
  stop("Parameters are out of bounds")
}
# Logit normal density
dlnor <- function(x, mu, s) {
  ((s*sqrt(2*pi))**-1)*((x*(1-x))**-1)*exp(-((logit(x)-mu)**2)/(2*s*s))
}

### a ver
# Modelos para fitear
model_beta <- stan_model("simplefit-beta.stan", model_name = "Simple beta fit")
model_lnor <- stan_model("simplefit-lnorm.stan", model_name = "Simple logitnormal fit")

# Funcion simuladora
simu_round <- function(N, p, rho){
  x_beta <- rbeta.repar(N, p, rho)
  
  fit_beta <- optimizing(model_beta, data = list(n = N, X = x_beta))
  fit_lnor <- optimizing(model_lnor, data = list(n = N, X = x_beta),
                         init = list(sigma = 1))
  
  pars <- c(fit_beta$par["aa"], fit_beta$par["bb"], fit_lnor$par["mu"], fit_lnor$par["sigma"])
  
  pvals <- rep(0, 1000)
  for(i in 1:1000){
    pvals[i] <-
      chisq.test(
      table(
        rbinom(1000, 7,
                 rbeta(100, fit_beta$par["aa"],
                       fit_beta$par["bb"])),
        rbinom(1000, 7,
               invlogit(rnorm(100, fit_lnor$par["mu"],
                              fit_lnor$par["sigma"])))))$p.value
  }

  
  list(
    pars = pars,
    pvals = pvals
  )
}


set.seed(1)
# LET'S GO SIMU
N <- 8000
p_range <- seq(from = 0.05, to = 0.5, by = 0.05)
s_range <- seq(from = 0.05, to = 0.5, by = 0.05)
ps_grid <- expand.grid(p_range, s_range)

pout <- matrix(ncol = 1000, nrow = nrow(ps_grid))
sout <- matrix(ncol = 2, nrow = nrow(ps_grid))
for(i in 1:nrow(ps_grid)) {
  cat(i)
  sink("NUL")
  tmp.res <- simu_round(N, ps_grid[i, 1], ps_grid[i, 2])
  pout[i, ] <- tmp.res$pvals
  sout[i, ] <- cbind(tmp.res$pars["mu"], tmp.res$pars["sigma"])
  sink()
}

sframe <- data.frame(p = ps_grid[, 1], rho = ps_grid[, 2],
                     mu = sout[, 1], sigma = sout[, 2])
pframe <- data.frame(p = ps_grid[, 1], rho = ps_grid[, 2],
                     signif = apply(pout, 1,
                                    function(x) sum(x<0.05)/length(x)))

saveRDS(list(sframe, pframe), file = "../REtestframes.rds")

# Plot discrepancias (GoF chi2) entre ajustes beta y lnorm
ggplot(pframe,
       aes(x = p, y = signif, color = rho, group = rho)) + 
  geom_point() + geom_line() +
  labs(x = "Beta mean",
       y = "% datasets detected",
       color = "Beta\ndispersion")
# La idea del plot es mostrar que la media de la beta no tiene un impacto sustancial en la identificacion del modelo y existe un buen match para un rango amplio de valores de dispersion

# Plot sigmas estimados en logit normal
ggplot(sframe,
       aes(x = p, y = sigma, color = rho, group = rho)) + 
  geom_point() + geom_line() +
  labs(x = "Beta mean",
       y = "Logit normal dispersion",
       color = "Beta\ndispersion")
# La idea del plot es mostrar que alrededor de un sigma estimado de 2 ya debería considerarse hacer un ajuste con betabinomial