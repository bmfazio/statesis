library(drake)
library(rstan)
library(dplyr)

# Extraer parametros y repetir iRB para cada posterior
postRB <- function(modelfit, origdata, modeltype, debug=F) {
  # Lista de parametros de interes
  allPars <- c("bx", "bz", "rho", "sigma")
  fitPars <- attributes(modelfit)$model_pars
  # Extraer solo parametros de interes del fit
  parpost <- extract(modelfit, intersect(allPars, fitPars))
  # Calcular estadistico RB para cada muestra posterior
  sapply(1:1000,function(i)iRB(modeltype, origdata, ipost(parpost, i), debug)) 
}


# Extraer la muestra i de la distribucion posterior
ipost <- function(postpar, i) {
  lapply(postpar, function(x)as.matrix(x)[i, ])
}

# Calcular estadistico RB para una sola muestra posterior
iRB <- function(distr, data, pars, debug) {
  # Crear los parametros necesarios (segun la distribucion) para evaluar pmf/cdf con una muestra posterior
  n <- data$n
  y <- data$y
  x <- data$x
  bx<- pars$bx
  N <- length(y)
  mu<- invlogit(x%*%bx)
  if(distr == "binom") {
    ddist <- function(U) dbinom(U, n, mu)
    pdist <- function(U) pbinom(U, n, mu)
  } else if(distr == "betab") {
    rho <- pars$rho
    ddist <- function(U) rmutil::dbetabinom(U, n, mu, 1/rho)
    pdist <- function(U) ifelse(U<0, 0, rmutil::pbetabinom(U, n, mu, 1/rho))
  } else if(distr == "eibin") {
    p <- cumu.norm(pars$z%*%pars$bz, pars$sigma)
    ddist <- function(U) deibi(U, mu, p[1], p[2], p[3], n)
    pdist <- function(U) peibi(U, mu, p[1], p[2], p[3], n)
  } else if(distr == "eibeb") {
    p <- cumu.norm(pars$z%*%pars$bz, pars$sigma)
    ddist <- function(U) deibb(U, mu, p[1], p[2], p[3], n)
    pdist <- function(U) peibb(U, mu, p[1], p[2], p[3], n)
  } else {
    stop("Undefined distribution")
  }
  
  # Calcular RB
  # rbk <- lapply(1:8, function(k) {
  #   print(length(ddist(k-1)))
  #   pk <- sum(ddist(k-1))
  #   ak <- pdist(k-1.1)
  #   Fy <- pdist(y)
  #   aK <- pdist(k-1)
  #   mk <- sum(ifelse(ak <Fy&Fy<= aK, 1, 0))
  #   data.frame(pk = pk, mk = mk)
  #   }) %>% do.call(rbind, .)
  
  rbk <- lapply(1:8, function(k) {
    pk <- sum(ddist(k-1))
    ak <- pdist(k-1.1)
    Fy <- pdist(y)
    aK <- pdist(k-1)
    mk <- sum(ifelse(ak <Fy&Fy<= aK, 1, 0))
    if(debug){
      print("ROUND")
      print(k)
      print("ak")
      print(round(c(ak),2))
      print("Fy")
      print(round(c(Fy),2))
      print("aK")
      print(round(c(aK),2))
      print("mk")
      print(round(c(mk),2))      
    }
    data.frame(pk = pk, mk = mk)
    }) %>% do.call(rbind, .)
  
  if(debug)print(rbk)
  
  with(rbk,
       sum(
         (
           (mk - pk)/sqrt(pk)
         )**2
         ))
}

# Extras
# Endpoint-inflated binomial pmf/cdf
deibi <- function(y, mu, p1, p2, p3, n) {
  if(p1+p2+p3!=1){stop("p elements must sum to 1")}
  p1*ifelse(y==0, 1, 0) +
    p2*dbinom(y, n, mu) +
    p3*ifelse(y==n, 1, 0)
}

peibi <- function(y, mu, p1, p2, p3, n) {
  pacote <- data.frame(y, mu, p1, p2, p3, n)
  apply(pacote, 1,
        function(x) sum(
          deibi(0:x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]])
          ))
}
# Endpoint-inflated beta-binomial pmf/cdf
deibb <- function(y, mu, rho, p1, p2, p3, n) {
  if(p1+p2+p3!=1){stop("p elements must sum to 1")}
  p1*ifelse(y==0, 1, 0) +
    p2*rmutil::dbetabinom(y, n, mu, 1/rho) +
    p3*ifelse(y==n, 1, 0)
}

peibb <- function(y, mu, rho, p1, p2, p3, n) {
  pacote <- data.frame(y, mu, rho, p1, p2, p3, n)
  apply(pacote, 1,
        function(x) sum(
          deibb(0:x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]])
          ))
}

# Inverse logistic
invlogit <- function(x)(1/(1+exp(-x)))

# Softmax
softmax <- function(x)exp(x)/sum(exp(x))

# Simular binomial
binom.sim <- function(N, n, bx = 0) {
  
  # Covariate generation
  Kx <- length(bx)
  x <- cbind(rep(1, N),
             sapply(rep(N, Kx - 1), rnorm)) %>% unlist %>% matrix(ncol = Kx)
  
  # Simular
  p <- invlogit(x %*% bx)
  y <- rbinom(N, n, p)

  list(N = N, Kx = Kx, Kz = 1, z = t(t(rep(1,N))),
       n = if(length(n)==1){rep(n,N)}else{n},
       y = y,
       x = x) %>% return
}