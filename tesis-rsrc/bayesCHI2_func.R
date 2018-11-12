# Inputs
  # Data
  # Posteriores

### Inicio de "la funcion"
# Carga de los resultados previos (data + posteriores)
# loadd(simu.betab.data_0.05_n1k_seed0)
# loadd(simu.betab.fit_simu.betab.data_0.05_n1k_seed0)
# origdata <- simu.betab.data_0.05_n1k_seed0
# modelfit <- simu.betab.fit_simu.betab.data_0.05_n1k_seed0
# loadd(simu.betab.data_0.05_seed0)
# loadd(simu.binom.fit_simu.betab.data_0.05_seed0)
# origdata <- simu.betab.data_0.05_seed0
# modelfit <- simu.binom.fit_simu.betab.data_0.05_seed0
loadd(simu.betab.data_0.2_seed0)
loadd(simu.betab.fit_simu.betab.data_0.2_seed0)
origdata <- simu.betab.data_0.2_seed0
modelfit <- simu.betab.fit_simu.betab.data_0.2_seed0


# Func que extrae un sample de cada posterior
allPars <- c("bx", "bz", "rho", "sigma")
fitPars <- attributes(modelfit)$model_pars
parpost <- extract(modelfit, intersect(allPars, fitPars))

ipost <- function(postpar, i) {
  lapply(postpar, function(x)as.matrix(x)[i, ])
}
# Func que recibe un post sample y crea los parametros necesarios segun la distribucion
iRB <- function(distr, data, pars) {
  # args: pars, n
  n <- data$n
  y <- data$y
  x <- data$x
  bx<- pars$bx
  mu<- invlogit(x%*%bx)
  if(distr == "binom") {
    ddist <- function(U) dbinom(U, n, mu)
    pdist <- function(U) pbinom(U, n, mu)
  } else if(distr == "betab") {
    rho <- pars$rho
    ddist <- function(U) rmutil::dbetabinom(U, n, mu, 1/rho)
    pdist <- function(U) ifelse(U<0, 0, rmutil::pbetabinom(U, n, mu, 1/rho))
  } else if(distr == "eibin") {
    p <- cumu.norm(data$z%*%data$bz, data$sigma)
    ddist <- function(U) deibi(U, mu, p[1], p[2], p[3], n)
    pdist <- function(U) peibi(U, mu, p[1], p[2], p[3], n)
  } else if(distr == "eibeb") {
    p <- cumu.norm(data$z%*%data$bz, data$sigma)
    ddist <- function(U) deibb(U, mu, p[1], p[2], p[3], n)
    pdist <- function(U) peibb(U, mu, p[1], p[2], p[3], n)
  } else {
    stop("Undefined distribution")
  }
  
  # Llamar a RB con ddist y pdist
  rbk <- lapply(1:8, function(k) {
    pk <- sum(ddist(k-1))
    ak <- pdist(k-1.1)
    Fy <- pdist(y)
    aK <- pdist(k-1)
    mk <- sum(ifelse(ak <Fy&Fy<= aK, 1, 0))
    data.frame(pk = pk, mk = mk)
    }) %>% do.call(rbind, .)
  
  with(rbk, sum(((mk - pk)/sqrt(pk))**2))
}

# Comenzar calculo RB, un posterior sample a la vez
# origdata <- simu.betab.data_0.05_n1k_seed0
# modelfit <- simu.binom.fit_simu.betab.data_0.05_n1k_seed0
# origdata <- simu.betab.data_0.05_seed0
# modelfit <- simu.binom.fit_simu.betab.data_0.05_seed0

sapply(1:1000,function(i)iRB("betab", origdata, ipost(parpost, i))) -> a

quantile(a)
quantile(pchisq(a, 7))