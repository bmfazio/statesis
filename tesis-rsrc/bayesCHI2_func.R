# Inputs
  # Data
  # Posteriores

### Inicio de "la funcion"
# Carga de los resultados previos (data + posteriores)
origdata <- simu.betab.data_0.05_n1k_seed0
modelfit <- simu.binom.fit_simu.betab.data_0.05_n1k_seed0
origdata <- simu.betab.data_0.05_seed0
modelfit <- simu.binom.fit_simu.betab.data_0.05_seed0


# Func que extrae un sample de cada posterior
ipost <- function(postpar, i) {
  lapply(postpar, function(x)x[i, ])
}
# Func que recibe un post sample y crea los parametros necesarios segun la distribucion
iRB <- function(distr, data, ...) {
  # args: pars, n
  dddot <- list(...)
  if(distr == "binom") {
    dddot$pars$bx
    ddist <- function(y)
    pdist <- 
  } else if(distr == "betab") {
    
  } else if(distr == "eibin") {
    
  } else if(distr == "eibeb") {
    
  } else {
    stop("Undefined distribution")
  }
}

# Comenzar calculo RB, un posterior sample a la vez

# Inputs:
  # pmf, cdf
  # ^parametros (posterior)
  # data y
function(n, origdata, modelfit, ) {
  
y <- origdata$y
x <- origdata$x

allPars <- c("bx", "bz", "rho", "sigma")
fitPars <- attributes(modelfit)$model_pars
parpost <- extract(modelfit, intersect(allPars, fitPars))

lapply(1:nrow(parpost$bx),
       function(i){
         bx <- parpost$bx[i,]
       }
       )
mu <- invlogit(x%*%bx)

lapply(1:8, function(k) {
  pk <- sum(dbinom(k-1, n, prob = mu))
  ak <- pbinom(k-1.1, n, prob = mu)
  Fy <- pbinom(y, n, prob = mu)
  aK <- pbinom(k-1, n, prob = mu)
  mk <- sum(ifelse(ak <Fy&Fy<= aK, 1, 0))
  data.frame(pk = pk, mk = mk)
  }) %>% do.call(rbind, .) -> rbk

# Calculo de R^B para una muestra de la posterior
RB <- with(rbk, sum(((mk - pk)/sqrt(pk))**2))

quantile(rchisq(10**6, 7))
pchisq(RB, 7)
}