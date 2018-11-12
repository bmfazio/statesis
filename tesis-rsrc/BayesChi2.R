### estar interpretando mal mk

# define todo claramente

# Bayesian Chi^2

  # f_j (y | theta) -> density of the j-th observation
  # sum all f_j(y | theta) in the same bin k, then sum over all n
  # the sum over all k bins

  # mk = number of obs in the bin

# mira en simu_betab_plan para sacar data y
# mira en simu_fits_plan para sacar LL
# loadd(simu.betab.data_0.05_n1k_seed0)
# loadd(simu.binom.fit_simu.betab.data_0.05_n1k_seed0)
loadd(simu.betab.data_0.05_seed0)
loadd(simu.binom.fit_simu.betab.data_0.05_seed0)


### Inicio de "la funcion"
# Carga de los resultados previos (data + posteriores)
# origdata <- simu.betab.data_0.05_n1k_seed0
# modelfit <- simu.binom.fit_simu.betab.data_0.05_n1k_seed0
origdata <- simu.betab.data_0.05_seed0
modelfit <- simu.binom.fit_simu.betab.data_0.05_seed0

allPars <- c("bx", "bz", "rho", "sigma")
fitPars <- attributes(modelfit)$model_pars

parpost <- extract(modelfit, intersect(allPars, fitPars))

# Comenzar calculo RB, un posterior sample a la vez
i <- 999 # O sea esto iria de 1:1000 (o nrow de los bx post)

n <- 7
y <- origdata$y
x <- origdata$x

bx <- parpost$bx[i,]
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

pchisq(RB, 7)