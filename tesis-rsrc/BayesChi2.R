### estar interpretando mal mk

# define todo claramente

# Bayesian Chi^2

  # f_j (y | theta) -> density of the j-th observation
  # sum all f_j(y | theta) in the same bin k, then sum over all n
  # the sum over all k bins

  # mk = number of obs in the bin

# mira en simu_betab_plan para sacar data y
# mira en simu_fits_plan para sacar LL
loadd(simu.betab.data_0.05_n1k_seed0)
loadd(simu.binom.fit_simu.betab.data_0.05_n1k_seed0)

origdata <- simu.betab.data_0.05_n1k_seed0
modelfit <- simu.binom.fit_simu.betab.data_0.05_n1k_seed0

allPars <- c("bx", "bz", "rho", "sigma")
fitPars <- attributes(modelfit)$model_pars

parpost <- extract(modelfit, intersect(allPars, fitPars))

# Comenzar calculo RB, un posterior sample a la vez
i <- 1

n <- 7
y <- origdata$y
x <- origdata$x

bx <- parpost$bx[i,]
mu <- invlogit(x%*%bx)

# para mi caso k = y
# pk calculeshon
kquants <- (0:8)*1/8
cdf <- pbinom(y, n, mu)

# seguro que esta bien v ? piensa como afecta la observacion individual
pk <- sapply(1:8, function(k) sum(dbinom(k-1, n, prob = mu))/N)
Pk <- c(0,cumsum(pk))

lapply(1:8, function(k) {
  mk <- sum(ifelse(cumsum(pk[1:k]) <cdf&cdf<= kquants[k+1], 1, 0))
  data.frame(pk = pk, mk = mk)
  }) %>% do.call(rbind, .) -> rbk

# Calculo de R^B para una muestra de la posterior
RB <- with(rbk, sum(((mk - N*pk)/sqrt(N*pk))**2))

# esto se ve correcto?
hist((a[a$y == 5,]$cdf))
barplot(table(y))
hist(cdf)
