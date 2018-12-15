# simu_testing.R
# Probar que rango de valores de inflacion para EIBB es razonable
source("R/setup.R")
## Dispersion de latente normal para
c(
  0.21492917,
  0.24345723,
  0.25510673,
  0.30397842,
  0.3901521,
  0.4346506,
  0.4824237,
  0.5940915,
  0.7413011,
  0.95346959) -> sigmas
# cumu.norm(0.5,0.25510673)

set.seed(1)
eibb.sim(N = 10**2, n = 7, bx = c(0, 0.5, -0.5), bz = 0.5, s = sigmas[4]) -> pob1

barplot(prop.table(table(pob1$y)), ylim = c(0,0.3))
barplot(prop.table(table(pob2$y)), ylim = c(0,0.3))
barplot(prop.table(table(pob3$y)), ylim = c(0,0.3))

## Fitting time :d
loadd(eibin.model)
loadd(binom.model)
sampling(eibin.model, data = pob1, chains = 1, iter = 2000,
         control = list(adapt_delta = 0.9, max_treedepth = 15)) -> fit1
sampling(binom.model, data = pob1, chains = 1, iter = 2000,
         control = list(adapt_delta = 0.9, max_treedepth = 15)) -> fit0
# sampling(eibin.model, data = pob6, chains = 1, iter = 2000,
#          control = list(adapt_delta = 0.9, max_treedepth = 15)) -> fit6

loo(fit0)
loo(fit1)