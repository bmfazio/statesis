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

ll <- extract(simu.binom.fit_simu.betab.data_0.05_n1k_seed0, "log_lik")$log_lik 
k <- lapply(0:7, function(x)which(simu.betab.data_0.05_n1k_seed0$y==x))

# Calculo de R^B para una muestra de la posterior
RB <- function(ll_samp, k_which){
  do.call(sum,
    lapply(k_which,
           function(x){
             pk <- sum(exp(ll_samp[x]))
             mk <- length(k_which)
             sum(((mk - pk)/sqrt(pk))**2)
           })
  )
}

rb_post <- unlist(lapply(1:nrow(ll), function(x){RB(ll[x,], k)}))



# chi2 with K-1 dof

###

# Tratando de interpretar Bayesian chisq:

# n = number of observed data points
# 

# pk(theta) = (1/n) * SUM_j=1^n SUM_y==k f_j(y|theta)
