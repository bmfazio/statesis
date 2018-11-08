# Bayesian Chi^2

  # f_j (y | theta) -> density of the j-th observation
  # sum all f_j(y | theta) in the same bin k, then sum over all n
  # the sum over all k bins

  # mk = number of obs in the bin

set.seed(13)
ll <- -rexp(20)
y <- sample(0:3, 20, replace = T)
k <- lapply(0:3, function(x)which(y==x))
lapply(k,
       function(x){
         pk <- exp(ll[x])
         mk <- length(y[x])
         N <- length(y)
         sum(((mk - N*pk)/sqrt(N*pk))**2)
       })