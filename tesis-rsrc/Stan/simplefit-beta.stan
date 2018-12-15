// Recover parameters for a univariate beta-distributed outcome

data {
  int<lower=0> n;
  vector[n] X;
}

parameters {
  real mu;
  real<lower=0> rho;
}

transformed parameters {
  real p;
  real aa;
  real bb;
  
  p = inv_logit(mu);
  aa = p/rho;
  bb = (1-p)/rho;
}

model {
  X ~ beta(aa, bb);
}
