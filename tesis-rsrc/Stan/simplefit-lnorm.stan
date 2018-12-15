// Recover parameters for a univariate logit normal-distributed outcome

data {
  int<lower=0> n;
  vector[n] X;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

transformed parameters {
  real p = inv_logit(mu);
}

model {
  logit(X) ~ normal(mu, sigma);
}
