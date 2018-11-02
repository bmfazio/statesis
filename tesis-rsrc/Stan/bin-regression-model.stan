// Mixtura de dos fronteras constantes con una binomial
// Adaptado por Boris Fazio de stan-reference-2.17.0 p194

data {
  int<lower=1> N; // sample size (data.frame rows)
  int<lower=1> Kx; // number of covariates for beta mean
  int<lower=1> Kz; // number of covariates for mixture proportions
  
  int<lower=1> n[N]; // # of attempts (binomial parameter)
  int<lower=0> y[N]; // # of successes (outcome)
  
  matrix[N, Kx] x; // covariate matrix for beta mean
  matrix[N, Kz] z; // covariate matrix for mixture proportions
}

parameters {
  vector[Kx] bx; // coeffs for beta mean
}

model {
  real mu_beta;

  for (i in 1:N) {
    mu_beta = inv_logit(x[i]*bx);

    target +=
    log(
      exp( binomial_lpmf(y[i] | n[i], mu_beta) )
      );
  }
}
