// EIBB model - Center vs Endpoint parametrization
data {
  int<lower=1> N; // sample size (data.frame rows)
  int<lower=1> Kx; // number of covariates for beta mean
  int<lower=1> Kz; // number of covariates for mixture proportions
  
  int<lower=1> n[N]; // # of attempts (binomial parameter)
  int<lower=0> y[N]; // # of successes (outcome)
  
  matrix[N, Kx] x; // covariate matrix for beta mean
  matrix[N, Kz] z; // covariate matrix for mixture proportions
}

transformed data{
  real ymin[N];
  real ymax[N];
  
  for (i in 1:N) {
    ymin[i] = 1 - min([ 1, y[i] ]);
    ymax[i] = 1 - min([ 1, n[i]-y[i] ]);
  }
}

parameters {
  vector[Kx] bx; // coeffs for beta mean
  vector[Kz] bz; // coeffs for inflated mean

  real P; // inflation proportion (to be logit transformed)
}

model {
  real mu_beta;
  real mu_infl;
  real litP;
  vector[3] p;
  
  bx ~ normal(0, 5);
  bz ~ normal(0, 5);
  P ~ normal(0, 5);
  
  litP = inv_logit(P);
  
  for (i in 1:N) {
    mu_beta = inv_logit(x[i]*bx);
    mu_infl = inv_logit(z[i]*bz);
    p = [litP*(1-mu_infl), (1-litP), litP*mu_infl]';
    
    target +=
    log(
      ymin[i]*p[1] + ymax[i]*p[3] + p[2]*exp( binomial_lpmf( y[i] | n[i], mu_beta ) )
      );
  }
}

generated quantities {
  vector[N] log_lik;
  vector[3] p;
  real litP;
  real mu_infl;
  
  litP = inv_logit(P);
  
  for (i in 1:N) {
    mu_infl = inv_logit(z[i]*bz);
    p = [litP*(1-mu_infl), (1-litP), litP*mu_infl]';
    
    log_lik[i] =
    log(
      ymin[i]*p[1] + ymax[i]*p[3] + p[2]*exp( binomial_lpmf( y[i] | n[i], inv_logit(x[i]*bx) ) )
      );
  }
}
