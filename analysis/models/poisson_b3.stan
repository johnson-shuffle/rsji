data {
  int<lower=1> S;             // # of sectors
  int<lower=1> R;             // # of races
  int<lower=1> K;             // # of sector-race combinations
  int<lower=1> N;             // # of observations
  int<lower=0> y[N];          // # of stops
  int<lower=0> b[N];          // # of arrests
  matrix[K,4] X;              // population, income, pct black, pct college
  int<lower=0,upper=S> yS[N]; // sector for each observation
  int<lower=0,upper=R> yR[N]; // race for for each observation
  int<lower=0,upper=K> yC[N]; // sector-race for each observation
}
parameters {
  real mu;
  real rho;
  vector[R] raw_a;
  vector[S] raw_b;
  vector[N] raw_e;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma_e;
}
transformed parameters{
  vector[R] alpha;
  vector[S] beta;
  vector[N] epsilon;
  vector[N] lp;
  
  // reparameterize
  alpha = sigma_a * raw_a;
  beta  = sigma_b * raw_b;
  epsilon = sigma_e * raw_e;
  
  // linear predictor
  for (n in 1:N) {
    lp[n] = log(b[n]) + mu + alpha[yR[n]] + rho * X[yC[n],3] + beta[yS[n]];
  }
}
model {
  
  // priors
  //mu ~ normal(0, 1);
  //rho ~ normal(0, 1);
  raw_a ~ normal(0, 1);
  raw_b ~ normal(0, 1);
  raw_e ~ normal(0, 1);
  sigma_a ~ normal(0, 1);
  //sigma_b ~ normal(0, 1);
  //sigma_e ~ normal(0, 1);
  
  // likelihood
  y ~ poisson_log(lp + epsilon);
}
generated quantities {
}
