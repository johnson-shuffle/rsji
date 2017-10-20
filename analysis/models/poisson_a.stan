data {
  int<lower=1> S;             // # of sectors
  int<lower=1> R;             // # of races
  int<lower=1> N;             // # of observations (= S * R)
  int<lower=0> y[N];          // counts (# of stops)
  int<lower=0> b[N];          // baselines (# of offenders + arrests)
  int<lower=0,upper=S> yS[N]; // sector for each observation
  int<lower=0,upper=R> yR[N]; // race for for each observation
}
parameters {
  real mu;
  vector[R] alpha_raw;
  vector[S] beta_raw;
  vector[N] epsilon_raw;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma_e;
}
transformed parameters{
  vector[R] alpha;
  vector[S] beta;
  vector[N] epsilon;
  real lp[N];
  
  // reparameterize
  alpha = sigma_a * alpha_raw;
  beta  = sigma_b * beta_raw;
  epsilon = sigma_e * epsilon_raw;
  
  // linear predictor
  for (n in 1:N) {
    lp[n] = mu + alpha[yR[n]] + beta[yS[n]] + epsilon[n] + log(b[n]);
  }
}
model {
  
  // priors
  mu ~ normal(0, 1);
  alpha_raw ~ normal(0, 1);
  beta_raw  ~ normal(0, 1);
  epsilon_raw ~ normal(0, 1);
  sigma_a ~ normal(0, 1);
  sigma_b ~ normal(0, 1);
  sigma_e ~ normal(0, 1);
  
  // likelihood
  y ~ poisson(exp(lp));
}
generated quantities {
  real mu_adj;
  vector[R] alpha_adj;
  
  // constrain alpha to sum to zero
  mu_adj = mu + mean(alpha);
  alpha_adj = alpha - mean(alpha);
}
