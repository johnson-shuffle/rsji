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
  int<lower=0,upper=S> cS[K]; // sector for each combination
  int<lower=0,upper=R> cR[K]; // race for each combination
}
parameters {
  real mu;
  vector[R] raw_a;
  vector[S] raw_b;
  vector[N] raw_e;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma_e;
  real nu;
  vector[R] raw_d;
  vector[S] raw_z;
  vector[N] raw_w;
  real<lower=0> sigma_d;
  real<lower=0> sigma_z;
  real<lower=0> sigma_w;
}
transformed parameters{
  vector[R] alpha;
  vector[S] beta;
  vector[N] epsilon;
  vector[N] lp;
  vector[R] delta;
  vector[S] zeta;
  vector[N] omega;
  vector[K] log_theta;
  
  // reparameterize
  alpha = sigma_a * raw_a;
  beta  = sigma_b * raw_b;
  epsilon = sigma_e * raw_e;
  
  // reparameterize (level two)
  delta = sigma_d * raw_d;
  zeta  = sigma_z * raw_z;
  omega = sigma_w * raw_w;
  
  // linear predictor
  for (n in 1:N) {
    lp[n]  = mu + alpha[yR[n]] + beta[yS[n]];
  }
  
  // crime rate
  for (k in 1:K) {
    log_theta[k] = log(X[k,1]) + nu + delta[cR[k]] + zeta[cS[k]];
  }
}
model {
  
  // priors
  //mu ~ normal(0, 1);
  raw_a ~ normal(0, 1);
  raw_b ~ normal(0, 1);
  raw_e ~ normal(0, 1);
  //sigma_a ~ normal(0, 1);
  //sigma_b ~ normal(0, 1);
  //sigma_e ~ normal(0, 1);
  
  // priors (level two)
  //nu ~ normal(0, 1);
  raw_d ~ normal(0, 1);
  raw_z ~ normal(0, 1);
  raw_w ~ normal(0, 1);
  //sigma_d ~ normal(0, 1);
  //sigma_z ~ normal(0, 1);
  //sigma_w ~ normal(0, 1);
  
  // likelihood (level one)
  for (n in 1:N) {
    y[n] ~ poisson_log(log_theta[yC[n]] + lp[n] + epsilon[n]);
  }
  
  // likelihood (level two)
  for (n in 1:N) {
    b[n] ~ poisson_log(log_theta[yC[n]] + omega[n]);
  }
}
generated quantities {
  vector[K] theta;
  
  theta = exp(log_theta);
}
