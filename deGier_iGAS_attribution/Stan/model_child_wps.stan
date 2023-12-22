data {
  int<lower = 0> n;
  int<lower = 0> k_Trend_u;
  int<lower = 0> k_Trend_p;
  int<lower = 0> k_lag;
  int iGAS_child_wps[n];
  matrix[n, k_Trend_u] X_Trend_u;
  matrix[n, k_Trend_p] X_Trend_p;
  matrix[n, k_lag] Strep;
  matrix[n, k_lag] VZV;
  vector[n] Coverage_isis;
  vector[k_lag] alpha;
}

parameters {
  real<lower = 0> phi;
  vector[k_Trend_u] beta_Trend;
  vector[k_Trend_p] b_Trend;
  real<lower = 0> sigma_b_Trend;
  real<lower = 0> beta_Strep;
  real<lower = 0> beta_VZV;
  simplex[k_lag] w_Strep;
  simplex[k_lag] w_VZV;
}

transformed parameters {
  vector[n] mu_Trend;
  vector[n] mu_Strep;
  vector[n] mu_VZV;
  vector[n] mu_iGAS;
  mu_Trend = exp(X_Trend_u * beta_Trend + X_Trend_p * b_Trend);
  mu_Strep = beta_Strep * (Strep * w_Strep);
  mu_VZV = beta_VZV * (VZV * w_VZV);
  mu_iGAS = Coverage_isis .* (mu_Trend + mu_Strep + mu_VZV);
}

model {
  phi ~ exponential(0.1) ;
  beta_Trend ~ std_normal();
  beta_Strep ~ std_normal();
  beta_VZV ~ std_normal();
  b_Trend ~ normal(0, sigma_b_Trend);
  sigma_b_Trend ~ std_normal();
  w_Strep ~ dirichlet(alpha);
  w_VZV ~ dirichlet(alpha);
  iGAS_child_wps ~ neg_binomial_2(mu_iGAS, phi);
}
