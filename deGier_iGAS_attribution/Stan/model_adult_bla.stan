data {
  int<lower = 0> n;
  int<lower = 0> k_Trend_u;
  int<lower = 0> k_Trend_p;
  int<lower = 0> k_infA;
  int<lower = 0> k_lag;
  int iGAS_adult_bla[n];
  matrix[n, k_Trend_u] X_Trend_u;
  matrix[n, k_Trend_p] X_Trend_p;
  matrix[n, k_infA] X_infA;
  matrix[n, k_lag] Strep;
  matrix[n, k_lag] infA;
  matrix[n, k_lag] infB;
  matrix[n, k_lag] RSV;
  matrix[n, k_lag] hMPV;
  matrix[n, k_lag] SARSCoV2;
  vector[n] Coverage_isis;
  vector[k_lag] alpha;
}

parameters {
  real<lower = 0> phi;
  vector[k_Trend_u] beta_Trend;
  vector[k_Trend_p] b_Trend;
  real<lower = 0> sigma_b_Trend;
  real<lower = 0> beta_Strep;
  vector<lower = 0>[k_infA] beta_infA;
  real<lower = 0> beta_infB;
  real<lower = 0> beta_RSV;
  real<lower = 0> beta_hMPV;
  real<lower = 0> beta_SARSCoV2;
  simplex[k_lag] w_Strep;
  simplex[k_lag] w_infA;
  simplex[k_lag] w_infB;
  simplex[k_lag] w_RSV;
  simplex[k_lag] w_hMPV;
  simplex[k_lag] w_SARSCoV2;
}

transformed parameters {
  vector[n] mu_Trend;
  vector[n] mu_Strep;
  vector[n] mu_infA;
  vector[n] mu_infB;
  vector[n] mu_RSV;
  vector[n] mu_hMPV;
  vector[n] mu_SARSCoV2;
  vector[n] mu_iGAS;
  mu_Trend = exp(X_Trend_u * beta_Trend + X_Trend_p * b_Trend);
  mu_Strep = beta_Strep * (Strep * w_Strep);
  mu_infA = (X_infA*beta_infA) .* (infA * w_infA);
  mu_infB = beta_infB * (infB * w_infB);
  mu_RSV = beta_RSV * (RSV * w_RSV);
  mu_hMPV = beta_hMPV * (hMPV * w_hMPV);
  mu_SARSCoV2 = beta_SARSCoV2 * (SARSCoV2 * w_SARSCoV2);
  mu_iGAS = Coverage_isis .* (mu_Trend + mu_Strep + mu_RSV + mu_infA + mu_infB + mu_hMPV + mu_SARSCoV2);
}

model {
  phi ~ exponential(0.1) ;
  beta_Trend ~ std_normal();
  beta_Strep ~ std_normal();
  beta_infA ~ std_normal();
  beta_infB ~ std_normal();
  beta_RSV ~ std_normal();
  beta_hMPV ~ std_normal();
  beta_SARSCoV2 ~ std_normal();
  b_Trend ~ normal(0, sigma_b_Trend);
  sigma_b_Trend ~ std_normal();
  w_Strep ~ dirichlet(alpha);
  w_infA ~ dirichlet(alpha);
  w_infB ~ dirichlet(alpha);
  w_RSV ~ dirichlet(alpha);
  w_hMPV ~ dirichlet(alpha);
  w_SARSCoV2 ~ dirichlet(alpha);
  iGAS_adult_bla ~ neg_binomial_2(mu_iGAS, phi);
}
