data {
  int<lower=0> J;     // Coders
  int<lower=0> N;     // unique country-dates
  int<lower=0> C;     // countries
  int<lower=1> n_obs; //observations
  int<lower=1, upper=C> cdata_id[J];            // j country indices
  int<lower=0, upper=1> wdata[n_obs];           // data
  int<lower=1, upper=J> rater_id[n_obs];        // coder_id index
  int<lower=1, upper=N> country_date_id[n_obs]; // country-date rds
  real gsigmasq;  // rater-level gamma variance around country-level gammas
  real gsigmasqc; // country-level gamma variance around world gammas
  vector[N] mc;   // N length  
}

parameters {
  vector[N] Z_star;
  vector[J] gamma;
  real gamma_mu;         // world-level cutpoints
  vector[C] gamma_c;     // country-level cuts, rows are countries
  real<lower=0> beta[J]; // reliability
}

transformed parameters {
  vector[N] Z;
  Z = mc + Z_star;
}

model {
  real p;

  Z_star ~ normal(0, 1);
  gamma_mu ~ uniform(-4, 4);

  for (j in 1:J)
    beta[j] ~ normal(1, 1)T[0,]; // truncated normal reliability

  for (c in 1:C)
    gamma_c[c] ~ normal(gamma_mu, gsigmasqc);

  for (j in 1:J)
    gamma[j] ~ normal(gamma_c[cdata_id[j]], gsigmasq);

  for (obs in 1:n_obs) {
    p = Phi_approx(gamma[rater_id[obs]] + Z[country_date_id[obs]] * beta[rater_id[obs]]);
    wdata[obs] ~ bernoulli(p);
  }
}
