data {
  int<lower=1> N;               // Country-years
  int<lower=2> J;               // # Manifest variables
  real<lower=-999> manifest[N,J]; // Data
  real a[J];                    // IG prior on Psi diagonal elements
  real b[J];                    // IG prior on Psi diagonal elements
}

parameters {
  real<lower=0> Psi_diag[J];    // Var-cov diagonal
  real phi[N];                  // Latent trait
  real<lower=0.1> Lambda1;     // First constrained loading
  real Lambda2p[J-1];           // Remaining parameters
}

transformed parameters {         // Roundabout way to constrain L1>0
  real Lambda[J];
  Lambda[1] = Lambda1;
  for (i in 2:J)
    Lambda[i] = Lambda2p[i-1];
}

model {
  for (i in 1:N)
    phi[i] ~ normal(0, 1);
  Lambda[1] ~ normal(0,10)T[0,];
  for (j in 2:J)
    Lambda[j] ~ normal(0, 10);
  for (j in 1:J) {
    Psi_diag[j] ~ inv_gamma(a[j], b[j]);
    for (i in 1:N) if (manifest[i,j] != -999) {
      manifest[i,j] ~ normal(Lambda[j] * phi[i], Psi_diag[j]);
    }
  }
}
