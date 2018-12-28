// Poisson regression model for bikemi data
//
  //  y[ij] ~ Poisson(mu[i]) and
//       log(mu[i]) = beta0 + beta1 * log(x[i]+10) + beta2 * x[i]
//
  // We also include predictions of counts at observed doses, to compare with observations
//
  data {
    int<lower=0> M; // number of stations
    int<lower=0> y[M,M]; // flows
    real <lower=0> d[M,M]; // distances
  }
parameters {
  real beta0;
  real beta1;
}

transformed parameters  {
  real lmu[M,M]; // linear predictor
  for (i in 1:M) {
    for(j in 1:M)
      lmu[i,j] = beta0 + beta1*log(d[i,j]+10); // linear predictor
  }
}

model {
  for (i in 1:M) {
    for (j in 1:M) {
      y[i,j] ~ poisson_log(lmu[i,j]);
    }
  }
  beta0 ~ normal(0,10^3);
  beta1 ~ normal(-1,10^2);
}

generated quantities {
  int<lower=0> ypred[M,M];// predictions at imputed m[i]'s
for (i in 1:M){
  for(j in 1:M){
    ypred[i,j] = poisson_log_rng(lmu[i,j]);
  }
  }
}