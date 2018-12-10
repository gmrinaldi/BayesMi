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
    int<lower=0> S[M];//sourceità
    int<lower=0> T[M];//targettosità
    int<lower=0,upper=1> CC[M,M];
    int<lower=0,upper=1> PC[M,M];
    int<lower=0,upper=1> CP[M,M];
    int<lower=0,upper=1> Auto[M,M];
  }
parameters {
  real beta0;
  real beta1;
  real beta2;
  real beta3;
  real beta4;
  real beta5;
  real beta6;
  real beta7;
}

transformed parameters  {
  real lmu[M,M]; // linear predictor
  for (i in 1:M) {
    for(j in 1:M)
      lmu[i,j] = beta0 + beta1*log(d[i,j]+10) + beta2*log(S[i]) + beta3*log(T[j])+beta4*CC[i,j]+beta5*CP[i,j]+beta6*PC[i,j]+beta7*Auto[i,j]; // linear predictor
  }
}

model {
  for (i in 1:M) {
    for (j in 1:M) {
      y[i,j] ~ poisson_log(lmu[i,j]);
    }
  }
  beta0 ~ normal(0,10^3);
  beta1 ~ normal(0,10^3);
  beta2 ~ normal(0,10^3);
  beta3 ~ normal(0,10^3);
  beta4 ~ normal(0,10^3);
  beta5 ~ normal(0,10^3);
  beta6 ~ normal(0,10^3);
  beta7 ~ normal(0,10^3);

}

generated quantities {
  int<lower=0> ypred[M,M];// predictions at imputed m[i]'s
for (i in 1:M){
  for(j in 1:M){
    ypred[i,j] = poisson_log_rng(lmu[i,j]);
  }
  }
}