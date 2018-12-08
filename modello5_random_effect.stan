// Poisson regression model for salmonella data with random effects
//
  //  y[ij] ~ Poisson(mu[ij]) and
//       log(mu[ij]) = beta0 + beta1 * log(x[i]+10) + beta2 * x[i] + b[ij]
//
  // with b[ij] ~ N(0,tau^2) and tau ~ U(0,10)
//
  // We also include predictions of counts at observed doses, to compare with observations
//
  data {
    int<lower=0> M; // number of stations
    int<lower=0> y[M,M,4]; // flows
    real <lower=0> d[M,M]; // distances
    int<lower=0> S[M];//sourceità
    int<lower=0> T[M];//targettosità
  }
parameters {
  real beta0;
  real beta1;
  real beta2;
  real beta3;
  real b[M,M,4];
  real<lower=0,upper=10> tau;
}

transformed parameters  {
  real lmu[M,M]; // linear predictor
  real logmean[M,M,4]; // log of mean of y[i,j]
   for (i in 1:M) {
    for(j in 1:M){
      lmu[i,j] = beta0 + beta1*log(d[i,j]+10) + beta2*log(S[i]) + beta3*log(T[j]); // linear predictor

        for(k in 1:4){  
          logmean[i,j,k] = lmu[i,j] + b[i,j,k];
        }
    }
  }
}

model {
  for (i in 1:M) {
    for (j in 1:M) {
//siamo arrivati qui a corregere il file      
      y[i,j] ~ poisson_log(logmean[i,j]);
      b[i,j,k] ~ normal(0,tau);
    }
  }
  beta0 ~ normal(0,10^3);
  beta1 ~ normal(0,10^3);
  beta2 ~ normal(0,10^3);
  tau ~ uniform(0,10);
}

generated quantities {
  int<lower=0> ypred[I]; // predictions at observed doses
  real pred_re[I]; // predicted random effects
  real mean_pred[I]; // prediction means
  for (i in 1:I) {
    pred_re[i] = normal_rng(0,tau);
    mean_pred[i] = lp[i] + pred_re[i];
    ypred[i] = poisson_log_rng(mean_pred[i]);
  }
}
