// Poisson regression model for bikemi data
//
  //  y[ij] ~ Poisson(mu[i]) and
//       log(mu[i]) = beta0 + beta1 * log(x[i]+10) + beta2 * x[i]
//
  // We also include predictions of counts at observed doses, to compare with observations
//
  data {
    int<lower=0> M; // number of viable edges
    int<lower=0> y[M,3]; // flows (first and second row identify the edge, third row is actual flow!)
    real <lower=0> d[263,263]; // distances
  }
parameters {
  real beta0;
  real beta1;
}

transformed parameters  {
  real lmu[M]; // linear predictor
  for (i in 1:M) {
    lmu[i] = beta0 + beta1*log(d[y[i,1],y[i,2]]+10); // linear predictor
  }
}

model {
  for (i in 1:M) {
    y[i,3] ~ poisson_log(lmu[i]);
  }
  beta0 ~ normal(0,10^2);
  beta1 ~ normal(0,10^2);
  
  
}

generated quantities {
  int<lower=0> ypred[M];// predictions at imputed m[i]'s
for (i in 1:M){
    ypred[i] = poisson_log_rng(lmu[i]);
  }
}
