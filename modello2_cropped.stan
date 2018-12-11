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
  int<lower=0> S[263];
  int<lower=0> T[263];
}
parameters {
  real beta0;
  real beta1;
  real beta2;
  real beta3;
}

transformed parameters  {
  real lmu[M,3]; // linear predictor
  for (i in 1:M) {
      lmu[i,1] = y[i,1];
      lmu[i,2] = y[i,2];
      lmu[i,3] = beta0 + beta1*log(d[y[i,1],y[i,2]]+10)+ beta2*log(S[y[i,1]]) + beta3*log(T[y[i,2]]); // linear predictor
    }
}

model {
for (i in 1:M) {
        y[i,3] ~ poisson_log(lmu[i,3]);
    }
beta0 ~ normal(0,10^2);
beta1 ~ normal(0,10^2);
beta2 ~ normal(0,10^2);
beta3 ~ normal(0,10^2);


}

generated quantities {
int<lower=0> ypred[M,3];// predictions at imputed m[i]'s
for (i in 1:M){
    ypred[i,1] = y[i,1];
    ypred[i,2] = y[i,2];
    ypred[i,3] = poisson_log_rng(lmu[i,3]);
  }
}
