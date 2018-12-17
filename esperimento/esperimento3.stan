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
  real <lower=0> d[67,67]; // distances
  int<lower=0> S[67];
  int<lower=0> T[67];
  int n_groups;
}
parameters {
  real beta0[n_groups];
  real<lower=0,upper=1> theta;
  simplex[n_groups] lambda[M];
}

transformed parameters  {
  real lmu[n_groups]; // linear predictor
  for (i in 1:M) {
    for (k in 1:n_groups){
      lmu[k] = beta0[k];
    }
  }
}

model {
vector[n_groups] contributions;

for (i in 1:M) {
  if (y[i,3]==0)
    contributions[1] = log(lambda[i,1]) + log_sum_exp(bernoulli_lpmf(1 | theta), bernoulli_lpmf(0 | theta) + poisson_log_lpmf(y[i,3] | lmu[1]));
  else contributions[1] = log(lambda[i,1]) + bernoulli_lpmf(0 | theta) + poisson_log_lpmf(y[i,3] | lmu[1]);
  for (k in 2:n_groups){
    contributions[k] = log(lambda[i,k]) + poisson_log_lpmf(y[i,3] | lmu[k]);
  }
  target += log_sum_exp(contributions);
}
beta0 ~ normal(0,25);
theta ~ uniform(0,1);
//lambda ~ dirichlet(rep_vector(2.0, n_groups));
for (i in 1:M)
  lambda[i] ~ dirichlet(rep_vector(2.0, n_groups));
}

generated quantities {
int zero;
int z;
int<lower=0> ypred[M];// predictions at imputed m[i]'s
for (i in 1:M){
    z = categorical_rng(lambda[i]);
    if(z==1){
      zero = bernoulli_rng(theta);
      ypred[i] = (1-zero)*poisson_log_rng(lmu[z]);
    }
    else ypred[i] = poisson_log_rng(lmu[z]);
  }
}
