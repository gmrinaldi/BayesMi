// Poisson regression model for bikemi data
//
  //  y[ij] ~ Poisson(mu[i]) and
//       log(mu[i]) = beta0 + beta1 * log(x[i]+10) + beta2 * x[i]
//
  // We also include predictions of counts at observed doses, to compare with observations
//
  data {
    int<lower=0> M; // number of viable edges
    int<lower=0> y[M]; 
    vector[M] X;
    // matrix[M,p] NEW;
    int n_groups;
    vector[n_groups] mu;
    real sigma;
    vector[n_groups] alpha;
  }

parameters {
  ordered[n_groups] beta0;
  real betaST;
  vector<lower=0,upper=1>[n_groups-1] lambda_;

}

transformed parameters{
  simplex[n_groups] lambda;

  lambda[1]=1-lambda_[1];
  for (k in 2:n_groups-1)
    lambda[k]=prod(lambda_[1:k-1])*(1-lambda_[k]);
  lambda[n_groups]=prod(lambda_[1:n_groups-1]); 
}


model {
  vector[n_groups] contributions;
  vector[n_groups]log_lambda=log(lambda);
  
  for (i in 1:M) {
    for (k in 1:n_groups){
    // contributions[k] = log_lambda[k] + poisson_log_lpmf(y[i] | beta0[k]+X[i,:]*beta[k]);
    contributions[k] = log_lambda[k] + poisson_log_lpmf(y[i] | beta0[k]+X[i]*betaST);

    }
    // if (y[i]==0)
    //   target += log_sum_exp(bernoulli_lpmf(1 | theta), bernoulli_lpmf(0 | theta) + log_sum_exp(contributions));
    // else target += bernoulli_lpmf(0 | theta) + log_sum_exp(contributions);
    target += log_sum_exp(contributions);
  }
  
  beta0 ~ normal(mu,sigma);
  betaST ~ normal(1,.5);

  for (k in 1:n_groups-1)
    lambda_[k] ~ beta(sum(alpha[k+1:n_groups]),alpha[k]);
}

generated quantities {
  vector[M] log_lik;
  vector[n_groups] contributions;
  
  int zero;
  real pred_lambda;
  // real prob_zero;
  // real prob_uno;
  int<lower=0,upper=n_groups> z[M];
  vector[n_groups] prob_z;
  vector<lower=0>[M] ypred;// predictions at imputed m[i]'s
  // int<lower=0> ymod[M];
  
  for (i in 1:M) {
    for (k in 1:n_groups)
      contributions[k] = log_lambda[k] + poisson_log_lpmf(y[i] | beta0[k]+X[i]*betaST);
  log_lik[i]=log_sum_exp(contributions);
  }


  for (i in 1:M){
    for (k in 1:n_groups){
      prob_z[k] = poisson_log_lpmf(y[i] | beta0[k]+X[i]*betaST)+log(lambda[k]);
    }
    // if(y[i]==0){
    //   prob_zero = log_sum_exp(prob_z)+bernoulli_lpmf(0 | theta);
    //   prob_uno = bernoulli_lpmf(1 | theta);
    //   zero = bernoulli_rng(exp(prob_uno)/(exp(prob_uno)+exp(prob_zero)));
    // }
    // else zero=0;

    // if(zero==0){
      z[i] = categorical_rng(softmax(prob_z));
      pred_lambda = beta0[z[i]]+X[i]*betaST;
      
      if (pred_lambda>20)
        ypred[i] = round(normal_rng(exp(pred_lambda),exp(pred_lambda/2)));
      else ypred[i]= poisson_log_rng(pred_lambda);
    // } else {z[i]=0; ypred[i]=0;}
  }

  //   for (i in 1:M){
  //     for (k in 1:n_groups)
  //       prob_z[k] = poisson_log_lpmf(y[i] | beta0[k]+NEW[i,:]*beta[k])+log(lambda[k]);
  //   if(y[i]==0){
  //     prob_zero = log_sum_exp(prob_z)+bernoulli_lpmf(0 | theta);
  //     prob_uno = bernoulli_lpmf(1 | theta);
  //     zero = bernoulli_rng(exp(prob_uno)/(exp(prob_uno)+exp(prob_zero)));
  //   }
  //   else zero=0;
  //
  //   if(zero==0){
  //     z[i] = categorical_rng(softmax(prob_z));
  //     ypred[i] = poisson_log_rng(beta0[z[i]]+NEW[i,:]*beta[z[i]]);
  //   } else {z[i]=0; ypred[i]=0;}
  // }

}
