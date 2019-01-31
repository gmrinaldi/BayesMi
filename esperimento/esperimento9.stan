// Poisson regression model for bikemi data
//
  //  y[ij] ~ Poisson(mu[i]) and
//       log(mu[i]) = beta0 + beta1 * log(x[i]+10) + beta2 * x[i]
//
  // We also include predictions of counts at observed doses, to compare with observations
//
  data {
    int<lower=0> M; // number of edges
    int<lower=0> y[M]; // flows 
    matrix[M,3] X;
    matrix[M,3] NEW;

  }
parameters {
  vector[3] beta;
}

// transformed parameters  {
//   real lmu[M]; // linear predictor
//   lmu = beta0 + beta1*log(d+10) + beta2*log(ACC_TO) + beta3*log(ACC_FROM); // linear predictor
//   
// }

model {
  y ~ poisson_log(X*beta);
  beta ~ normal(0,1);
}

generated quantities {
  int<lower=0> ypred[M];// predictions at imputed m[i]'s
  int<lower=0> ymod[M];// predictions at imputed m[i]'s
  vector[M] temp;
  
  ypred = poisson_log_rng(X*beta);
  temp=NEW*beta;
  for (i in 1:M)
    ymod[i] = (temp[i]<1000 && temp[i]>-1000) ? poisson_log_rng(temp[i]) : 0;
}





//   vector<lower=0>[M] ypred;// predictions at imputed m[i]'s
//   vector<lower=0>[M] ymod;// predictions at imputed m[i]'s
//   vector[M] temp;
//   
//   temp=X*beta;
//   
//   for (i in 1:M){
//     if (temp[i]>20)
//       ypred[i] = round(normal_rng(exp(temp[i]),exp(temp[i]/2)));
//     else ypred[i]= poisson_log_rng(temp[i]);
//   }
// 
//   temp=NEW*beta;
//   for (i in 1:M){
//     if (temp[i]< -1000 && temp[i]>1000)
//      ymod[i]=0;
//     else if (temp[i]>20)
//       ymod[i] = round(normal_rng(exp(temp[i]),exp(temp[i]/2)));
//           else ymod[i]= poisson_log_rng(temp[i]);
//   }
// }
// 
// 
