# Usare loo - primo tentativo
library(loo)

log_lik_1<-extract_log_lik(fit.mixture1,merge_chains = F)
r_eff<-relative_eff(exp(log_lik_1))

loo_1<-loo(log_lik_1,r_eff=r_eff)
print(loo_1)
