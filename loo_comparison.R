library(rstan)
library("rstanarm")
library("bayesplot")
library("loo")

#metti i nomi giusti negli Rdata alle righe 8 e 17

mod1<-load("fit.mixture_beta0k_beta1.RData")
saveRDS(mod1, "mod1.rds")
rm(mod1)
fit1 <- readRDS("mod1.rds")
chains1 <- rstan::extract(fit1, permuted = TRUE)
loo1<-loo(chains1$ypred,cores=2)
rm(fit1)
rm(chains1)

mod2<-load("fit.mixture0_beta0k_beta1.RData")
saveRDS(mod2, "mod2.rds")
rm(mod2)
fit2 <- readRDS("mod2.rds")
chains2 <- rstan::extract(fit2, permuted = TRUE)
loo2<-loo(chains2$ypred,cores=2)
rm(fit2)
rm(chains2)

loo::compare(loo1, loo2)  # use loo::compare(loo1, loo2) if not using rstanarm

##in caso non cancellare chains
waic1 <- waic(chains1$ypred)
waic2 <- waic(chains2$ypred)
compare(waic1, waic2)
