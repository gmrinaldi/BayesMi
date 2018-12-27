adj<-read.table("diag_flow_matrix_db67.txt",header=T)
d<-read.table("distanze_db.txt",header=T)


M<-67

dat <- list(y=adj, d=d, M=M )

options(mc.cores = parallel::detectCores())
fit.1 <- stan(file = 'modello1_distanza_db67.stan',data = dat, chains = 4, verbose = TRUE,
              iter=5000, save_warmup=F)
print(fit.1)

traceplot(fit.1,par=c("beta0","beta1"))
plot(fit.1,par=c("beta0","beta1"))
stan_hist(fit.1, pars=c("beta0","beta1"),bins=50)
stan_ac(fit.1,par=c("beta0","beta1")) #canceled from stan code
chains.1 <- rstan::extract(fit.1, permuted = TRUE)
mu.1 <- exp(chains.1$lmu)
#
# Compute some measures of fit: LPML
#
CPO.1 <- matrix(0,nrow=M,ncol=M)
for (j in 1:M) for (i in 1:M) CPO.1[j,i] <- 1/mean(1/dpois(adj[j,i],lambda=mu.1[,j,i]))
LPML.1 <- sum(log(CPO.1))
print(LPML.1)


