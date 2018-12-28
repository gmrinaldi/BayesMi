adj<-read.table("diag_flow_matrix_db67.txt",header=T)
d<-read.table("distanze_db.txt",header=T)

library(igraph)

graph<-graph_from_adjacency_matrix(as.matrix(adj), mode = c("directed"), diag = TRUE,
                                   add.colnames = NULL, add.rownames = NA)

S<-degree(graph, v = V(graph), mode = c("out"),
          loops = TRUE, normalized = FALSE)
names(S)<-rownames(adj)
S #grado totale uscente di ogni nodo

T<-degree(graph, v = V(graph), mode = c("in"),
          loops = TRUE, normalized = FALSE)
names(T)<-rownames(adj)
T #grado totale entrante di ogni nodo

M<-67

dat <- list(y=adj, d=d, M=M , S=S, T=T)

options(mc.cores = parallel::detectCores())
fit.1 <- stan(file = 'modello2_distanza_s_t_db67.stan',data = dat, chains = 4, verbose = TRUE,
              iter=5000, save_warmup=F)
print(fit.1)

traceplot(fit.1,par=c("beta0","beta1","beta2","beta3"))
plot(fit.1,par=c("beta0","beta1","beta2","beta3"))
stan_hist(fit.1, pars=c("beta0","beta1","beta2","beta3"),bins=50)
stan_ac(fit.1,par=c("beta0","beta1","beta2","beta3")) #canceled from stan code
chains.1 <- rstan::extract(fit.1, permuted = TRUE)
mu.1 <- exp(chains.1$lmu)
#
# Compute some measures of fit: LPML
#
CPO.1 <- matrix(0,nrow=M,ncol=M)
for (j in 1:M) for (i in 1:M) CPO.1[j,i] <- 1/mean(1/dpois(adj[j,i],lambda=mu.1[,j,i]))
LPML.1 <- sum(log(CPO.1))
print(LPML.1)


