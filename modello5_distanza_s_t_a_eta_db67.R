adj<-read.table("diag_flow_matrix_db67.txt",header=T)
d<-read.table("distanze_db.txt",header=T)

centro<-c(1,2,3,4,7,8,9,10,11,14,21,22,24)+1000
dbnames<-rownames(adj)

CCmatr<-matrix(rep(0,67^2),67,67)
for (i in 1:67){
  for (j in 1:67){
    CCmatr[i,j]<-dbnames[i]%in%centro && dbnames[j] %in% centro
  }
}

CPmatr<-matrix(rep(0,67^2),67,67)
for (i in 1:67){
  for (j in 1:67){
    CCmatr[i,j]<-dbnames[i]%in%centro && !(dbnames[j] %in% centro)
  }
}

PCmatr<-t(CPmatr)
autoarchi<-diag(67)

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

centro<-rownames(adj)%>%as.numeric()>1000
centro<-centro%>%as.numeric()

csi<-10
eta<-matrix(rep(0,M^2),M,M)
loc_eta<-c(30,10,1)
for (i in 1:M){
  for (j in 1:M){
    eta[i,j]<-loc_eta[1+centro[i]+centro[j]]
  }
}
somme<-rep(0,M)
A<-matrix(rep(0,M^2),M,M)
for (i in 1:M){
  for (k in 1:M){
    somme[i]<-somme[i]+T[k]/((d[i,k]+csi)^eta[i,j])
  }
}
for (i in 1:M){
  for (j in 1:M){
    A[i,j]<-(T[j]/(d[i,j]+csi)^eta[i,j])/somme[i]
  }
}

library(rstan)
dat <- list(y=adj, d=d, M=M , S=S, T=T, CC=CCmatr,CP=CPmatr,PC=PCmatr,Auto=autoarchi)

options(mc.cores = parallel::detectCores())
fit.1 <- stan(file = 'modello5_distanza_s_t_hier_db67.stan',data = dat, chains = 4, verbose = TRUE,
              iter = 2000,control=list(max_treedepth=15),save_warmup=F)
print(fit.1)

traceplot(fit.1,par=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6","beta7"))
plot(fit.1,par=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6","beta7"))
stan_hist(fit.1, pars=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6","beta7"),bins=50)
stan_ac(fit.1,par=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6","beta7")) #canceled from stan code
chains.1 <- rstan::extract(fit.1, permuted = TRUE)
mu.1 <- exp(chains.1$lmu)
#
# Compute some measures of fit: LPML
#
CPO.1 <- matrix(0,nrow=M,ncol=M)
for (j in 1:M) for (i in 1:M) CPO.1[j,i] <- 1/mean(1/dpois(adj[j,i],lambda=mu.1[,j,i]))
LPML.1 <- sum(log(CPO.1))
print(LPML.1)
