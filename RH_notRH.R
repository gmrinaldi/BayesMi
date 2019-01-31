#confronto RH e non RH

data<-read.table("databikemi_db.txt", header = T)
head(data)
Y_tot<-read.table("diag_flow_matrix_db67.txt",header=T)

attach(data)
adj_RH<-matrix(rep(0,67^2),67,67)
adj_notRH<-matrix(rep(0,67^2),67,67)
load("dbnames_ordered.txt")
dbnames<-dbnames_ordered

#aggiungo +1 al flusso se il viaggio è stato fatto in un'ora di punta--> controllo la colonna RH
for (i in 1:length(inizio_db)){
  idi<-dbnames==inizio_db[i] #punto di partenza
  idj<-dbnames==fine_db[i] #punto di arrivo
  if(RH[i]=="True") #se il viaggio è stato fatto in un'ora di punta
    adj_RH[idi,idj]<-adj_RH[idi,idj]+1
  if(RH[i]=="False")
    adj_notRH[idi,idj]<-adj_notRH[idi,idj]+1
}

adj<-adj_notRH #da cambiare se vuoi non RH

rownames(adj)<-dbnames
colnames(adj)<-dbnames

d<-read.table("distanze_db.txt",header=T)

centro<-c(1,2,3,4,7,8,9,10,11,14,21,22,24)+1000

CCmatr<-matrix(rep(0,67^2),67,67)
for (i in 1:67){
  for (j in 1:67){
    CCmatr[i,j]<-dbnames[i]%in%centro && dbnames[j] %in% centro
  }
}

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


library(rstan)

dat <- list(y=adj, d=d, M=M , S=S, T=T, CC=CCmatr, Auto=autoarchi)

options(mc.cores = parallel::detectCores())
fit.1 <- stan(file = 'modello5_mattina_pomeriggio.stan',data = dat, chains = 4, verbose = TRUE,
              iter = 2000,control=list(max_treedepth=15),save_warmup=F)
print(fit.1)

traceplot(fit.1,par=c("beta0","beta1","beta2","beta3","beta4","beta5"))
plot(fit.1,par=c("beta0","beta1","beta2","beta3","beta4","beta5"))
stan_hist(fit.1, pars=c("beta0","beta1","beta2","beta3","beta4","beta5"),bins=50)
stan_ac(fit.1,par=c("beta0","beta1","beta2","beta3","beta4","beta5")) #canceled from stan code
chains.1 <- rstan::extract(fit.1, permuted = TRUE)
mu.1 <- exp(chains.1$lmu)
#
# Compute some measures of fit: LPML
#
CPO.1 <- matrix(0,nrow=M,ncol=M)
for (j in 1:M) for (i in 1:M) CPO.1[j,i] <- 1/mean(1/dpois(adj[j,i],lambda=mu.1[,j,i]))
LPML.1 <- sum(log(CPO.1))
print(LPML.1)


detach(data)
