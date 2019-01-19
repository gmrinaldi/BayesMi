library(rstan)

#####clustered---------
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
fit.clustered <- stan(file = 'modello2_distanza_s_t_db67.stan',data = dat, chains = 4, verbose = TRUE,
              iter=5000, save_warmup=F)

saveRDS(fit.clustered, "clustered.rds")
rm(list=ls())

#####cropped-------------
load("C:/Users/Celeste/Desktop/netw/BayesMi/bike_NIL_NewDay[1].Rdata")
library(tidyverse)

d<-read.table("distmatrix.txt", header = T)
colnames(d)<-rownames(d)

flows<-bike_NIL_NewDay%>%group_by(NumeroStzInizio,NumeroStzFine)%>%summarise(Flow=n())%>%ungroup()

staznames<-unique(flows$NumeroStzInizio)

full<-crossing(staznames,staznames)
colnames(full)<-c("NumeroStzInizio","NumeroStzFine")

lookUp<-data.frame(names=colnames(d)%>%as.numeric(),id=1:263)

full_flows<-left_join(full,flows)%>%left_join(lookUp,by=c("NumeroStzInizio"="names"))%>%
  rename(id_inizio=id)%>%left_join(lookUp,by=c("NumeroStzFine"="names"))%>%rename(id_fine=id)
full_flows$Flow[is.na(full_flows$Flow)]<-0

cropped_flows<-full_flows%>%filter(Flow>=15)

Sourcity<-full_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
  .$Sourcity
Targettosity<-full_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
  .$Targettosity

M<-6752

dat <- list(y=cropped_flows%>%select(id_inizio,id_fine,Flow), d=d, M=M,S=Sourcity,T=Targettosity)

options(mc.cores = parallel::detectCores())
fit.cropped <- stan(file = 'modello2_cropped.stan',data = dat, iter=5000, save_warmup=F)

saveRDS(fit.cropped, "cropped.rds")
rm(list=ls())

####comparison----
library("rstanarm")
library("bayesplot")
library("loo")

fit.clustered <- readRDS("clustered.rds")
fit.cropped <- readRDS("cropped.rds")

chainscl <- rstan::extract(fit.clustered, permuted = TRUE)
chainscr <- rstan::extract(fit.cropped, permuted = TRUE)

loocl<-loo(chainscl$ypred,cores=2)
loocr<-loo(chainscr$ypred,cores=2)
loo::compare(loocl, loocr)  # use loo::compare(loo1, loo2) if not using rstanarm

waiccl <- waic(chainscl$ypred)
waiccr <- waic(chainscr$ypred)
compare(waiccl, waiccr)
