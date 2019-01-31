# Carico le librerie e importo il dataset
library(tidyverse)
library(reshape2)
library(rstan)
library(rgexf)
# Nota: per ottenere questo fate andare lo script community detection
# e salvate nuovo_bikemi con il comando
# save(nuovo_bikemi,file="nuovo_bikemi.RData")
load("nuovo_bikemi.Rdata")

# Manipolo i dati per lavorarci meglio (e calcolo S e T)

d<-read.table("distanze_db.txt", header = T)
colnames(d)<-rownames(d)

flows<-nuovo_bikemi%>%group_by(inizio_db,fine_db)%>%summarise(Flow=n())%>%ungroup()

dbnames<-unique(flows$inizio_db)

full<-tidyr::crossing(dbnames,dbnames)
colnames(full)<-c("inizio_db","fine_db")

lookUp<-data.frame(names=colnames(d)%>%as.numeric(),id=1:67)

full_flows<-left_join(full,flows)%>%left_join(lookUp,by=c("inizio_db"="names"))%>%
  rename(id_inizio=id)%>%left_join(lookUp,by=c("fine_db"="names"))%>%rename(id_fine=id)
full_flows$Flow[is.na(full_flows$Flow)]<-0

MeltedDist<-melt(as.matrix(d))

M<-67^2;

##### Per eliminare i nodi: #####

AccessibilityFrom<-full_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityFrom=Sourcity/sum(Sourcity))
AccessibilityTo<-full_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityTo=Targettosity/sum(Targettosity))

Accessibility<-full_flows%>%left_join(AccessibilityFrom,by="id_inizio")%>%left_join(AccessibilityTo,by="id_fine")

# Importo la funzione per eliminare i nodi
source("esperimento/multimod_rm_function.R")
# La funzione riceve in ingresso il dataframe full_flows (non modificatelo!)
# un vettore con gli ID dei nodi da rimuovere (se volete sapere a cosa corrispondono
# basta guardare lookup), la matrice delle distanze e due parametri alphato e alphafrom (metteteli uguali)
alpha<-500 # alpha<-100
NewAcc<-multimod_rm(full_flows,c(21,52),d,alpha,alpha)

covariates<-Accessibility%>%mutate(Intercept=rep(1,M),LogAccTo=log(AccessibilityTo),LogAccFrom=log(AccessibilityFrom))%>%
  select(Intercept,LogAccTo,LogAccFrom)

newdata<-NewAcc%>%mutate(Intercept=rep(1,M),LogAccTo=log(NewTo),LogAccFrom=log(NewFrom))%>%
  select(Intercept,LogAccTo,LogAccFrom)

dat <- list(y=Accessibility$Flow, M=M, X=as.matrix(covariates),NEW=as.matrix(newdata))

options(mc.cores = parallel::detectCores())

fit.removed<- stan(file = 'esperimento/esperimento9.stan',data = dat, iter=4000, save_warmup=F)

traceplot(fit.removed,par=c("beta"))
plot(fit.removed,par=c("beta"))
stan_hist(fit.removed, pars=c("beta"),bins=50)

# Per costruire heatmap e visualizzare usate questa funzione (se volete
# potete modificarla, si basa sul solito script visualizzazione)
# Nota: qua lasciate sempre perm=T
source("esperimento/visualizzazione_removed_function.R")
modified_flows<-visualizzazione_removed(fit.removed,perm=T)
# Per produrre un file da usare in gephi
# Attenzione! Al momento questo sovrascrive ogni volta il file rete_modificata
# nella cartella Gephi, usare con cautela!

# source("esperimento/esporta_removed_function.R")
# esporta_removed(modified_flows)

#### Per il modello mistura ####

# Solo intercetta: mu è il vettore delle medie dei beta0, sigma la varianza 
# uguale per tutti, alpha è il vettore dei parametri della prior sui pesi
# della mistura (i lambda), quindi lambda~dirichlet(alpha) ####
dat <- list(y=full_flows$Flow, n_groups=4, M=M,sigma=1.5,mu=c(0,3,5.5,8),alpha=c(.80,.15,.04,.01))

options(mc.cores = parallel::detectCores())

fit.mixture1 <- stan(file = 'esperimento/esperimento5.stan',data = dat, iter=1000, save_warmup=T)

traceplot(fit.mixture1,par=c("beta0","lambda"))
plot(fit.mixture1,par=c("beta0"))
plot(fit.mixture1,par=c("lambda"))
# Provate a vedere se l'istogramma resta nel range della prior o sta cercando
# di spingersi più al limite dei mu+4*sigma
stan_hist(fit.mixture1, pars=c("beta0","lambda"),bins=50)

# Per visualizzare (se dal traceplot non si vede un solo grosso baco
# mettete perm=F)
source("esperimento/visualizzazione_mixture_function.R")
visualizzazione_mixture(fit.mixture1,perm=T)

# Intercetta e covariate (distanza e/o S*T) ####
AccessibilityFrom<-full_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityFrom=Sourcity/sum(Sourcity))
AccessibilityTo<-full_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityTo=Targettosity/sum(Targettosity))

Accessibility<-full_flows%>%left_join(AccessibilityFrom,by="id_inizio")%>%left_join(AccessibilityTo,by="id_fine")%>%
  left_join(MeltedDist,by=c("inizio_db"="Var1","fine_db"="Var2"))%>%rename(Distance=value)

# Se volete provare un modello con una sola covariata basta modificare select(LogAccToFrom,LogDist)
# lasciandone una sola 

covariates<-Accessibility%>%mutate(LogDist=log(Distance+10),LogAccToFrom=log(AccessibilityTo)+log(AccessibilityFrom))%>%
  select(LogAccToFrom,LogDist)

# Qui ci vogliono sicuramente un po' di prove con mu, alpha e n_groups!
dat <- list(y=full_flows$Flow, X=covariates, n_groups=2, p=dim(covariates)[2], M=M,
            sigma=2,mu=c(12,15),alpha=c(.85,.15))

options(mc.cores = parallel::detectCores())

# Nota: questo al momento non è zero inflated!
fit.mixture2 <- stan(file = 'esperimento/esperimento10.stan', pars=c("beta0","lambda","beta","log_lik"), data = dat, iter=800, save_warmup=T)

traceplot(fit.mixture2,par=c("beta0","lambda","beta"),inc_warmup=T)
plot(fit.mixture2,par=c("beta0","lambda","beta"))
stan_hist(fit.mixture2, pars=c("beta0","lambda","beta"),bins=50)

# Per visualizzare (se dal traceplot non si vede un solo grosso baco
# mettete perm=F)
source("esperimento/visualizzazione_mixture_function.R")
visualizzazione_mixture(fit.mixture2,perm=F)

# Per provare a fittare un modello con beta0-k + beta1*ST ####
AccessibilityFrom<-full_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityFrom=Sourcity/sum(Sourcity))
AccessibilityTo<-full_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityTo=Targettosity/sum(Targettosity))

Accessibility<-full_flows%>%left_join(AccessibilityFrom,by="id_inizio")%>%left_join(AccessibilityTo,by="id_fine")%>%
  left_join(MeltedDist,by=c("inizio_db"="Var1","fine_db"="Var2"))%>%rename(Distance=value)

covariates<-Accessibility%>%mutate(LogDist=log(Distance+10),LogAccToFrom=log(AccessibilityTo)+log(AccessibilityFrom))%>%
  select(LogAccToFrom,LogDist)


# Qui ci vogliono sicuramente un po' di prove con mu, alpha e n_groups!
dat <- list(y=full_flows$Flow, X=covariates$LogAccToFrom, n_groups=5, M=M,
            sigma=1.5,mu=c(8,10,11.5,13,15),alpha=c(2,2,2,2,2))

options(mc.cores = parallel::detectCores())

# Nota: questo al momento non è zero inflated!
# Nota: sono utili prove con poche iterazioni, anche per stimare il numero di iterazioni
# necessario (ma soprattutto per vedere cosa succede senza dover aspettare ore)

fit.mixture3 <- stan(file = 'esperimento/esperimento14.stan', pars=c("ypred","beta0","lambda","betaST","log_lik"), data = dat, 
                     iter=2000, save_warmup=F,control=list(max_treedepth=12))

traceplot(fit.mixture3,par=c("beta0","lambda","betaST"),inc_warmup=F)
plot(fit.mixture3,par=c("beta0","lambda","betaST"))
stan_hist(fit.mixture3, pars=c("beta0","lambda","betaST"),bins=50)

# Per visualizzare (se dal traceplot non si vede un solo grosso baco
# mettete perm=F)
source("esperimento/visualizzazione_mixture_function.R")
visualizzazione_mixture(fit.mixture3,perm=T)

# Per provare a fittare un modello con beta0-k + beta1*ST + beta2-k*dist ####
AccessibilityFrom<-full_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityFrom=Sourcity/sum(Sourcity))
AccessibilityTo<-full_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityTo=Targettosity/sum(Targettosity))

Accessibility<-full_flows%>%left_join(AccessibilityFrom,by="id_inizio")%>%left_join(AccessibilityTo,by="id_fine")%>%
  left_join(MeltedDist,by=c("inizio_db"="Var1","fine_db"="Var2"))%>%rename(Distance=value)

covariates<-Accessibility%>%mutate(LogDist=log(Distance+10),LogAccToFrom=log(AccessibilityTo)+log(AccessibilityFrom))%>%
  select(LogAccToFrom,LogDist)


# Qui ci vogliono sicuramente un po' di prove con mu, alpha e n_groups!
dat <- list(y=full_flows$Flow, X=covariates, n_groups=5, M=M, p=dim(covariates)[2],
            sigma=1.5,mu=c(8,10,11.5,13,15),alpha=c(2,2,2,2,2))

options(mc.cores = parallel::detectCores())

# Nota: questo al momento non è zero inflated!
# Nota: sono utili prove con poche iterazioni, anche per stimare il numero di iterazioni
# necessario (ma soprattutto per vedere cosa succede senza dover aspettare ore)

fit.mixture4 <- stan(file = 'esperimento/esperimento13.stan', pars=c("ypred","beta0","lambda","betaST","betaDist","log_lik"), data = dat, 
                     iter=2000, save_warmup=F,control=list(max_treedepth=12))

traceplot(fit.mixture4,par=c("beta0","lambda","betaST","betaDist"),inc_warmup=F)
plot(fit.mixture4,par=c("beta0","lambda","betaST","betaDist"))
stan_hist(fit.mixture4, pars=c("beta0","lambda","betaST","betaDist"),bins=50)

# Per visualizzare (se dal traceplot non si vede un solo grosso baco
# mettete perm=F)
source("esperimento/visualizzazione_mixture_function.R")
visualizzazione_mixture(fit.mixture4,perm=T)


# Per provare a fittare un modello ZERO INFLATED con beta0-k  ####

# Solo intercetta: mu è il vettore delle medie dei beta0, sigma la varianza 
# uguale per tutti, alpha è il vettore dei parametri della prior sui pesi
# della mistura (i lambda), quindi lambda~dirichlet(alpha) ####
dat <- list(y=full_flows$Flow, n_groups=4, M=M,sigma=1.5,mu=c(0,3,5.5,8),alpha=c(.80,.15,.04,.01))

options(mc.cores = parallel::detectCores())

fit.mixture5 <- stan(file = 'esperimento/esperimento15.stan',data = dat, iter=4000, save_warmup=T)

traceplot(fit.mixture5,par=c("beta0","lambda","theta"))
plot(fit.mixture5,par=c("beta0"))
plot(fit.mixture5,par=c("lambda"))
# Provate a vedere se l'istogramma resta nel range della prior o sta cercando
# di spingersi più al limite dei mu+4*sigma
stan_hist(fit.mixture5, pars=c("beta0","lambda","theta"),bins=50)

# Per visualizzare (se dal traceplot non si vede un solo grosso baco
# mettete perm=F)
source("esperimento/visualizzazione_mixture_function.R")
visualizzazione_mixture(fit.mixture5,perm=T)


# Per provare a fittare un modello ZERO INFLATED con beta0-k + beta1*ST ####
AccessibilityFrom<-full_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityFrom=Sourcity/sum(Sourcity))
AccessibilityTo<-full_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityTo=Targettosity/sum(Targettosity))

Accessibility<-full_flows%>%left_join(AccessibilityFrom,by="id_inizio")%>%left_join(AccessibilityTo,by="id_fine")%>%
  left_join(MeltedDist,by=c("inizio_db"="Var1","fine_db"="Var2"))%>%rename(Distance=value)

covariates<-Accessibility%>%mutate(LogDist=log(Distance+10),LogAccToFrom=log(AccessibilityTo)+log(AccessibilityFrom))%>%
  select(LogAccToFrom,LogDist)


# Qui ci vogliono sicuramente un po' di prove con mu, alpha e n_groups!
dat <- list(y=full_flows$Flow, X=covariates$LogAccToFrom, n_groups=5, M=M,
            sigma=1.5,mu=c(8,10,11.5,13,15),alpha=c(2,2,2,2,2))

options(mc.cores = parallel::detectCores())

# Nota: sono utili prove con poche iterazioni, anche per stimare il numero di iterazioni
# necessario (ma soprattutto per vedere cosa succede senza dover aspettare ore)

fit.mixture6 <- stan(file = 'esperimento/esperimento16.stan', pars=c("ypred","beta0","lambda","betaST","theta","log_lik"), data = dat, 
                     iter=2000, save_warmup=F,control=list(max_treedepth=12))

traceplot(fit.mixture6,par=c("beta0","lambda","betaST","theta"),inc_warmup=F)
plot(fit.mixture6,par=c("beta0","lambda","betaST","theta"))
stan_hist(fit.mixture6, pars=c("beta0","lambda","betaST","theta"),bins=50)

# Per visualizzare (se dal traceplot non si vede un solo grosso baco
# mettete perm=F)
source("esperimento/visualizzazione_mixture_function.R")
visualizzazione_mixture(fit.mixture6,perm=T)




