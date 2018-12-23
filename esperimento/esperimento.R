# Preparazione dati (taglio flussi <15)
# Nota: bisogna prima importare i dati! 
library(tidyverse)

d<-read.table("distanze_db.txt", header = T)
colnames(d)<-rownames(d)

flows<-nuovo_bikemi%>%group_by(inizio_db,fine_db)%>%summarise(Flow=n())%>%ungroup()

dbnames<-unique(flows$inizio_db)

full<-crossing(dbnames,dbnames)
colnames(full)<-c("inizio_db","fine_db")

lookUp<-data.frame(names=colnames(d)%>%as.numeric(),id=1:67)

full_flows<-left_join(full,flows)%>%left_join(lookUp,by=c("inizio_db"="names"))%>%
  rename(id_inizio=id)%>%left_join(lookUp,by=c("fine_db"="names"))%>%rename(id_fine=id)
full_flows$Flow[is.na(full_flows$Flow)]<-0

# cropped_flows<-full_flows%>%filter(Flow>=15)

# Sourcity_c<-cropped_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
#   add_row(id_inizio=256, Sourcity=0,.before=256)%>%add_row(id_inizio=259,Sourcity=0,.before=259)%>%.$Sourcity
# Targettosity_c<-cropped_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
#   add_row(id_fine=259,Targettosity=0,.before=259)%>%.$Targettosity

Sourcity<-full_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
  .$Sourcity
Targettosity<-full_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
 .$Targettosity

# Modello con Stan 
library(rstan)

# M<-6752
M<-67^2;
  
dat <- list(y=full_flows%>%select(id_inizio,id_fine,Flow), n_groups=4, d=d, M=M,S=Sourcity,T=Targettosity)

options(mc.cores = parallel::detectCores())

# fit.1 <- stan(file = 'esperimento/esperimento4.stan',control=list(max_treedepth=15),data = dat, iter=3000, save_warmup=F, refresh=100)

# fit.2<- stan(file = 'esperimento.stan',control=list(max_treedepth=15),data = dat, iter=2000, save_warmup=F)

fit.3 <- stan(file = 'esperimento/esperimento5.stan',data = dat, iter=10000, save_warmup=F)

print(fit.1,par=c("beta0","beta1","gamma0","gamma1","gamma2"))

traceplot(fit.1,par=c("beta0","beta1","gamma0","gamma1","gamma2"))
stan_plot(fit.1,par=c("beta0","beta1","gamma0","gamma1","gamma2"))
stan_hist(fit.1, pars=c("beta0","beta1","gamma0","gamma1","gamma2"),bins=50)

print(fit.2,par=c("beta0"))

traceplot(fit.2,par=c("beta0","lambda"))
plot(fit.2,par=c("beta0","lambda"))
stan_hist(fit.2, pars=c("beta0","lambda"),bins=50)


print(fit.3,par=c("beta0"))

traceplot(fit.3,par=c("beta0","lambda"))
plot(fit.3,par=c("beta0","lambda"))
stan_hist(fit.3, pars=c("beta0","lambda","theta"),bins=50)


