# Preparazione dati (taglio flussi <15)
# Nota: bisogna prima importare i dati! 
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
Sourcity<-cropped_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
  add_row(id_inizio=256, Sourcity=0,.before=256)%>%add_row(id_inizio=259,Sourcity=0,.before=259)%>%.$Sourcity
Targettosity<-cropped_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
  add_row(id_fine=259,Targettosity=0,.before=259)%>%.$Targettosity

# Modello con Stan 
library(rstan)

M<-6752
  
dat <- list(y=cropped_flows%>%select(id_inizio,id_fine,Flow), d=d, M=M,S=Sourcity,T=Targettosity)

options(mc.cores = parallel::detectCores())
fit.1 <- stan(file = 'modello2_cropped.stan',data = dat, iter=2000, save_warmup=F)
print(fit.1)

traceplot(fit.1,par=c("beta0","beta1","beta2","beta3"))
plot(fit.1,par=c("beta0","beta1","beta2","beta3"))
stan_hist(fit.1, pars=c("beta0","beta1","beta2","beta3"),bins=50)





