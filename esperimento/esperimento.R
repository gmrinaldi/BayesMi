# Preparazione dati (taglio flussi <15)
# Nota: bisogna prima importare i dati! 
library(tidyverse)
library(reshape2)

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

MeltedDist<-melt(as.matrix(d/100))

# Accessibility<-full_flows%>%group_by(id_inizio)%>%mutate(Sourcity=sum(Flow))%>%ungroup()%>%
#   group_by(id_fine)%>%mutate(Targettosity=sum(Flow))%>%ungroup()%>%left_join(MeltedDist,by=c("inizio_db"="Var1","fine_db"="Var2"))%>%
#   rename(Distance=value)%>%mutate(AccessibilityFrom=(Sourcity/(Distance+1)/sum(Sourcity/(Distance+1))),
#                                   AccessibilityTo=(Targettosity/(Distance+1)/sum(Targettosity/(Distance+1))))


# Modello con Stan 
library(rstan)

# M<-6752
M<-67^2;
  
dat <- list(y=full_flows%>%select(Flow), n_groups=2, d=d, M=M,S=Sourcity,T=Targettosity)

options(mc.cores = parallel::detectCores())


# fit.1 <- stan(file = 'esperimento/esperimento4.stan',control=list(max_treedepth=15),data = dat, iter=3000, save_warmup=F, refresh=100)

# fit.2<- stan(file = 'esperimento.stan',control=list(max_treedepth=15),data = dat, iter=2000, save_warmup=F)

fit.3 <- stan(file = 'esperimento/esperimento7.stan',data = dat, iter=50000, save_warmup=F)

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





AccessibilityFrom<-full_flows%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityFrom=Sourcity/sum(Sourcity))
AccessibilityTo<-full_flows%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()%>%
  mutate(AccessibilityTo=Targettosity/sum(Targettosity))

Accessibility<-full_flows%>%left_join(AccessibilityFrom,by="id_inizio")%>%left_join(AccessibilityTo,by="id_fine")

alpha<-1000

# NewFrom<-AccessibilityFrom%>%mutate(NewSource=Sourcity*(id_inizio!=21),Distance21=d[21,]%>%as.numeric(),
#                                     NewFrom=NewSource/sum(Sourcity)*(1+(sum(Sourcity)/sum(NewSource)-1)*exp(-Distance21/alpha)))
#
# NewTo<-AccessibilityTo%>%mutate(NewTarget=Targettosity*(id_fine!=21),Distance21=d[21,]%>%as.numeric(),
#                                 NewTo=NewTarget/sum(Targettosity)*(1+(sum(Targettosity)/sum(NewTarget)-1)*exp(-Distance21/alpha)))
#
# NewAcc<-full_flows%>%left_join(NewFrom,by="id_inizio")%>%left_join(NewTo,by="id_fine")

NewAcc<-multimod_rm(full_flows,c(21,52),d,100,100)

covariates<-Accessibility%>%mutate(Intercept=rep(1,M),LogAccTo=log(AccessibilityTo),LogAccFrom=log(AccessibilityFrom))%>%
  select(Intercept,LogAccTo,LogAccFrom)

newdata<-NewAcc%>%mutate(Intercept=rep(1,M),LogAccTo=log(NewTo),LogAccFrom=log(NewFrom))%>%
  select(Intercept,LogAccTo,LogAccFrom)

dat <- list(y=Accessibility$Flow, M=M, X=as.matrix(covariates),NEW=as.matrix(newdata))

options(mc.cores = parallel::detectCores())

fit.5<- stan(fit.5,file = 'esperimento/esperimento9.stan',data = dat, iter=4500, save_warmup=F)

traceplot(fit.5,par=c("beta"))
plot(fit.5,par=c("beta"))
stan_hist(fit.5, pars=c("beta"),bins=50)



covariates<-Accessibility%>%mutate(LogAccTo=log(AccessibilityTo),LogAccFrom=log(AccessibilityFrom))%>%
  select(LogAccTo,LogAccFrom)

dat <- list(y=Accessibility$Flow, M=M, X=as.matrix(covariates),n_groups=3)

options(mc.cores = parallel::detectCores())

fit.6<- stan(file = 'esperimento/esperimento10.stan',data = dat, iter=10000, control=list(max_treedepth=15),save_warmup=F)

traceplot(fit.6,par=c("beta","beta0","lambda","theta"))
plot(fit.6,par=c("beta","beta0","lambda","theta"))
stan_hist(fit.6, pars=c("beta","beta0","lambda","theta"),bins=50)


covariates<-Accessibility%>%mutate(LogAcc=log(AccessibilityTo)+log(AccessibilityFrom))%>%
  select(LogAcc)

dat <- list(y=Accessibility$Flow, M=M, X=covariates$LogAcc,n_groups=3)

options(mc.cores = parallel::detectCores())

fit.7<- stan(file = 'esperimento/esperimento11.stan',data = dat, iter=10000, control=list(max_treedepth=15),save_warmup=F)

traceplot(fit.7,par=c("beta","beta0","lambda","theta"))
plot(fit.7,par=c("beta","beta0","lambda","theta"))
stan_hist(fit.7, pars=c("beta","beta0","lambda","theta"),bins=50)



dat <- list(y=full_flows$Flow, n_groups=10,M=M)

fit.8 <- stan(file = 'esperimento/esperimento12.stan',data = dat, iter=10000, save_warmup=F)

traceplot(fit.8,par=c("v"))
plot(fit.8,par=c("v"))
stan_hist(fit.8, pars=c("v"),bins=50)

dat <- list(y=full_flows$Flow, n_groups=4, M=M,sigma=2,mu=c(0,3,6,9),alpha=c(.80,.15,.04,.01))

options(mc.cores = parallel::detectCores())

fit.9 <- stan(file = 'esperimento/esperimento5.stan',data = dat, iter=4000, save_warmup=F)

traceplot(fit.9,par=c("beta0","lambda"))
plot(fit.9,par=c("beta0","lambda"))
stan_hist(fit.9, pars=c("beta0","lambda"),bins=50)



multimod_rm <- function(DF, id_names, d, alphaF, alphaT) { 
  
  NewFrom<-DF%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()
  NewTo<-DF%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()
  
  for (i in 1:length(id_names)){
    fromname<-paste("NewFromPart",i,sep="")
    toname<-paste("NewToPart",i,sep="")
    scale_factor_from<-(dim(NewFrom)[1]-1)/(sum(exp(-as.numeric(d[id_names[i],])/alphaF))-1)
    scale_factor_to<-(dim(NewFrom)[1]-1)/(sum(exp(-as.numeric(d[id_names[i],])/alphaT))-1)
    
    NewFrom<-NewFrom%>%mutate(!!fromname := (sum(Sourcity)/sum(Sourcity*(id_inizio!=id_names[i]))-1)*scale_factor_from*exp(-as.numeric(d[id_names[i],])/alphaF))
    NewTo<-NewTo%>%mutate(!!toname := (sum(Targettosity)/sum(Targettosity*(id_fine!=id_names[i]))-1)*scale_factor_to*exp(-as.numeric(d[id_names[i],])/alphaT))
  }

  NewFrom<-NewFrom%>%mutate(NewSource=Sourcity*(!(id_inizio%in%id_names)),
                            NewFrom=NewSource/sum(Sourcity)*(1+rowSums(.[2+1:length(id_names)])))%>%
    select(id_inizio,NewFrom)
  

  NewTo<-NewTo%>%mutate(NewTarget=Targettosity*(!(id_fine%in%id_names)),
                        NewTo=NewTarget/sum(Targettosity)*(1+rowSums(.[2+1:length(id_names)])))%>%
    select(id_fine,NewTo)
  
  NewAcc<-DF%>%left_join(NewFrom,by="id_inizio")%>%left_join(NewTo,by="id_fine")
  
  return(NewAcc)
  
}


