d<-read.table("distmatrix.txt", header = T)
head(d)
colnames(d)<-rownames(d)

library("rstan")
library("dplyr")
library("tidyr")

# data

load("bike_NIL_NewDay[1].Rdata")
attach(bike_NIL_NewDay)
M<-263

#construct distance matrix

distances<-bike_NIL_NewDay%>%group_by(NumeroStzInizio,NumeroStzFine)%>%summarise(MeanDist=mean(KmPercorsi), Flow=n())%>%ungroup()

staznames<-unique(distances$NumeroStzInizio)

full<-crossing(staznames,staznames)
colnames(full)<-c("NumeroStzInizio","NumeroStzFine")

full_distances<-left_join(full,distances)
full_distances$Flow[is.na(full_distances$Flow)]<-0
full_distances$MeanDist[is.na(full_distances$MeanDist)]<-1.96

w_adj<-matrix(full_distances$Flow,M,M)
Y<-w_adj

# Frequentist fit: use glm(y~x1+x2,family=poisson)
# where x1=log(x+10) and x2=x/1000
#
dat <- list(y=Y, d=d, M=M)

options(mc.cores = parallel::detectCores())
fit.1 <- stan(file = 'distanzakm.stan',data = dat, chains = 3, verbose = TRUE,
              iter = 10000)
print(fit.1)

traceplot(fit.1,par=c("beta0","beta1"))
plot(fit.1,par=c("beta0","beta1"))
stan_hist(fit.1, pars=c("beta0","beta1"),bins=50)
#stan_ac(fit.1,par=c("beta0","beta1","ypred")) #canceled from stan code
chains.1 <- rstan::extract(fit.1, permuted = TRUE)
mu.1 <- exp(chains.1$lmu)
#
# Compute some measures of fit: LPML
#
CPO.1 <- matrix(0,nrow=M,ncol=M)
for (j in 1:M) for (i in 1:M) CPO.1[j,i] <- 1/mean(1/dpois(Y[j,i],lambda=mu.1[,j,i]))
LPML.1 <- sum(log(CPO.1))
print(LPML.1)

#
# Predictions for new observations with same doses
#
#
# Note: we can do predictions from within R as well. For instance:
#nmcmc <- nrow(chains.1$mu) # number of simulations in the chain
#ypred <- matrix(0,nrow=nmcmc,ncol=6) # here we store predictions
#for (i in 1:6) ypred[,i] <- rpois(nmcmc,lambda=mu.1[,i])
#
# Plot credibility regions for predictions with data points
#
yquant <- matrix(0,nrow=3,ncol=6)
for (i in 1:6) yquant[,i] <- quantile(chains.1$ypred[,i],probs=c(0.025,0.5,0.975))
plot(x,yquant[1,],pch="",ylim=range(c(y,yquant)),xlab="Dose",ylab="Counts",
     main="95% Prediction Intervals")
lines(x,yquant[2,],lty=1)
lines(x,yquant[1,],lty=2)
lines(x,yquant[3,],lty=2)
points(x,y[1,],col="1")
points(x,y[2,],col="2")
points(x,y[3,],col="3")
lines(x,ybar,lwd=2,col=4)
legend(700,70,legend=c("Plate 1","Plate 2","Plate 3","Mean by Plate"),text.col=c(1,2,3,4),bty="n")

detach(bike_NIL_NewDay)