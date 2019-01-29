visualizzazione_mixture <- function(myfit,perm) { 
  if(perm==TRUE){
    chains <- rstan::extract(myfit, permuted = TRUE)
    N<-dim(chains$ypred)[1]
    ypred1_<-chains$ypred[1,]
    ypred2_<-chains$ypred[N,]
    
    ymean_<-ypred1_
    zmean_<-chains$z[1,]
    
    for (i in 2:N){
      ymean_<-ymean_+chains$ypred[i,]
      zmean_<-zmean_+chains$z[i,]
    }
    ymean_<-ymean_/N
    zmean_<-zmean_/N
    
    M<-sqrt(length(ymean_))
    
    Y<-matrix(rep(0,M^2),M,M)
    ypred1<-matrix(rep(0,M^2),M,M)
    ypred2<-matrix(rep(0,M^2),M,M)
    ymean<-matrix(rep(0,M^2),M,M)

    
    for (i in 1:M^2){
      Y[full_flows$id_inizio[i],full_flows$id_fine[i]]<-full_flows$Flow[i]
      ypred1[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ypred1_[i]
      ypred2[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ypred2_[i]
      ymean[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ymean_[i]

    }
    
  } else{
    chains <- rstan::extract(myfit, permuted = FALSE)
    N<-dim(chains)[1]*dim(chains)[2]
    ind_beg<-which(dimnames(chains)$parameters=="ypred[1]")
    ind_end<-which(dimnames(chains)$parameters=="ypred[4489]")
    
    ind_begz<-which(dimnames(chains)$parameters=="z[1]")
    ind_endz<-which(dimnames(chains)$parameters=="z[4489]")
    
    ypred1_<-chains[500,1,ind_beg:ind_end]
    ypred2_<-chains[500,4,ind_beg:ind_end]
    
    ymean_<-rep(0, length(ypred1_))
    zmean_<-rep(0, length(ypred1_))
    
    for (i in 1:dim(chains)[2]){
      for (j in 1:dim(chains)[1]){
        ymean_<-ymean_+chains[j,i,ind_beg:ind_end]
        zmean_<-zmean_+chains[j,i,ind_begz:ind_endz]
      }
    }
    ymean_<-ymean_/N
    zmean_<-zmean_/N
    
    M<-sqrt(length(ymean_))
    
    ypred1<-matrix(rep(0,M^2),M,M)
    ypred2<-matrix(rep(0,M^2),M,M)
    ymean<-matrix(rep(0,M^2),M,M)
    Y<-matrix(rep(0,M^2),M,M)
    
    
    for (i in 1:dim(full_flows)[1]){
      ypred1[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ypred1_[i]
      ypred2[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ypred2_[i]
      ymean[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ymean_[i]
      Y[full_flows$id_inizio[i],full_flows$id_fine[i]]<-full_flows$Flow[i]
    }
  }
  
  print(range(Y))
  print(range(ypred1))
  print(range(ypred2))
  print(range(ymean))
  print(mean(colMeans(Y)))
  print(mean(colMeans(ymean)))
  print(quantile(Y,seq(0,1,by=.1)))
  print(quantile(ymean,seq(0,1,by=.1)))
  
  x11()
  par(mfrow=c(2,1))
  Yloghist<-hist(log(Y+1))
  hist(log(ymean+1),breaks=Yloghist$breaks)
  
  x11(width=100,height=30)
  par(mfrow=c(1,4))
  image(as.matrix(log(Y+1)), main='Observed flow',col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y+1)),length.out=201) )
  image(as.matrix(log(round(ypred1)+1)), main='Estimated 1', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y+1)),length.out=201)) 
  image(as.matrix(log(round(ypred2)+1)), main='Estimated 2', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y+1)),length.out=201))
  image(as.matrix(log(round(ymean)+1)), main='Estimated mean', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y+1)),length.out=201))
  
  # clustered_flows<-full_flows%>%mutate(cluster=round(zmean_), prediction=ymean_)
}
