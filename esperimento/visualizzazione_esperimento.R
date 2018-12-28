perm=TRUE
if(perm==TRUE){
chains <- rstan::extract(fit.3, permuted = TRUE)
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

Y<-matrix(rep(0,67^2),67,67)
ypred1<-matrix(rep(0,67^2),67,67)
ypred2<-matrix(rep(0,67^2),67,67)
ymean<-matrix(rep(0,67^2),67,67)

for (i in 1:dim(full_flows)[1]){
  Y[full_flows$id_inizio[i],full_flows$id_fine[i]]<-full_flows$Flow[i]
  ypred1[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ypred1_[i]
  ypred2[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ypred2_[i]
  ymean[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ymean_[i]
}

} else{
chains <- rstan::extract(fit.3, permuted = FALSE)
N<-dim(chains)[1]*dim(chains)[2]
ind_beg<-which(dimnames(chains)$parameters=="ypred[1]")
ind_end<-which(dimnames(chains)$parameters=="ypred[4489]")

ind_begz<-which(dimnames(chains)$parameters=="z[1]")
ind_endz<-which(dimnames(chains)$parameters=="z[4489]")

ypred1_<-chains[500,1,ind_beg:ind_end]
ypred2_<-chains[500,3,ind_beg:ind_end]

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


ypred1<-matrix(rep(0,67^2),67,67)
ypred2<-matrix(rep(0,67^2),67,67)
ymean<-matrix(rep(0,67^2),67,67)
Y<-matrix(rep(0,67^2),67,67)


for (i in 1:dim(full_flows)[1]){
  ypred1[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ypred1_[i]
  ypred2[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ypred2_[i]
  ymean[full_flows$id_inizio[i],full_flows$id_fine[i]]<-ymean_[i]
  Y[full_flows$id_inizio[i],full_flows$id_fine[i]]<-full_flows$Flow[i]
}
}
# colnames(ypred1)<-colnames(Y)
# colnames(ypred2)<-colnames(Y)
# colnames(ymean)<-colnames(Y)

range(Y)
range(ypred1)
range(ypred2)
range(ymean)
mean(colMeans(Y))
mean(colMeans(ymean))

#heatmap(as.matrix(adj), keep.dendro=F,Rowv=F, Colv=F)
x11(width=100,height=30)
par(mfrow=c(1,4))
image(as.matrix(log(Y+1)), main='Observed flow',col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y+1)),length.out=201) )
image(as.matrix(log(ypred1+1)), main='Predicted flow 1', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y+1)),length.out=201)) 
image(as.matrix(log(ypred2+1)), main='Predicted flow 2', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y+1)),length.out=201))
image(as.matrix(log(round(ymean)+1)), main='Predicted mean', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y+1)),length.out=201) )

clustered_flows<-full_flows%>%mutate(cluster=round(zmean_), prediction=ymean_)



