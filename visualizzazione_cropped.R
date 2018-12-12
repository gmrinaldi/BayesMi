chains <- rstan::extract(fit.1, permuted = TRUE)
N<-dim(chains$ypred)[1]
ypred1_<-chains$ypred[1,]
ypred2_<-chains$ypred[N,]

ymean_<-ypred1_

for (i in 2:N)
  ymean_<-ymean_+chains$ypred[i,]
ymean_<-ymean_/N

Y<-matrix(rep(0,263^2),263,263)
for (i in 1:dim(cropped_flows)[1]){
    Y[cropped_flows$id_inizio[i],cropped_flows$id_fine[i]]<-cropped_flows$Flow[i]
  }


ypred1<-matrix(rep(0,263^2),263,263)
ypred2<-matrix(rep(0,263^2),263,263)
ymean<-matrix(rep(0,263^2),263,263)

for (i in 1:dim(cropped_flows)[1]){
  ypred1[cropped_flows$id_inizio[i],cropped_flows$id_fine[i]]<-ypred1_[i]
}
for (i in 1:dim(cropped_flows)[1]){
  ypred2[cropped_flows$id_inizio[i],cropped_flows$id_fine[i]]<-ypred2_[i]
}
for (i in 1:dim(cropped_flows)[1]){
  ymean[cropped_flows$id_inizio[i],cropped_flows$id_fine[i]]<-ymean_[i]
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
image(as.matrix(Y), main='Observed flow',col = rev(heat.colors(200)),  axes=F, breaks=seq(min(Y),max(Y),length.out=201) )
image(as.matrix(ypred1), main='Predicted flow 1', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(Y),max(Y),length.out=201)) 
image(as.matrix(ypred2), main='Predicted flow 2', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(Y),max(Y),length.out=201))
image(as.matrix(ymean), main='Predicted mean', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(Y),max(Y),length.out=201) )
