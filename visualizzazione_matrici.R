chains <- rstan::extract(fit.1, permuted = TRUE)
N<-dim(chains$ypred)[1]
ypred1<-chains$ypred[1,,]
<<<<<<< HEAD
ypred2<-chains$ypred[N,,]
=======
ypred2<-chains$ypred[4000,,]
ymean<-0
>>>>>>> 6457b2ade4197f956a29df0ac20079dac87128ed

ymean=0

for (i in 1:N)
  ymean<-ymean+chains$ypred[i,,]
ymean<-ymean/N

Y<-adj
colnames(ypred1)<-colnames(Y)
colnames(ypred2)<-colnames(Y)
colnames(ymean)<-colnames(Y)

range(Y)
range(ypred1)
range(ypred2)
range(ymean)
mean(colMeans(Y))
mean(colMeans(ypred1))

#heatmap(as.matrix(adj), keep.dendro=F,Rowv=F, Colv=F)
x11(width=100,height=30)
par(mfrow=c(1,4))
image(as.matrix(log(Y+1)), main='Observed flow',col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )
image(as.matrix(log(ypred1+1)), main='Predicted flow 1', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )
image(as.matrix(log(ypred2+1)), main='Predicted flow 2', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )
image(as.matrix(log(ymean+1)), main='Predicted mean', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )

load("C:/Users/Celeste/Desktop/netw/BayesMi/grafici_distanza_s_t/previsione2media.RData")
ymean2<-ymean

abs(ymean-ymean2)
x11(width=100,height=30)
par(mfrow=c(1,4))
image(as.matrix(log(Y+1)), main='Observed flow',col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )
image(as.matrix(log(ymean+1)), main='Predicted mean modello3', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )
image(as.matrix(log(ymean2+1)), main='Predicted mean modello2', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )
image(as.matrix(log(abs(ymean-ymean2)+1)), main='Predicted difference abs(model3-model2)', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )

range(ymean)
range(ymean2)