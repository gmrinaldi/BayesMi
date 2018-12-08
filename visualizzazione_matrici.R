chains <- rstan::extract(fit.1, permuted = TRUE)
ypred1<-chains$ypred[1,,]
ypred2<-chains$ypred[4000,,]

Y<-adj
colnames(ypred1)<-colnames(Y)
colnames(ypred2)<-colnames(Y)

range(Y)
range(ypred1)
range(ypred2)
mean(colMeans(Y))
mean(colMeans(ypred1))

#heatmap(as.matrix(adj), keep.dendro=F,Rowv=F, Colv=F)
x11()
par(mfrow=c(1,3))
image(as.matrix(log(Y+1)), col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )
image(as.matrix(log(ypred1+1)), col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )
image(as.matrix(log(ypred2+1)), col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y+1)),max(log(Y)),length.out=201) )
