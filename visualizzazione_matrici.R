chains <- rstan::extract(fit.1, permuted = TRUE)
ypred1<-chains$ypred[1,,]
ypred2<-chains$ypred[4000,,]

colnames(ypred1)<-colnames(Y)
colnames(ypred2)<-colnames(Y)

range(Y)
range(ypred1)
mean(colMeans(Y))
mean(colMeans(ypred1))

#heatmap(as.matrix(adj), keep.dendro=F,Rowv=F, Colv=F)
x11()
par(mfrow=c(1,3))
image(as.matrix(log(Y)), col = heat.colors(100),  axes=F,
      breaks = quantile(rbind(log(Y), log(ypred1), log(ypred2)), (0:100)/100 ))
image(as.matrix(log(ypred1)), col = heat.colors(100),  axes=F, 
  breaks = quantile(rbind(log(Y), log(ypred1), log(ypred2)), (0:100)/100 ))
image(as.matrix(log(ypred2)), col = heat.colors(100),  axes=F ,
      breaks = quantile(rbind(log(Y), log(ypred1), log(ypred2)), (0:100)/100 ))

#visiva (multivariata)
# x11()
# par(mfrow=c(1,2))
# image(s1, col=heat.colors(100), main='cov s1', asp=1, axes=FALSE, 
#       breaks=quantile(rbind(s1,s2), (0:100)/100), na.rm=TRUE)
# image(s2, col=heat.colors(100), main='cov s2', asp=1, axes=FALSE, 
#       breaks=quantile(rbind(s1,s2), (0:100)/100), na.rm=TRUE)
# 
