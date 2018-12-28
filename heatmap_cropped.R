#devi aver già calcolato Y, ypred1, ypred2, ymean

#la parte commentata è servita solo per calcolare i breaks
#range(Y)

#x11()
#hist(Y[which(Y>0)],breaks=c(15,16,17,18,19,20,40,80,150,max(Y)))
#image(as.matrix(Y), main='Observed flow',col = rev(heat.colors(10)),  axes=F, breaks=c(0,15,16,17,18,19,20,40,80,150,max(Y))) 

x11(width=100,height=30)
par(mfrow=c(1,4))
#faccio permutare la reale
hm<-heatmap(as.matrix(Y),col=rev(heat.colors(10)),Colv="Rowv",symm=T,breaks=c(0,15,16,17,18,19,20,40,80,150,max(Y)))
permutation<-hm$rowInd
#permuto le predette come è stata permutata la reale
heatmap(as.matrix(Y[permutation,permutation]),col=rev(heat.colors(10)),Colv=NA,Rowv=NA,symm=T,breaks=c(0,15,16,17,18,19,20,40,80,150,max(Y)))
heatmap(as.matrix(ypred1[permutation,permutation]),col=rev(heat.colors(10)),Colv=NA,Rowv=NA,symm=T,breaks=c(0,15,16,17,18,19,20,40,80,150,max(Y)))
heatmap(as.matrix(ypred2[permutation,permutation]),col=rev(heat.colors(10)),Colv=NA,Rowv=NA,symm=T,breaks=c(0,15,16,17,18,19,20,40,80,150,max(Y)))
heatmap(as.matrix(ymean[permutation,permutation]),col=rev(heat.colors(10)),Colv=NA,Rowv=NA,symm=T,breaks=c(0,15,16,17,18,19,20,40,80,150,max(Y)))

