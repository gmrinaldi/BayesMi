Y_tot<-read.table("diag_flow_matrix_db67.txt",header=T)

#copia incolla la prima parte dello script modello5_mattina_pomeriggio.R
#per creare adj_mattina e adj_pomeriggio

x11()
image(as.matrix(log(abs(adj_mattina-adj_pomeriggio)+1)), main='differenza', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y_tot+1)),max(log(Y_tot)),length.out=201) )
#salva l'immagine


#lancia modello5_mattina fino a fit.1

chains <- rstan::extract(fit.1, permuted = TRUE)
N<-dim(chains$ypred)[1]
ypred1<-chains$ypred[1,,]

ypred2<-chains$ypred[N,,]

ypred2<-chains$ypred[N,,]
ymean<-0

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
image(as.matrix(log(Y+1)), main='Observed flow',col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y_tot+1)),max(log(Y_tot)),length.out=201) )
image(as.matrix(log(ypred1+1)), main='Predicted flow 1', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y_tot+1)),max(log(Y_tot)),length.out=201) )
image(as.matrix(log(ypred2+1)), main='Predicted flow 2', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y_tot+1)),max(log(Y_tot)),length.out=201) )
image(as.matrix(log(ymean+1)), main='Predicted mean', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y_tot+1)),max(log(Y_tot)),length.out=201) )
#salva l'immagine

#lancia modello5_pomeriggio fino a fit.1 e ripeti da riga 12