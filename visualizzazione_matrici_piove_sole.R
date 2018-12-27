Y_tot<-read.table("diag_flow_matrix_db67.txt",header=T)

data<-read.table("databikemi_db.txt", header = T)

attach(data)
adj_piove<-matrix(rep(0,67^2),67,67)
adj_sole<-matrix(rep(0,67^2),67,67)
load("dbnames_ordered.txt")
dbnames<-dbnames_ordered

#consideriamo solo partenza

for (i in 1:length(inizio_db)){
  idi<-dbnames==inizio_db[i]
  idj<-dbnames==fine_db[i]
  if(grepl("a", as.character(Hour1[i]))){
    if(grepl('F', as.character(MorningRH[i])))
      adj_sole[idi,idj]<-adj_sole[idi,idj]+1
    else{
      adj_piove[idi,idj]<-adj_piove[idi,idj]+1
    }
  }
  else{
    if(grepl('F', as.character(AfternoonRH[i])))
      adj_sole[idi,idj]<-adj_sole[idi,idj]+1
    else{
      adj_piove[idi,idj]<-adj_piove[idi,idj]+1
    }
  }
}
}


x11()
image(as.matrix(log(abs(adj_piove-adj_sole)+1)), main='differenza', col = rev(heat.colors(200)),  axes=F, breaks=seq(min(log(Y_tot+1)),max(log(Y_tot)),length.out=201) )
#salva l'immagine


#lancia modello5_piove fino a fit.1

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

#lancia modello5_sole fino a fit.1 e ripeti da riga 41