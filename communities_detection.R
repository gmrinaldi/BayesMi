load("bike_NIL_NewDay[1].Rdata")

latlong<-read.table("LatLongIdNomeIdnilNomenil_263.txt", header = T, sep=" ")
head(latlong)
dim(latlong)
N<-dim(latlong)[1] #numero totale delle stazioni

# nomenil<-data.frame( id=bike_NIL_NewDay$NumeroStzInizio, nil=bike_NIL_NewDay$numeroNILinizio,
#                      nomenil=bike_NIL_NewDay$NILinizio)
# head(nomenil)
# nomenil<-unique(nomenil)
# head(nomenil)
# latlong<-data.frame(latlong, nilname=nomenil$nomenil)
# head(latlong)
# write.table(latlong, file = "LatLongIdNomeIdnilNomenil_263.txt")

x<-dist(latlong[,1:2])
head(x)
x<-as.vector(x)
hist(x)

library(dbscan)
library(RColorBrewer)

for (ep in seq(from=0.001, by=0.001, to=0.01)){
  for(mp in 2:5){
    cluster<-dbscan(latlong[,1:2], eps=ep, minPts = mp)
    x11()
    plot(latlong[,1], latlong[,2], pch=cluster$cluster+1, xlab = 'longitude', ylab = 'latitude',
         main=paste('epsilon=',ep, ' MinPoints=', mp), 
         col=rainbow(100)[latlong$nil+10], xlim=c(45.44 , 45.6), ylim = c(9.05, 9.275))
    legend(45.55, 9.27, legend =unique(latlong$nilname), cex = 0.5 ,  col=rainbow(100)[unique(latlong$nil)+10], pch = 16 )
  }
}

