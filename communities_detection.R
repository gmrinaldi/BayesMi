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

#sistemare il grafico con legenda
for (ep in seq(from=0.001, by=0.001, to=0.01)){
  for(mp in 2:5){
    cluster<-dbscan(latlong[,1:2], eps=ep, minPts = mp)
    print(paste("cluster" ,length(unique(cluster$cluster))))
    print(ep)
    print(mp)
    if(length(unique(cluster$cluster))>10){
      x11()
      plot(latlong[,2], latlong[,1], pch=cluster$cluster, xlab = 'longitude', ylab = 'latitude',
           main=paste('epsilon=',ep, ' MinPoints=', mp), 
           col=rainbow(100)[latlong$nil+10], ylim=c(45.44 , 45.537), xlim = c(9.08, 9.3))
      legend( 9.25,45.53, legend =unique(latlong$nilname), cex = 0.5 ,  col=rainbow(100)[unique(latlong$nil)+10], pch = 16 )
    }
  }
}

#004 2

cluster<-dbscan(latlong[,1:2], eps=0.004, minPts = 2)
cluster$cluster
cluster

nuove_stazioni<-data.frame(latlong, db=cluster$cluster) #densità bayesiana
head(nuove_stazioni)

#1-calcolo matrice distanze

library(dplyr)

baricentri<-nuove_stazioni%>%filter(db>0)%>%group_by(db)%>%
  summarise(MeanLat=mean(lat), MeanLong=mean(long))%>%ungroup()
head(baricentri)
baricentri$db<-baricentri$db+1000
zeri<-data.frame(db=nuove_stazioni$id[which(nuove_stazioni$db==0)],
                 MeanLat=nuove_stazioni$lat[which(nuove_stazioni$db==0)],
                 MeanLong=nuove_stazioni$long[which(nuove_stazioni$db==0)])
nuove_stazioni<-rbind(zeri,  baricentri)

head(nuove_stazioni)
N<-dim(nuove_stazioni)[1]

library("geosphere")

distanze_db<-matrix(rep(0, N*N), N)

for(i in 1:N){
  for(j in 1:N){
    distanze_db[i,j]<-distGeo(c(nuove_stazioni[i,2], nuove_stazioni[i,3]),
                              c(nuove_stazioni[j,2], nuove_stazioni[j,3])) #distanze in metri
    
  }
} #matrice simmetrica

head(distanze_db)
colnames(distanze_db)<-nuove_stazioni$db
rownames(distanze_db)<-nuove_stazioni$db

write.table(distanze_db, file="distanze_db.txt")

#2-calcolo dei flussi

conversione<-data.frame(id=latlong$id, db=cluster$cluster)
conversione
conversione$db<-conversione$db+1000

for(i in 1: 263){
  if(conversione$db[i]==1000)
    conversione$db[i]=conversione$id[i]
}

M<-dim(bike_NIL_NewDay)[1]
nuovo_bikemi<-bike_NIL_NewDay

nuovo_bikemi<-nuovo_bikemi%>%left_join(conversione, by=c("NumeroStzInizio"="id"))%>%
  rename(inizio_db=db)%>%left_join(conversione, by=c("NumeroStzFine"="id"))%>%
  rename(fine_db=db)

adj_db<-matrix(rep(0,67^2),67,67)
db_names<-nuove_stazioni$db

for (i in 1:dim(nuovo_bikemi)[1]){
  idi<-db_names==nuovo_bikemi$inizio_db[i]
  idj<-db_names==nuovo_bikemi$fine_db[i]
  adj_db[idi,idj]<-adj_db[idi,idj]+1
}

for (i in 1:67)
  adj_db[i,i]<-0
adj_db

rownames(adj_db)<-db_names
colnames(adj_db)<-db_names

write.table(adj_db, "flow_matrix_db67.txt")
