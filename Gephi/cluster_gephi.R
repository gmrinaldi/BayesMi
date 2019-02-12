library(dplyr,tidyr)
library(rgexf)

chains <- rstan::extract(fit.mixture1, permuted = TRUE)
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

clustered_flows<-full_flows%>%mutate(cluster=round(zmean_), prediction=ymean_)

# Rete per gephi (cluster da modello mistura)
rete<-clustered_flows%>%rename(Source=id_inizio,Target=id_fine)

latlong_staz<-read.table("LatLongIdNomeIdnilNomenil_263.txt",head=T,sep=" ")
latlong_db<-read.table("db_lat_long.txt",head=T,sep=" ")

latlong_db<-latlong_db%>%rename(nome=db,lat=MeanLat,long=MeanLong)

nodes<-latlong_staz%>%select(id,lat,long)%>%rename(nome=id)%>%filter(nome %in% clustered_flows$inizio_db)%>%
  bind_rows(latlong_db)%>%mutate(id=1:67)

geonodes<-nodes%>%select(id,nome)

geonodes_attr<-nodes%>%select(lat,long)

geoedges<-rete%>%select(Source,Target,Flow,prediction,cluster)

# Attenzione! Ricordati sempre di cambiare il file destinazione (output=...) PRIMA di lanciare quest'ultimo comando, altrimenti
# sovrascrive il file esistente senza dare avvertimenti

write.gexf(nodes = geonodes,edges = geoedges[,1:2],edgesWeight = geoedges$Flow,edgesAtt = geoedges[,4:5],
           nodesAtt = geonodes_attr,defaultedgetype="directed",output="C:/Users/nb2/Desktop/Politecnico/StatBayes/BayesMi/Gephi/rete_cluster.gexf")
