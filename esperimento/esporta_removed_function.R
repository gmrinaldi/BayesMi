esporta_removed <- function(DF) {
require(rgexf)
  
rete<-DF%>%rename(Source=id_inizio,Target=id_fine)

latlong_staz<-read.table("LatLongIdNomeIdnilNomenil_263.txt",head=T,sep=" ")
latlong_db<-read.table("db_lat_long.txt",head=T,sep=" ")

latlong_db<-latlong_db%>%rename(nome=db,lat=MeanLat,long=MeanLong)

nodes<-latlong_staz%>%select(id,lat,long)%>%rename(nome=id)%>%filter(nome %in% modified_flows$inizio_db)%>%
  bind_rows(latlong_db)%>%mutate(id=1:67)

geonodes<-nodes%>%select(id,nome)

geonodes_attr<-nodes%>%select(lat,long)

geoedges<-rete%>%filter(relchange>0,relchange<1000)%>%select(Source,Target,relchange,prediction,modified,segno)

write.gexf(nodes = geonodes,edges = geoedges[,1:2],edgesWeight = geoedges$relchange,edgesAtt = geoedges[,4:6],
           nodesAtt = geonodes_attr,defaultedgetype="directed",output="BayesMi/Gephi/rete_modificata.gexf")
}
