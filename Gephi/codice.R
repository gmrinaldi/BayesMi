# Rete per gephi
library(dplyr,tidyr)
library(rgexf)
rete<-bike_NIL_NewDay%>%group_by(StazioneInizio,StazioneFine)%>%summarise(Flow=n())%>%
  ungroup()%>%rename(Source=StazioneInizio,Target=StazioneFine)
latlong<-read.table("LatLongIdNomeIdnilNomenil_263.txt",head=T,sep=" ")

nodes<-latlong%>%select(id,nome,lat,long)
# Occhio che l'id viene modificato! Non usare questi se è importante.
nodes$id<-1:263

geonodes<-nodes%>%select(id,nome)
geonodes$nome<-as.character(geonodes$nome)

# Correzione
geonodes$nome[91]<-"San Lorenzo - \"Colonne\""
geonodes$nome[180]<-"Donizetti \"Provincia\" - RIMOSSA PROVVISORIAMENTE"
geonodes$nome[217]<-"M. Gioia - \"Regione\""

geonodes_attr<-nodes%>%select(lat,long)

edges<-rete%>%left_join(geonodes,by=c("Source"="nome"))%>%select(id,Target,Flow)%>%
    rename(IDS=id)

geoedges<-edges%>%left_join(geonodes,by=c("Target"="nome"))%>%select(IDS,id,Flow)%>%
    rename(IDT=id)

# Necessario per corretto encoding

geonodes$nome[47]<-"Santissima Trinita"
geonodes$nome[151]<-"Cantu"
geonodes$nome[198]<-"Universita Bocconi"
geonodes$nome[235]<-"Universita Cattolica"
geonodes$nome[91]<-"San Lorenzo - Colonne"
geonodes$nome[180]<-"Donizetti Provincia - RIMOSSA PROVVISORIAMENTE"
geonodes$nome[217]<-"M. Gioia - Regione"

write.gexf(nodes = geonodes,edges = geoedges[,1:2],edgesWeight = geoedges$Flow,
           nodesAtt = geonodes_attr,defaultedgetype="directed",output="C:/Users/nb2/Desktop/Politecnico/StatBayes/BayesMi/Gephi/rete.gexf")

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
  
write.gexf(nodes = geonodes,edges = geoedges[,1:2],edgesWeight = geoedges$Flow,edgesAtt = geoedges[,4:5],
           nodesAtt = geonodes_attr,defaultedgetype="directed",output="C:/Users/nb2/Desktop/Politecnico/StatBayes/BayesMi/Gephi/rete_cluster.gexf")

# Rete per gephi (dbscan)
library(tidyverse)
library(rgexf)
rete<-bike_NIL_NewDay%>%group_by(StazioneInizio,StazioneFine)%>%summarise(Flow=n())%>%
  ungroup()%>%rename(Source=StazioneInizio,Target=StazioneFine)
latlong<-read.table("LatLongIdNomeIdnilNomenil_263.txt",head=T,sep=" ")

nodes<-latlong%>%select(id,nome,lat,long)
# Occhio che l'id viene modificato! Non usare questi se è importante.
nodes$id<-1:263

geonodes<-nodes%>%select(id,nome)
geonodes$nome<-as.character(geonodes$nome)

# Correzione
geonodes$nome[91]<-"San Lorenzo - \"Colonne\""
geonodes$nome[180]<-"Donizetti \"Provincia\" - RIMOSSA PROVVISORIAMENTE"
geonodes$nome[217]<-"M. Gioia - \"Regione\""

geonodes_attr<-nodes%>%select(lat,long)%>%mutate(db=cluster$cluster)

edges<-rete%>%left_join(geonodes,by=c("Source"="nome"))%>%select(id,Target,Flow)%>%
  rename(IDS=id)

geoedges<-edges%>%left_join(geonodes,by=c("Target"="nome"))%>%select(IDS,id,Flow)%>%
  rename(IDT=id)

# Necessario per corretto encoding

geonodes$nome[47]<-"Santissima Trinita"
geonodes$nome[151]<-"Cantu"
geonodes$nome[198]<-"Universita Bocconi"
geonodes$nome[235]<-"Universita Cattolica"
geonodes$nome[91]<-"San Lorenzo - Colonne"
geonodes$nome[180]<-"Donizetti Provincia - RIMOSSA PROVVISORIAMENTE"
geonodes$nome[217]<-"M. Gioia - Regione"

write.gexf(nodes = geonodes,edges = geoedges[,1:2],edgesWeight = geoedges$Flow,
           nodesAtt = geonodes_attr,defaultedgetype="directed",output="C:/Users/nb2/Desktop/Politecnico/StatBayes/BayesMi/Gephi/rete_dbscan.gexf")


