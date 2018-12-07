# Rete per gephi
library(dplyr,tidyr,rgexf)
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
