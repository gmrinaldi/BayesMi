load("bike_NIL_NewDay[1].Rdata")

library(igraph)

attach(bike_NIL_NewDay)

adj<-matrix(rep(0,39^2),39,39)
NILnames<-unique(numeroNILinizio)
#1  7 11 12 68  5 69 21 49 70  4 22 10 14  2 26 50 20 51 71  9 78 77 44  8  6 27 79 37 67 35 59 36 58  3 13 15 66 65


for (i in 1:length(numeroNILfine)){
  idi<-NILnames==numeroNILinizio[i]
  idj<-NILnames==numeroNILfine[i]
  adj[idi,idj]<-adj[idi,idj]+1
}

traveli<-rep(0,39)
for (i in 1:39){
  traveli[i]<-sum(adj[i,])
}


graph<-graph_from_adjacency_matrix(adj, mode = c("directed"), diag = FALSE,
                            add.colnames = NULL, add.rownames = NA)

#tkplot(graph)

authority<-authority_score(graph, scale = TRUE,
                options = arpack_defaults)
authority$vector #first eigenvector
authority$value # first eigenvalue
NILnames[which(authority$vector==max(authority$vector))]

hub<-hub_score(graph, scale = TRUE, options = arpack_defaults)
hub$vector
hub$value

clout<-closeness(graph, vids = V(graph), mode = c("out"), normalized = FALSE)

max(clout) #max vicinanza-out
NILnames[which(clout==max(clout))] #71: Villapizzone

clin<-closeness(graph, vids = V(graph), mode = c("in"), normalized = FALSE)

max(clin) #max vicinanza-out
NILnames[which(clin==max(clin))] #66: QT8

degout<-degree(graph, v = V(graph), mode = c("out"),
       loops = TRUE, normalized = FALSE)
degout #grado totale di ogni nodo

degin<-degree(graph, v = V(graph), mode = c("in"),
               loops = TRUE, normalized = FALSE)
degin #grado totale di ogni nodo


# 
# This function calculates the optimal community structure 
# of a graph, by maximizing the modularity measure over all possible partitions.

#unico comando che ho trovato per reti dirette!

clusters<-cluster_optimal(graph)  #3 cluster che massimizzano la modularit?

clusters$modularity 
#la modularit? quantifica la differenza di densit? inter-intra comunit?
clusters$membership #vettore di appartenenza ai clusters
cl<-clusters$membership

NILnames[which(cl==1)]
#1  7  5 49  4 26 50 44  6 27 37 35 36

NILnames[which(cl==2)]
#11 12 21 22 10 14 20  9 78 77 79  3 13 15

NILnames[which(cl==3)]
# 68 69 70  2 51 71  8 67 59 58 66 65

#osservando la cartina di Milano, i cluster sembrano avere senso per vicinanza

kcore<-coreness(graph, mode = c("all"))
kcore

#stima s e d sulle stazioni


adj<-matrix(rep(0,263^2),263,263)
staznames<-unique(NumeroStzInizio)

for (i in 1:length(NumeroStzInizio)){
  idi<-staznames==NumeroStzInizio[i]
  idj<-staznames==NumeroStzFine[i]
  adj[idi,idj]<-adj[idi,idj]+1
}

graph<-graph_from_adjacency_matrix(adj, mode = c("directed"),weighted=TRUE, diag = FALSE,
                                   add.colnames = NULL, add.rownames = NA)

S<-degree(graph, v = V(graph), mode = c("out"),
               loops = TRUE, normalized = FALSE)
S #grado totale di ogni nodo

T<-degree(graph, v = V(graph), mode = c("in"),
              loops = TRUE, normalized = FALSE)
T #grado totale di ogni nodo

kcore<-coreness(graph, mode = c("all"))
kcore
unique(kcore)
min(kcore)
max(kcore)



detach(bike_NIL_NewDay)
