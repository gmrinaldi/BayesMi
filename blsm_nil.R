setwd("C:/Users/Celeste/Desktop/netw")
load("bike_NIL_NewDay[1].Rdata")

attach(bike_NIL_NewDay)

adj<-matrix(rep(0,39^2),39,39)
NILnames<-unique(numeroNILinizio)

for (i in 1:length(numeroNILfine)){
  idi<-NILnames==numeroNILinizio[i]
  idj<-NILnames==numeroNILfine[i]
  adj[idi,idj]<-adj[idi,idj]+1
}

for (i in 1:39)
  adj[i,i]<-0

detach(bike_NIL_NewDay)

rownames(adj)<-NILnames
colnames(adj)<-NILnames

library(BLSM)

adj2<-matrix(as.numeric(adj>=1),39)

est<-estimate_latent_positions(adj2[1:37,1:37],adj[1:37,1:37],odens=5000,burn_in = 10^6,nscan=10^6)

plot_traceplots_acf(est)

plot_latent_positions(est, colors=rep("darkorange",39), points_size = 0.1,
                      labels_point_size = 1, labels_point_color = "orange",
                      labels_text_size = 1, labels_text_color = "red", circles_2D = FALSE)

save.image()
