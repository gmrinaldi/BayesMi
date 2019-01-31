multimod_rm <- function(DF, id_names, d, alphaF, alphaT) { 
  # alpha ragionevole fra 100 e 500
  
  NewFrom<-DF%>%group_by(id_inizio)%>%summarize(Sourcity=sum(Flow))%>%ungroup()
  NewTo<-DF%>%group_by(id_fine)%>%summarize(Targettosity=sum(Flow))%>%ungroup()
  
  for (i in 1:length(id_names)){
    fromname<-paste("NewFromPart",i,sep="")
    toname<-paste("NewToPart",i,sep="")
    # scale_factor_from<-(dim(NewFrom)[1]-1)/(sum(exp(-as.numeric(d[id_names[i],])/alphaF))-1)
    # scale_factor_to<-(dim(NewFrom)[1]-1)/(sum(exp(-as.numeric(d[id_names[i],])/alphaT))-1)
    # 
    # NewFrom<-NewFrom%>%mutate(!!fromname := (sum(Sourcity)/sum(Sourcity*(id_inizio!=id_names[i]))-1)*scale_factor_from*exp(-as.numeric(d[id_names[i],])/alphaF))
    # NewTo<-NewTo%>%mutate(!!toname := (sum(Targettosity)/sum(Targettosity*(id_fine!=id_names[i]))-1)*scale_factor_to*exp(-as.numeric(d[id_names[i],])/alphaT))

    scale_factor_from<-NewFrom%>%summarize(scale=sum(Sourcity*(id_inizio!=id_names[i]))/sum((Sourcity*(id_inizio!=id_names[i])*exp(-as.numeric(d[id_names[i],])/alphaF))))%>%.$scale
    scale_factor_to<-NewTo%>%summarize(scale=sum(Targettosity*(id_fine!=id_names[i]))/sum((Targettosity*(id_fine!=id_names[i])*exp(-as.numeric(d[id_names[i],])/alphaF))))%>%.$scale

    NewFrom<-NewFrom%>%mutate(!!fromname := (sum(Sourcity)/sum(Sourcity*(id_inizio!=id_names[i]))-1)*scale_factor_from*exp(-as.numeric(d[id_names[i],])/alphaF))
    NewTo<-NewTo%>%mutate(!!toname := (sum(Targettosity)/sum(Targettosity*(id_fine!=id_names[i]))-1)*scale_factor_to*exp(-as.numeric(d[id_names[i],])/alphaT))
    
    
  }
  
  NewFrom<-NewFrom%>%mutate(NewSource=Sourcity*(!(id_inizio%in%id_names)),
                            NewFrom=NewSource/sum(Sourcity)*(1+rowSums(.[2+1:length(id_names)])))%>%
    select(id_inizio,NewFrom)
  
  
  NewTo<-NewTo%>%mutate(NewTarget=Targettosity*(!(id_fine%in%id_names)),
                        NewTo=NewTarget/sum(Targettosity)*(1+rowSums(.[2+1:length(id_names)])))%>%
    select(id_fine,NewTo)
  
  NewAcc<-DF%>%left_join(NewFrom,by="id_inizio")%>%left_join(NewTo,by="id_fine")
  
  return(NewAcc)
  
}


