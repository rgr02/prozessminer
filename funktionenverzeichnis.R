getVarianten<- function(eventlog){
  cases<-unique(eventlog$caseID)
  maxVar<- max(table(eventlog$caseID))
  varianten<- NULL
  
  for(i in 1:length(cases)){
    case<- eventlog[which(eventlog$caseID==cases[i]),]
    variante<- c(case$akt, rep("X", maxVar-length(case$akt)))
    varianten<- rbind(varianten, variante)
  }
  anzKennzahl<-NULL
  
  
  #Doppelte Varianten löschen
  for(i in 1:dim(varianten)[1]){
    anz<-1
    if(i>dim(varianten)[1]){
      break
    }
    for(j in i:dim(varianten)[1]){
      if(j>dim(varianten)[1]){
        break
      }
      if(identical(varianten[i,],varianten[j,])&& i!=j){
        anz<-anz+1 
        varianten<-varianten[-j,]
      }
    }
    anzKennzahl<- c(anzKennzahl,anz)
  }
  varianten_Anz<- data.frame(varianten,anzKennzahl, stringsAsFactors = F)
  rownames(varianten_Anz)<-NULL
  return(varianten_Anz)
}

