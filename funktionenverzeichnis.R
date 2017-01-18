getVarianten<- function(eventlog){
  cases<-unique(eventlog$caseID)
  maxVar<- max(table(eventlog$caseID))
  varianten<- NULL
  
  time<- eventlog$timestamp
  dur<- time[-1]-time[-length(time)]
  eventlog
  dauer<- data.frame(caseId=eventlog$caseID[-1], dauer=dur)
  print(dauer[1,"caseId"])
  
  for(i in 2: dim(dauer)[1]){
   if(i>dim(dauer)[1]){
     break
   }
   if(dauer[i-1,"caseId"]!=dauer[i,"caseId"]){
     dauer<- dauer[-i,]
   }
  }
  

   print(head(dauer))
  
  for(i in 1:length(cases)){
    case<- eventlog[which(eventlog$caseID==cases[i]),]
    variante<- c(case$akt, rep("X", maxVar-length(case$akt)))
    varianten<- rbind(varianten, variante)
  }
  #varianten<-as.data.frame(varianten, stringsAsFactors=F)
  print("dimensionen varianten")
  #print(varianten[!duplicated(varianten), ])
  countVar<-count(varianten,vars=colnames(varianten))
  #print(ddply(varianten,.(colnames(varianten)),nrow))
  #Doppelte Varianten löschen
  #varianten<- varianten[,-1]
  
  countVar<-as.data.frame(count(varianten,vars=colnames(varianten)),stringsAsFactors=F)
  colnames(countVar)<-c(colnames(countVar)[-length(colnames(countVar))],"Anzahl")
  print(str(countVar))
  #return(countVar)
  
   anzKennzahl<-NULL
   varId<- NULL
  for(i in 1:dim(varianten)[1]){
    anz<-1
    if(i>dim(varianten)[1]){
      break
    }
    varId<- c(varId, cases[i])
    for(j in i:dim(varianten)[1]){
      if(j>dim(varianten)[1]){
        break
      }
      if(identical(varianten[i,],varianten[j,])&& i!=j){
        anz<-anz+1
        dauer[which(dauer$caseId==cases[j]),"caseId"]<- cases[i]
        varianten<-varianten[-j,]
        cases<- cases[-j]
      }
    }
    anzKennzahl<- c(anzKennzahl,anz)
  }
  print(dim(varianten))
  print(varId)
  print(sum(anzKennzahl))
  print((unique(dauer$caseId)))

  varianten_Anz<- data.frame(varianten,Anzahl=anzKennzahl, stringsAsFactors = F)
  # rownames(varianten_Anz)<-NULL
  print(str(varianten_Anz))
  
  return(varianten_Anz)
  
  
}


head(eventlog)
