getVarianten<- function(eventlog){
  cases<-unique(eventlog$caseID)
  maxVar<- max(table(eventlog$caseID))

  varianten<- NULL
  dauer<-NULL
  for(i in 1:length(cases)){
    case<- eventlog[which(eventlog$caseID==cases[i]),]
    variante<- c(case$akt, rep("X", maxVar-length(case$akt)))
    varianten<- rbind(varianten, variante)
    
    time<- case$timestamp
    dur<- time[-1]-time[-length(time)]
    print("DAUER AUSGEBEN!!!")
    print(head(dur))
    dur<-c(dur, rep(0, maxVar-length(dur)-1))
    dauer<- rbind(dauer, c(cases[i],dur))
  }
  dauer<- data.frame(dauer)
  colnames(dauer)<-c("caseID", colnames(dauer)[-1])
  
  ncol_dauer<-dim(dauer)[2]
  ncol_var<- dim(varianten)[2]
  
  tableDV<-data.frame(Anzahl=1,dauer[-1], varianten,stringsAsFactors = F)
  table_agg<-aggregate.data.frame(as.matrix(tableDV[,1:ncol_dauer]), by=as.list(tableDV[,(ncol_dauer+1):dim(tableDV)[2]]), sum)
  
  
  dauer_agg<-table_agg[,-(1:(ncol_dauer))]
  dauer_agg<- data.frame(ID=1:dim(dauer_agg)[1],dauer_agg, stringsAsFactors = F)
  var_agg<-table_agg[,(1:(ncol_dauer))] #Anzahl fehlt noch
  var_agg<- data.frame(ID=1:dim(var_agg)[1], Anzahl=table_agg$Anzahl, var_agg, stringsAsFactors = F)
  
  return(list(var=var_agg, dauer=dauer_agg))
  
  
  
  #return(var_agg)
  # #Doppelte Varianten löschen
  # print("-------------")
  # print(dim(table_agg))
  # print(str(table_agg))
  # print(dim(dauer_agg))
  # print(dim(var_agg))
  # print(str(dauer_agg))
  # print(str(var_agg))
  # print(str(varianten))
  # countVar<-count(varianten,vars=colnames(varianten))
  # varAnzahl<-countVar$freq
  # countVar<- countVar[,-dim(countVar)[2]]
  # countVar<- data.frame(ID=0,DuplicateOf=2,Anzahl=varAnzahl,as.matrix(countVar),  stringsAsFactors = F)
  # print(str(countVar))
  # 
  # #Duplikates Finden Varianten Finden
  # cases<-eventlog$caseID
  # isDuplicate<-duplicated(varianten)
  # noDuplicate<- varianten[!isDuplicate,]
  # id<- unique(cases[isDuplicate])
  # #countVar$ID<-id
  # duplicate<- varianten[isDuplicate,]
  # 
  # 
  # 
  # 
  # print("WICHTIG!!!")
  # test<- data.frame(dauer, varianten)
  # #print(head(test))
  # print(head(aggregate.data.frame(as.matrix(test[,1:22]), by=as.list(test[,23:40]), sum)))
  # print(dim(aggregate.data.frame(as.matrix(test[,1:22]), by=as.list(test[,23:40]), sum)))
  # 
  # # print(dim(varianten))
  # # print(dim(dauer))
  # # print(length(id))
  # # print(length(dauer$caseID))
  # # print(all.equal(id,dauer$caseID))
  # return(countVar)
  # 
  # #countVar<- data.frame(as.matrix(countVar), Anzahl=varAnzahl, stringsAsFactors = F)
  # #colnames(countVar)<-c(colnames(countVar)[-length(colnames(countVar))],"Anzahl")
  # 
  # #print(str(countVar))
  # #return(countVar)
  # 
  # #  anzKennzahl<-NULL
  # #  varId<- NULL
  # # for(i in 1:dim(varianten)[1]){
  # #   anz<-1
  # #   if(i>dim(varianten)[1]){
  # #     break
  # #   }
  # #   varId<- c(varId, cases[i])
  # #   for(j in i:dim(varianten)[1]){
  # #     if(j>dim(varianten)[1]){
  # #       break
  # #     }
  # #     if(identical(varianten[i,],varianten[j,])&& i!=j){
  # #       anz<-anz+1
  # #       dauer[which(dauer$caseId==cases[j]),"caseID"]<- cases[i]
  # #       varianten<-varianten[-j,]
  # #       cases<- cases[-j]
  # #     }
  # #   }
  # #   anzKennzahl<- c(anzKennzahl,anz)
  # # }
  # # 
  # # varianten_Anz<- data.frame(varianten,Anzahl=anzKennzahl, stringsAsFactors = F)
  # # # rownames(varianten_Anz)<-NULL
  # # print(str(varianten_Anz))
  # # 
  # 
  # 
}


head(eventlog)
