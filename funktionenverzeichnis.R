getVarianten<- function(eventlog){
  print("Funktion")
  cases<-unique(eventlog$caseID)
  maxVar<- max(table(eventlog$caseID))
  
  #Initialisierung der Tabellen
  d<-as.numeric(eventlog$timestamp[1]-eventlog$timestamp[1],units="secs")
  varianten<- data.frame(matrix(rep("X",maxVar*length(cases)),ncol=maxVar), stringsAsFactors = F)
  dauer<- data.frame(matrix(rep(d,maxVar*length(cases)),ncol=maxVar), stringsAsFactors = F)
  
  colNr<-0
  rowNr<-1
  casePre<- eventlog[1,"caseID"]
  caseThis<- eventlog[1,"caseID"]
  
  dauerPre<--1
  dauerThis<-eventlog[1,"timestamp"]
  
  #Erstellung Tabelle Varianten und Dauer
  for(i in 1:dim(eventlog)[1]){
    
    caseThis<-eventlog[i,"caseID"]
    aktThis<-eventlog[i,"akt"]
    if(caseThis!=casePre){
      colNr<-1
      rowNr<-rowNr+1
      casePre<-caseThis
      dauerPre<- -1
    }else{
      colNr<-colNr+1
      
      dauerPre<-dauerThis
      dauerThis<-eventlog[i,"timestamp"]
    }
    
    if(dauerPre!= -1 & colNr!=1){
      dauer[rowNr,colNr-1]<-as.numeric(dauerThis-dauerPre,units="secs")
      dauerPre<-dauerThis
    }
    varianten[rowNr,colNr]<-aktThis
    if((i %% 1000)==0){
      print(i)
    }
  }
  
  
  dauer<- data.frame(dauer)
  
  ncol_dauer<-dim(dauer)[2]
  ncol_var<- dim(varianten)[2]
  
  #Tabelle mit Dauer und Varianten
  tableDV<-data.frame(Anzahl=1,dauer[-1], varianten,stringsAsFactors = F)
  
  #Tabelle DV aufaggregiert -> keine doppelten Varianten
  table_agg<-aggregate.data.frame(as.matrix(tableDV[,1:ncol_dauer]), by=as.list(tableDV[,(ncol_dauer+1):dim(tableDV)[2]]), sum)
  
  #Herauslesen aus table_agg die aggregierte Dauer und Varianten
  dauer_agg<-table_agg[,-(1:(ncol_dauer))]
  dauer_agg<- data.frame(ID=1:dim(dauer_agg)[1],dauer_agg, stringsAsFactors = F)
  var_agg<-table_agg[,(1:(ncol_dauer))] #Anzahl fehlt noch
  var_agg<- data.frame(ID=1:dim(var_agg)[1], Anzahl=table_agg$Anzahl, var_agg, stringsAsFactors = F)
  
  
  
  return(list(var=var_agg, dauer=dauer_agg))
  
  
}


