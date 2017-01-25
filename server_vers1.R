server_end <- function(input, output) { 
  
  getVarianten<- reactive({
    eventlog<- evLog
    cases<-unique(eventlog$caseID)
    maxVar<- max(table(eventlog$caseID))
    
    varianten<- NULL
    
    for(i in 1:length(cases)){
      case<- eventlog[which(eventlog$caseID==cases[i]),]
      variante<- c(case$akt, rep("X", maxVar-length(case$akt)))
      varianten<- rbind(varianten, variante)
    }
    
    anzVar<-NULL
    
    for(i in 1:dim(varianten)[1]){
      anz<-1
      if(i>dim(varianten)[1]){
        break
      }
      for(j in 1:dim(varianten)[1]){
        
        if(identical(varianten[i,],varianten[j,])&& i!=j){
          anz<-anz+1 
          varianten<-varianten[-j,]
        }
      }
      anzVar<- c(anzVar,anz)
    }

    varianten_Anz<- cbind(varianten,anzKennzahl)
    
    return(list(anzVar, varianten))
  })#getVarianten
  
  getStartNodes<- reactive({
    varianten<-data.frame(unlist(list(varianten, anzKennzahl)[[1]]), stringsAsFactors = F)
    start<-  unique(varianten[,1])
    return(start)
  })#getStart
  
  getBezWithX<- reactive({
    varianten<-data.frame(unlist(getVarianten()[[1]]), stringsAsFactors = F)
    
    bez<-NULL
    
    for(i in 1:dim(varianten)[1]){
      for(j in 1:(dim(varianten)[2]-1)){
        bez_help<-c(varianten[i,j], varianten[i,j+1])
        bez_sub<-bez[which(bez[,1]==bez_help[1]),]
        bez_sub<- bez_sub[which(bez_sub[2]==bez_help[2])]
        bez<-rbind(bez, bez_help)
        
      }
    }
    return(bez)
  })#getBezWithX
  
  getEndNodes<- reactive({
    bez<- getBezWithX()
    varianten<-data.frame(unlist(getVarianten()[[1]]), stringsAsFactors = F)
    
    end_help<-bez[which(bez[,2]=="X"),1]
    endEvents<- varianten[,dim(varianten)[2]]
    endEvents<-unique(c(end_help,endEvents))
    endEvents<- endEvents[which(endEvents!="X")]
    
    return(endEvents)
  })# getEndNodes
  
  getBezWithoutX<- reactive({
    bez<- getBezWithX()
    bez<- bez[which(bez[,1]!="X"),]
    bez<- bez[which(bez[,2]!="X"),]
    
    return(bez)
  })
  
  getWithoutDuplicates<- reactive({
    
  })
}

runApp(list(ui=ui, server=server_end))