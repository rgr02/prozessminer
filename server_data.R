server_proto<- function(input,output){
  
  output$networkVis<- renderVisNetwork({
    
    varianten_Anz<- getVarianten(eventlog)

    varianten<- varianten_Anz[,-dim(varianten_Anz)[2]]
    
    output$abdeckung<- renderPlot({
      anzVar<-sort(varianten_Anz[,dim(varianten_Anz)[2]],decreasing = T)
      #barplot(anzVar,col = "#58ACFA", names.arg = paste0("Var",1:length(anzVar)))
      anzV<-data.frame(Variante=paste0("V",1:length(anzVar)),Anzahl=anzVar, stringsAsFactors = F)
      print(head(anzV))
      
      
      g <- ggplot(anzV, aes(reorder(Variante, -Anzahl),weight= Anzahl))
      g + geom_bar()
      
    })
    
    startEvents<- unique(varianten[,1])
    
    output$statistics<- renderPrint({
      anzC<- sum(varianten_Anz[,dim(varianten_Anz)[2]])
      anzV<- dim(varianten)[1]
      data.frame(row.names = c("Anzahl Cases", "Anzahl Varianten"), val= c(anzC, anzV))      
      
      
    })
    
    ###Beziehungen
    bez<-NULL
    
    for(i in 1:dim(varianten)[1]){
      for(j in 1:(dim(varianten)[2]-1)){
        bez_help<-c(varianten[i,j], varianten[i,j+1])
        bez_sub<-bez[which(bez[,1]==bez_help[1]),]
        bez_sub<- bez_sub[which(bez_sub[2]==bez_help[2])]
        bez<-rbind(bez, bez_help)
        
      }
    }
    
    #Endknoten
    end_help<-bez[which(bez[,2]=="X"),1]
    endEvents<- varianten[,dim(varianten)[2]]
    endEvents<-unique(c(end_help,endEvents))
    endEvents<- endEvents[which(endEvents!="X")]
    
    #Beziehungen X entfernen
    bez<- bez[which(bez[,1]!="X"),]
    bez<- bez[which(bez[,2]!="X"),]
    
    
    #doppelte Beziehungen entfernen
    alsWort<-unique(paste0(bez[,1],"-",bez[,2]))
    wieOftBez1<-table(paste0(bez[,1],"-",bez[,2]))
    namesBez<-  unlist(strsplit(names(wieOftBez1), split="-"))
    wieOftBez<- data.frame(wieOftBez1, matrix(namesBez, ncol = 2, byrow = T),stringsAsFactors = F)
    wieOftBez<- wieOftBez[-1]

    bez<-wieOftBez
   
    #Startpunkt erstellen
    startNodes<-data.frame(Startknoten="S", start=startEvents, stringsAsFactors = F)
    
    #Endknoten erstellen
    endNodes<-data.frame(end=endEvents, Endknoten="E", stringsAsFactors = F)
    
    
    #####Graph erstellen
    nodes1<-unique(c("E", "S",unique(bez$X1, bez$X2)))
    nodes<- data.frame(id=nodes1, label= nodes1,color=c("red","green",rep("#CECEF6",length(nodes1)-2)), x=c(1,rep(NULL,length(nodes1)-1)))
    from<- c(startNodes$Startknoten, endNodes$end,bez$X1)
    to<- c(startNodes$start, endNodes$Endknoten, bez$X2)
    edges<- data.frame(from=from, to=to, label=c(table(startEvents), table(endEvents),bez[,1]))
    
    
    visNetwork(nodes,edges)%>%visEdges(arrows = 'to')%>%
      visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}")%>%
      visIgraphLayout(randomSeed= 3)
    
  })#Network
  
  
}

runApp(list(ui=ui, server=server_proto))
