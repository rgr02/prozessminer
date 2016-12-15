server_proto<- function(input,output){
  
    
    varianten_Anz<- getVarianten(eventlog)

    varianten_all<- varianten_Anz[,-dim(varianten_Anz)[2]]
    
    output$abdeckung<- renderPlot({
      anzVar<-sort(varianten_Anz[,"Anzahl"],decreasing = T)
      anzV<-data.frame(Variante=paste0("V",1:length(anzVar)),Anzahl=anzVar, stringsAsFactors = F)
      
      if(!is.null(input$plot_brush)){
        start<- round(input$plot_brush$xmin)
        end<- round(input$plot_brush$xmax)
        col_bar<- rep("grey",length(anzVar))
        col_bar[start:end]<-"blue"
        varianten<- varianten_Anz[order(-varianten_Anz$Anzahl),]
        varianten<- varianten[start:end,]
      }else{
        col_bar<- rep("blue",length(anzVar))
      }
      
      
      g <- ggplot(anzV, aes(reorder(Variante, -Anzahl),weight= Anzahl))
      g + geom_bar(fill=col_bar)
      
    })
 
####Ab hier individuell von Auswahl abhängig 
    
    varianten_sub<- reactive({
      if(!is.null(input$plot_brush)){
        start<- round(input$plot_brush$xmin)
        end<- round(input$plot_brush$xmax)
        
        varianten<- varianten_Anz[order(-varianten_Anz$Anzahl),]
        varianten<- varianten[start:end,]
      }else{
        varianten<- getVarianten(eventlog)
      }
    })
    
    output$statistics<- renderPrint({
      varianten<- varianten_sub()
      #print(head(varianten))
      
      anzC<- sum(varianten$Anzahl)
      anzV<- dim(varianten)[1]
      data.frame(row.names = c("Anzahl Cases", "Anzahl Varianten"), val= c(anzC, anzV))      
      
      
    })
    
  ##Beziehungen
    bez_X<- reactive({
      bez<-NULL
      varianten<-varianten_sub()
      varianten<- varianten[,-dim(varianten)[2]]
      for(i in 1:dim(varianten)[1]){
        for(j in 1:(dim(varianten)[2]-1)){
          bez_help<-c(varianten[i,j], varianten[i,j+1])
          bez_sub<-bez[which(bez[,1]==bez_help[1]),]
          bez_sub<- bez_sub[which(bez_sub[2]==bez_help[2])]
          bez<-rbind(bez, bez_help)
          
        }
      }
      return(bez)
    })#beziehungen mi A X beziehungen
    
   
    
    startEvents<- reactive({
      varianten<- varianten_sub()
      startEvents<- unique(varianten[,1])
    })

    endEvents<-reactive({
      varianten<- varianten_sub()
      varianten<- varianten[,-dim(varianten)[2]]
      bez<- bez_X()
      end_help<-bez[which(bez[,2]=="X"),1]
      endEvents<- varianten[,dim(varianten)[2]]
      endEvents<-unique(c(end_help,endEvents))
      endEvents<- endEvents[which(endEvents!="X")]
    })
    #Beziehungen X entfernen
    beziehungen<- reactive({
      bez<-bez_X()
      bez<- bez[which(bez[,1]!="X"),]
      bez<- bez[which(bez[,2]!="X"),]
      
      #doppelte Beziehungen entfernen
      alsWort<-unique(paste0(bez[,1],"-",bez[,2]))
      wieOftBez1<-table(paste0(bez[,1],"-",bez[,2]))
      namesBez<-  unlist(strsplit(names(wieOftBez1), split="-"))
      wieOftBez<- data.frame(wieOftBez1, matrix(namesBez, ncol = 2, byrow = T),stringsAsFactors = F)
      wieOftBez<- wieOftBez[-1]
      
      bez<-wieOftBez
      return(bez)
    })
    
  # 
    output$aktivitaeten<- renderPlot({
      bez<- beziehungen()
      
      anzAkt<-aggregate(bez$Freq, by=list(bez$X2), sum)
      anzAkt<- data.frame(anzAkt)
      barplot(anzAkt$x,names.arg = anzAkt$Group.1)

      g <- ggplot(anzAkt, aes(Group.1,weight= x))
      g + geom_bar()+
      theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5))
    })

    #####Graph erstellen
    output$networkVis<- renderVisNetwork({
    bez<-beziehungen()
    startEvents<-startEvents()
    endEvents<- endEvents()
    #Startpunkt erstellen
    startNodes<-data.frame(Startknoten="S", start=startEvents, stringsAsFactors = F)
    
    #Endknoten erstellen
    endNodes<-data.frame(end=endEvents, Endknoten="E", stringsAsFactors = F)
    
    
    nodes1<-unique(c("E", "S",unique(bez$X1, bez$X2)))
    nodes<- data.frame(id=nodes1, label= nodes1,color=c("red","green",rep("#CECEF6",length(nodes1)-2)), x=c(1,rep(NULL,length(nodes1)-1)))
    from<- c(startNodes$Startknoten, endNodes$end,bez$X1)
    to<- c(startNodes$start, endNodes$Endknoten, bez$X2)
   
    #valueE<-c(table(startEvents), table(endEvents),bez[,1])
    #valueE[which(valueE>30)]<-30
    #valueE<- c(100, valueE[-1])
    
    if(input$anzeige =="Dauer"){
      labelE<-NA
    }else{
      labelE<- c(table(startEvents), table(endEvents),bez[,1])
    }
    edges<- data.frame(from=from, to=to,label=labelE)#, value=valueE)
    

    visNetwork(nodes,edges, width="100%")%>%visEdges(arrows = 'to')%>%
      visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}")%>%
      visIgraphLayout(randomSeed= 46)%>%
      visEdges(smooth= list(enabled = TRUE, type = "horizontal"))
  })#Network
  
    ######Nur zur Veranschaulichung
    output$matrix<- renderPrint({
      zz<- runif(13*13, min=0, max=1)
      m<-matrix(round(zz,2), ncol = 13, nrow=13)
      m1<-as.data.frame(m)
      
      print(m1)
    })
  
}

runApp(list(ui=ui, server=server_proto))
