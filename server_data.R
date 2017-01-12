server_proto<- function(input,output){
  
    
    varianten_Anz<- getVarianten(eventlog)

    varianten_all<- varianten_Anz[,-dim(varianten_Anz)[2]]
    
    output$abdeckung<- renderPlot({
      anzVar<-sort(varianten_Anz[,"Anzahl"],decreasing = T)
      anzV<-data.frame(Variante=paste0("V",1:length(anzVar)),Anzahl=anzVar, stringsAsFactors = F)
      
      if(!is.null(input$plot_brush)){
        start<- round(input$plot_brush$xmin)
        end<- round(input$plot_brush$xmax)
        if(start<0){
          start<-0
        }
        if(end>length(anzVar)){
          end<-length(anzVar)
        }
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
    
    varianten_sub_act<- reactive({
      if(!is.null(input$plot_brush)){
        start<- round(input$plot_brush$xmin)
        end<- round(input$plot_brush$xmax)
        
        varianten<- varianten_Anz[order(-varianten_Anz$Anzahl),]
        varianten<- varianten[start:end,]
      }else{
        print(input$pql)
        varianten<- getVarianten(eventlog)
      }
    })
    
    varianten_sub_pql<-eventReactive(input$pqlButton,{
      varianten<-varianten_sub_act()
      eingabe<-gsub(" ", "", input$pql, fixed = TRUE)
      eingabe<-unlist(strsplit(eingabe,"->"))
      eingabe[which(eingabe=="*")]<-"[a-z]*"
      eingabe<- paste(eingabe, collapse="")
      eingabe<-tolower(eingabe)
      print("joer")
      print(eingabe)
      varWort<-NULL
      
      for(i in 1:dim(varianten)[1]){
        var_ohne_space<-gsub(" ","", varianten[i,], fixed = T)
        wort<-paste(var_ohne_space, collapse = "")
        varWort<-c(varWort,wort)
      }
      varWort<-tolower(varWort)
      rows<-grep(eingabe, varWort, perl=TRUE, value=FALSE)
      print(varWort)
      print(rows)
      varianten<- varianten[rows,]
      # for(i in 1:length(eingabe)){
      #   if(eingabe[i]!="*"& eingabe[i]!="?"){
      #     var_ohne_space<-gsub(" ","", varianten[,i], fixed = T)
      #     varianten<-varianten[which(var_ohne_space==eingabe[i]),]
      #   }
        # if(eingabe[i]=="?"){
        #   index<-which(varianten==eingabe[i+1], arr.ind=TRUE)
        #   index<- index[which(index$col)>i,]
        # }
      #}
      #print(varianten)
    })
    
    varianten_sub<-eventReactive(input$pqlButton,{
      output$pqlWarning<-renderText({
        
      })
      if(input$pql==""){
        varianten_sub_act()
      }else{
        subpql<-varianten_sub_pql()
        if(dim(subpql)[1]==0){
          output$pqlWarning<-renderText({
            "Keine Varianten mit dieser Auswahl -> Gesamtes Dashboard mit Auswahl in Abdeckung wird angezeigt"
          })
          varianten_sub_act()
        }else{
          return(subpql)
        }
      }
      
    })
  
    output$statistics<- renderTable({
      varianten<- varianten_sub()

      anzC<- sum(varianten$Anzahl)
      anzV<- dim(varianten)[1]
      data.frame(Variable = c("Anzahl Cases", "Anzahl Varianten"), val= c(anzC, anzV))      
      
      
    })
    
  ##Beziehungen
    bez_X<- reactive({
      bez<-NULL
      varianten<-varianten_sub()
      varAnz<-varianten[,dim(varianten)[2]]
      varAnz<- rep(varAnz,each=dim(varianten)[2]-2)
      varianten<- varianten[,-dim(varianten)[2]]
      
      for(i in 1:dim(varianten)[1]){
        for(j in 1:(dim(varianten)[2]-1)){
          bez_help<-c(varianten[i,j], varianten[i,j+1])
          bez_sub<-bez[which(bez[,1]==bez_help[1]),]
          bez_sub<- bez_sub[which(bez_sub[2]==bez_help[2])]
          bez<-rbind(bez, bez_help)
          
        }
      }
      bez<- as.data.frame(bez,stringsAsFactors=F)
      bez<-cbind(bez,varAnz)
      return(bez)
    })#beziehungen mi A X beziehungen
    
   
    
    startEvents<- reactive({
      varianten<- varianten_sub()
      startEvents<-aggregate(varianten$Anzahl, by=list(varianten[,1]),sum)
      colnames(startEvents)<- c("akt","anz")
      #startEvents<- table(varianten[,1])
      return(startEvents)
    })

    endEvents<-reactive({
      varianten<- varianten_sub()
      #anzVar<-varianten[,dim(varianten)[2]]
      var_anz<-varianten[,dim(varianten)[2]]
      varianten<- varianten[,-dim(varianten)[2]]
      bez<- bez_X()
      end_help_anz<-bez[which(bez[,2]=="X"),3]
      
      bez<-bez[,-dim(bez)[2]]
      end_help<-bez[which(bez[,2]=="X"),1]
      if(dim(varianten)[1]==1 & varianten[1,dim(varianten)[2]]!="X"){
        endEvents<- data.frame(akt=varianten[1,dim(varianten)[2]],anz=var_anz, stringsAsFactors = F)
        print("------END")
        print(varianten[1,dim(varianten)[2]])
        print(var_anz)
        print(endEvents)
        }else{
        anzEnd<-aggregate(end_help_anz, by=list(end_help),sum)
        anzEnd<-anzEnd[-which(anzEnd[,1]=="X"),]
        endEvents<- varianten[,dim(varianten)[2]]
        
        anzEnd_2<-table(endEvents)
        anzEnd_2_akt<-names(anzEnd_2)
        
        for(i in 1:length(anzEnd_2)){
          if(anzEnd_2_akt[i]%in%anzEnd[,1]){
            anzEnd[which(anzEnd[,1]==anzEnd_2_akt[i]),2]<-anzEnd[which(anzEnd[,1]==anzEnd_2_akt[i]),2]+anzEnd_2[i]
          }
        }
        colnames(anzEnd)<-c("akt","anz")
        endEvents<-anzEnd
      }
      
      
      #endEvents<-unique(c(end_help,endEvents))
      #endEvents<- endEvents[which(endEvents!="X")]
      return(endEvents)
    })
    #Beziehungen X entfernen
    beziehungen<- reactive({
      bez<-bez_X()
      bez<- bez[which(bez[,1]!="X"),]
      bez<- bez[which(bez[,2]!="X"),]
      bez<-aggregate(bez$varAnz,by=list(bez$V1,bez$V2),sum)
      #doppelte Beziehungen entfernen
      # alsWort<-unique(paste0(bez[,1],"-",bez[,2]))
      # wieOftBez1<-table(paste0(bez[,1],"-",bez[,2]))
      # namesBez<-  unlist(strsplit(names(wieOftBez1), split="-"))
      # wieOftBez<- data.frame(wieOftBez1, matrix(namesBez, ncol = 2, byrow = T),stringsAsFactors = F)
      # wieOftBez<- wieOftBez[-1]
      # 
      # bez<-wieOftBez
      colnames(bez)<-c("X1","X2","anz")
      return(bez)
    })
    
  
    output$aktivitaeten<- renderPlot({
      bez<- beziehungen()
      start<- startEvents()
      anzAkt<-aggregate(bez$anz, by=list(bez$X2), sum)
      colnames(anzAkt)<-c("akt", "anz")
      anzAkt<- data.frame(anzAkt)
      anzAkt<-rbind(anzAkt,start)

      g <- ggplot(anzAkt, aes(akt,weight= anz))
      g + geom_bar()+
      theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5))
    })

    #####Graph erstellen
    output$networkVis<- renderVisNetwork({
    bez<-beziehungen()
    
    start<-startEvents()
    startEvents<-start$akt
    anzStart<-start$anz
    
    end<- endEvents()
    anzEnd<- end$anz
    endEvents<-end$akt
    print(endEvents)
    #Startpunkt erstellen
    startNodes<-data.frame(Startknoten="Start", start=startEvents, stringsAsFactors = F)
    
    #Endknoten erstellen
    endNodes<-data.frame(end=endEvents, Endknoten="End", stringsAsFactors = F)
    
    
    nodes1<-unique(c("End", "Start",startEvents, endEvents,unique(bez$X1, bez$X2)))
    nodes<- data.frame(id=nodes1, label= nodes1,color=c("red","green",rep("#CECEF6",length(nodes1)-2)), x=c(1,rep(NULL,length(nodes1)-1)))
    from<- c(startNodes$Startknoten, endNodes$end,bez$X1)
    to<- c(startNodes$start, endNodes$Endknoten, bez$X2)
    
    valueE<-c(table(startEvents), anzEnd,bez$anz)
    valueE[which(valueE>30)]<-30
    valueE<- c(100, valueE[-1])
    
    if(input$anzeige =="Dauer"){
      labelE<-NA
    }else{
      labelE<- c(anzStart, anzEnd,bez[,3])
    }
    edges<- data.frame(from=from, to=to,label=labelE)#, value=valueE)
    

    visNetwork(nodes,edges, width="100%")%>%visEdges(arrows = 'to')%>%
      visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}")%>%
      visIgraphLayout(randomSeed= 46)%>%
      visEdges(smooth= list(enabled = TRUE, type = "vertical"))
  })#Network
  
    output$select_matrix<- renderUI({
      bez<- beziehungen()
      
      akt<- unique(c(bez$X1, bez$X2))
      selectInput("selectAkt",label=NULL, choices=akt)
    })
    output$matrix<- renderTable({
      
      varianten<- varianten_sub()
      bez<- beziehungen()
      
      akt<- unique(c(bez$X1, bez$X2))
      
      
      matrixHelp<- NULL
      for(i in 1: dim(varianten)[1]){
        var_einzel<- varianten[i,]
        
        codierung01<-as.numeric(akt%in%var_einzel)
        matrixHelp<- rbind(matrixHelp, codierung01)
      }
      colnames(matrixHelp)<-akt
      rownames(matrixHelp)<-NULL

      if(dim(matrixHelp)[1]==1){

        zh_matrix<-data.frame(Aktivitaet=colnames(matrixHelp),Prozent=paste0(matrixHelp[1,]*100,"%"))
        
      }else{
        zh_matrix<-NULL
        for(j in 1:dim(matrixHelp)[2]){
          var<- matrixHelp[which(matrixHelp[,j]!=0),]
          
          if(is.null(dim(var)[1])){
            zh_matrix<- rbind(zh_matrix, rep(0, dim(matrixHelp)[2]))
            
          }else{
            sums<-colSums(as.matrix(var,nrow=1))
            zh_matrix<- rbind(zh_matrix,round(100*sums/sums[j],2))
          }
      }
        
      
        rownames(zh_matrix)<-akt
        
        zh_matrix<- zh_matrix[input$selectAkt,]
        zh_matrix<- data.frame(Aktivitaet=akt,Prozent=paste(zh_matrix,"%"))
         
      }
      zh_matrix
    })
  
}

runApp(list(ui=ui, server=server_proto))
