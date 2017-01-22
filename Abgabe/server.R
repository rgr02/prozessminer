server<- function(input,output){
  
  #Berechnung aller Varianten und Dauer
  varianten_all<- getVarianten(eventlog)
  varianten_Anz<- varianten_all$var
  varianten_dauer<- varianten_all$dauer
  
  #Abdeckungsplot erstellen  
  output$abdeckung<- renderPlot({
    print("Abdeckung")
    
    anzVar<-sort(varianten_Anz[,"Anzahl"],decreasing = T)
    anzV<-data.frame(Variante=paste0("V",1:length(anzVar)),Anzahl=anzVar, stringsAsFactors = F)
    
    #Auswahl in Abdeckungsgraphen (Farben setzen)
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
      
    }else{
      col_bar<- rep("blue",length(anzVar))
    }
    
    anzV$percent<-anzV$Anzahl/sum(anzV$Anzahl)*100
    
    #Abdeckungsgraph zeichnen
    g <- ggplot(anzV, aes(reorder(Variante, -percent),weight= percent))
    g + geom_bar(fill=col_bar)+xlab(NULL)+ylab("%")
  })
  
  ####Ab hier individuell von Auswahl abhängig 
  #Funktion nach Auswahl Abdeckung Varianten auswaehlen
  varianten_sub_act<- reactive({
    if(!is.null(input$plot_brush)){
      start<- round(input$plot_brush$xmin)
      end<- round(input$plot_brush$xmax)
      
      varianten<- varianten_Anz[order(-varianten_Anz$Anzahl),]
      varianten<- varianten[start:end,]
    }else{
      varianten<- varianten_Anz
    }
  })
  
  #Funktion nach Eingabe PQL Varianten auswaehlen
  varianten_sub_pql<-eventReactive(input$pqlButton,{
    varianten<-varianten_sub_act()
    
    #Erstellung eines regulaeren Ausdrucks
    eingabe<-gsub(" ", "", input$pql, fixed = TRUE)
    eingabe<-unlist(strsplit(eingabe,"->"))
    eingabe[which(eingabe=="*")]<-"[a-z]*"
    eingabe<- paste(eingabe, collapse="")
    eingabe<-tolower(eingabe)
    
    varWort<-NULL
    
    #Varianten Auswaelen, die regulaeren Ausdruck entsprechen
    for(i in 1:dim(varianten)[1]){
      var_ohne_space<-gsub(" ","", varianten[i,], fixed = T)
      wort<-paste(var_ohne_space, collapse = "")
      varWort<-c(varWort,wort)
    }
    varWort<-tolower(varWort)
    rows<-grep(eingabe, varWort, perl=TRUE, value=FALSE)
    
    varianten<- varianten[rows,]
    
  })
  
  #Allgemeine Funktion zum Auswaehlen der Varianten
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
  
  #Erstellung der Kennzahlen
  output$statistics<- renderTable({
    print("Statistics")
    varianten<- varianten_sub()
    dauersub<- varianten_dauer[varianten$ID,]
    
    dauerSum<- sum(colSums(dauersub[,3:dim(dauersub)[2]]))
    
    print(varianten$ID)
    anzC<- sum(varianten$Anzahl)#anzahl Cases
    anzV<- dim(varianten)[1]# Anzahl Varianten
    durchlaufzeit<-round(seconds_to_period(dauerSum/anzC))
    cNames<- c("Anzahl Cases", "Anzahl Varianten", "Durchschnittliche Durchlaufszeit")
    werte<-as.character(c(anzC, anzV, as.character(durchlaufzeit)))
    
    data.frame(Variable = cNames, val= werte)      
    
    
  })
  
  #Erstellung von Beziehungen mit Platzhalter X
  bez_X<- reactive({
    print("bEz_X")
    bez<-NULL
    varianten<-varianten_sub()
    dauer<- varianten_dauer[varianten$ID,-(1:2)]# Spalte ID und Anzahl wird gelöscht
    
    varAnz<-varianten[,"Anzahl"]
    varAnz<- rep(varAnz,each=dim(varianten)[2]-3)#spalte ID,  anzahl zu viel und 1 weniger beziehungen
    varianten<- varianten[,-c(1,2)]#Löschen ID & Anzahl
    
    #Erstellung Beziehungen + Dauer + Anzahl
    bez<- matrix("X", ncol=2,nrow=length(varAnz))
    dur<- vector("numeric",length = length(varAnz))
    posDur<-0
    rowNr<-0
    for(i in 1:dim(varianten)[1]){
      d<-dauer[i,]
      dur[(posDur+1):(posDur+length(d))]<-d
      posDur<- posDur+length(d)
      for(j in 1:(dim(varianten)[2]-1)){
        bez_help<-c(varianten[i,j], varianten[i,j+1])
        rowNr<-rowNr+1
        
        bez[rowNr,]<- bez_help
        
      }
    }
    #Formatieren Dataframe
    bez<- as.data.frame(bez,stringsAsFactors=F)
    bez<-cbind(bez,varAnz)
    bez<-cbind(bez,unlist(dur))
    colnames(bez)<-c("X1","X2","Anzahl","Dauer")
    
    return(bez)
  })
  
  
  #Ermittlung Startevents + Anzahl
  startEvents<- reactive({
    print("Start")
    varianten<- varianten_sub()
    startEvents<-aggregate(varianten$Anzahl, by=list(varianten[,3]),sum)# Spalte 3 ist Startelement
    colnames(startEvents)<- c("akt","anz")
    
    return(startEvents)
  })
  
  #Ermittlung Endevents + Anzahl
  endEvents<-reactive({
    print("END")
    varianten<- varianten_sub()
    var_anz<-varianten[,"Anzahl"]
    #Loeschen Spalte Anzahl
    varianten$Anzahl<-NULL
    
    bez<- bez_X()
    end_help_anz<-bez[which(bez[,2]=="X"),3]
    
    bez<-bez[,-dim(bez)[2]]
    end_help<-bez[which(bez[,2]=="X"),1]
    #Wenn genau eine zeile besteht mit keinem "X" hinten
    if(dim(varianten)[1]==1 & varianten[1,dim(varianten)[2]]!="X"){
      endEvents<- data.frame(akt=varianten[1,dim(varianten)[2]],anz=var_anz, stringsAsFactors = F)
    }else{
      anzEnd<-aggregate(end_help_anz, by=list(end_help),sum)
      anzEnd<-anzEnd[-which(anzEnd[,1]=="X"),]
      endEvents<- varianten[,dim(varianten)[2]]#letztes element varianten
      
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
    
    return(endEvents)
  })
  #Beziehungen X entfernen
  beziehungen<- reactive({
    print("Beziehung")
    bez<-bez_X()
    bez<- bez[which(bez[,1]!="X"),]
    bez<- bez[which(bez[,2]!="X"),]
    bez<-aggregate(as.matrix(bez[,c("Anzahl","Dauer")]),by=list(bez$X1,bez$X2),sum)
    
    colnames(bez)<-c("X1","X2","anz","Dauer")
    
    return(bez)
  })
  
  #Balkendiagramm Aktivitaeten erzeugen
  output$aktivitaeten<- renderPlot({
    print("aktivitaeten")
    bez<- beziehungen()
    start<- startEvents()
    anzAkt<-aggregate(bez$anz, by=list(bez$X2), sum)
    colnames(anzAkt)<-c("akt", "anz")
    anzAkt<- data.frame(anzAkt)
    anzAkt<-data.frame(rbind(anzAkt,start))
    
    #Sortieren Dataframe
    anzAkt<-transform(anzAkt, akt=reorder(akt,-anz))
    
    #Erstellung Graph
    g <- ggplot(anzAkt, aes(akt,weight= anz))
    g + geom_bar()+
      theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5))+
      ylab(NULL)+xlab(NULL)
  })
  
  #Graph erstellen
  output$networkVis<- renderVisNetwork({
    print("Network")
    bez<-beziehungen()
    
    start<-startEvents()
    startEvents<-start$akt
    anzStart<-start$anz
    
    end<- endEvents()
    anzEnd<- end$anz
    endEvents<-end$akt
    
    #Startknoten erstellen
    startNodes<-data.frame(Startknoten="Start", start=startEvents, stringsAsFactors = F)
    
    #Endknoten erstellen
    endNodes<-data.frame(end=endEvents, Endknoten="End", stringsAsFactors = F)
    
    #Knoten und Kanten erstellen
    nodes1<-unique(c("End", "Start",startEvents, endEvents,unique(bez$X1, bez$X2)))#"#CECEF6"
    nodes<- data.frame(id=nodes1, label= nodes1,color=c("red","green",rep("#CECEF6",length(nodes1)-2)), x=c(1,rep(NULL,length(nodes1)-1)))
    from<- c(startNodes$Startknoten, endNodes$end,bez$X1)
    to<- c(startNodes$start, endNodes$Endknoten, bez$X2)
    
    #Kantenbeschriftung definieren
    if(input$anzeige =="Dauer"){
      durchschnitt<- round(seconds_to_period(bez$Dauer/bez$anz))
      labelE<-c(rep(NA,length(anzStart)+length(anzEnd)),as.character(durchschnitt))
    }else{
      labelE<- c(anzStart, anzEnd,bez[,3])
    }
    edges<- data.frame(from=from, to=to,label=labelE)
    
    #Erstellung Graph
    visNetwork(nodes,edges, width="100%")%>%visEdges(arrows = 'to')%>%
      visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}")%>%
      visIgraphLayout(randomSeed= 46)%>%
      visEdges(smooth= list(enabled = TRUE, type = "vertical"))
  })#Network
  
  #Erstellung Dropdownmenu
  output$select_matrix<- renderUI({
    bez<- beziehungen()
    
    akt<- unique(c(bez$X1, bez$X2))
    selectInput("selectAkt",label=NULL, choices=akt)
  })
  
  #Erstellung Zusammenhangsmatrix
  output$matrix<- renderTable({
    print("matrix")
    varianten<- varianten_sub()
    bez<- beziehungen()
    
    akt<- unique(c(bez$X1, bez$X2))
    
    #Erstellung Hilfmatrix ob Akt in Variante vorkommt
    matrixHelp<- data.frame(matrix(0,nrow=dim(varianten)[1], ncol=length(akt)),stringsAsFactors=F)
    for(i in 1: dim(varianten)[1]){
      var_einzel<- varianten[i,]
      
      codierung01<-as.numeric(akt%in%var_einzel)
      matrixHelp[i,]<-codierung01
    }
    colnames(matrixHelp)<-akt
    rownames(matrixHelp)<-NULL
    
    #Wenn nur eine Zeile vorhanden
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

runApp(list(ui=ui, server=server))
