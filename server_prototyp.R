server <- function(input, output) { 
  output$network<- renderSimpleNetwork({
    src <- c("A", "A", "A", "A",
             "B", "B", "C", "C", "D")
    target <- c("B", "C", "D", "J",
                "E", "F", "G", "H", "I")
    networkData <- data.frame(src, target)
    
    # Plot
    simpleNetwork(networkData,)
  })
  
  output$statistics<- renderPrint({
    if(input$anzeige=="Dauer"){
      data.frame(row.names = c("mean", "Dauer"), val= c(5.12,13))      
      
    }
    
  })
  
  output$abdeckung<- renderPlot({
    barplot(sort(c(5,8,7,3,1,4,8,12),decreasing = T),col = "#58ACFA")
  })
  
  output$networkVis<- renderVisNetwork({
    anzahlN<-c(10,10,4,6,6,10)
    anzahlE<-c(4,1,5,2,3,4)
    dauerN<- paste0(c(3,2,4,1,5,3),"s")
    dauerE<- paste0(c(4,4,4,1,2,3),"s")
    
    if(input$anzeige=="Anzahl"){
      labelN<- anzahlN
      labelE<- anzahlE
    }else{
      labelN<-dauerN
      labelE<-dauerE
    }
    nodes <- data.frame(id = 1:6, 
                        label = labelN,       
                        title = paste0("<p><b>", 1:6,"</b><br>Node !</p>")         # tooltip (html or character)
    )           
    
    
    edges<-data.frame(from = c(1,2,2,3,4,5), to = c(2,3,4,6,5,6),
                      label = labelN)
    
    visNetwork(nodes,edges) %>%visEdges(arrows ="to")%>%
      visLayout(improvedLayout = T)
  })
  
  output$matrix<- renderPrint({
    m<-matrix(abs(round(rnorm(25),2)), ncol = 5)
    m1<-as.data.frame(m)
    rownames(m1)<- c("a", "b", "c","d","e")
    colnames(m1)<- c("a", "b", "c","d","e")
    
    print(m1)
  })
}

runApp(list(ui=ui, server=server))

