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
    data.frame(row.names = c("mean", "Dauer"), val= c(5.12,13))
  })
  
  output$abdeckung<- renderPlot({
    barplot(sort(c(5,8,7,3,1,4,8,12),decreasing = T),col = "#58ACFA")
  })
  
  output$network1<- renderGrViz({
    grViz("
          digraph {
          layout = twopi
          node [shape = circle]
          A -> {B C D} 
          }")
  })
  
  output$matrix<- renderPrint({
    m<-matrix(abs(round(rnorm(25),2)), ncol = 5)
    m1<-as.data.frame(m)
    rownames(m1)<- c("a", "b", "c","d","e")
    colnames(m1)<- c("a", "b", "c","d","e")
    
    print(m1)
  })
}
