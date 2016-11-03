#install.packages("networkD3")
library(shiny)
library(shinydashboard)
library(networkD3)
library(DiagrammeR)
header<- dashboardHeader(title = "Prototyp")

sidebar<- dashboardSidebar()

body<- dashboardBody(
  fluidRow(
    column(width=7,      
           simpleNetworkOutput("network", width = "80%", height = "250px")
    ),
    column(width=5,
           fluidRow(
             column(width=12,
                    h5("Beschriftung Graph"),
                    radioButtons("anzeige", label=NULL, choices = c("Dauer", "Anzahl")))
           ),#row
           fluidRow(
             column(width=12,
                    h5("Statistics"),
                    verbatimTextOutput("statistics"))
           )
    )
  ),
  fluidRow(
    column(width=7,
           
           plotOutput("abdeckung", height = "200px")
    ),
    column(width=5,
           verbatimTextOutput("matrix"))
  )
)

ui<- dashboardPage(header, sidebar, body)


shinyApp(ui, server)
runApp("C:\path-to-download-location\shiny-d3-plot")
