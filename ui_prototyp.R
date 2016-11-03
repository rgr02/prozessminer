#install.packages("visNetwork")
library(shiny)
library(shinydashboard)
library(networkD3)
library(DiagrammeR)
library(visNetwork)
header<- dashboardHeader(title = "Prototyp")

sidebar<- dashboardSidebar()

body<- dashboardBody(
  fluidRow(
    column(width=7,      
           #simpleNetworkOutput("network", width = "80%", height = "250px"),
           visNetworkOutput("networkVis", width="80%", height="300px")
    ),
    column(width=5,
           fluidRow(
             column(width=12,
                    h5("Beschriftung Graph"),
                    radioButtons("anzeige", label=NULL, choices = c("Dauer", "Anzahl"),inline = T))
           ),#row 
           fluidRow(
             column(width=12,
                    h5("Statistics"),
                    verbatimTextOutput("statistics"),
                    h5("Matrix"),
                    verbatimTextOutput("matrix"))
           )
    )
  ),
  fluidRow(
    column(width=7,
           h5("Abdeckung"),
           plotOutput("abdeckung", height = "200px")
    ),
    column(width=5,
           h5("Aktivitäten"),
           plotOutput("aktivitaeten", height="200px")
           )
  )
)

ui<- dashboardPage(header, sidebar, body)


