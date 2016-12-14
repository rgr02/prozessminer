header<- dashboardHeader(title = "Prototyp")

sidebar<- dashboardSidebar()

body<- dashboardBody(
  fluidRow(
    column(width=7,      
           #simpleNetworkOutput("network", width = "80%", height = "250px"),
           visNetworkOutput("networkVis", width="80%", height="600px")
    ),
    column(width=5,
           fluidRow(
             column(width=12,
                   # h5("Wähle Eventlog"),
                    #fileInput("file", label=NULL),                    
                    h5("Pocess Query Language"),
                    textInput("pql", label=NULL,placeholder = "A -> B->?"),
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
  #http://shiny.rstudio.com/articles/plot-interaction.html
  fluidRow(
    column(width=7,
           h5("Abdeckung"),
           plotOutput("abdeckung", height = "250px",
                      hover = "plot_hover",
                      brush = "plot_brush")
    ),
    column(width=5,
           h5("Aktivitaeten"),
           plotOutput("aktivitaeten", height="250px")
    )
  )
)

ui<- dashboardPage(header, sidebar, body)