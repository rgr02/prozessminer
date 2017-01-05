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
             column(width=6,
                   # h5("Wähle Eventlog"),
                    #fileInput("file", label=NULL),                    
                    h4("Pocess Query Language"),
                    textInput("pql", label=NULL,placeholder = "A -> B->*"),
                    actionButton("pqlButton",label="Subset"),
                    h4("Beschriftung Graph"),
                    radioButtons("anzeige", label=NULL, choices = c("Dauer", "Anzahl"),inline = T)),
             column(width=3,
                    h6("A= Bestellmenge geaendert"),
                    h6("B= Bestellposition erstellt"),
                    h6("C=Bestellposition storniert"),
                    h6("D=Bestellung erstellt"),
                    h6("E=Bestellung freigegeben"),
                    h6("F=Kreditor erstellt"),
                    h6("G=Kreditor gesperrt")),
             column(width=3,
                    h6("H=Preis geaendert"),
                    h6("I=Rechnung eingegangen"),
                    h6("J=Rechnung gestellt"),
                    h6("K=Ware eingegangen"),
                    h6("L=Zahlung durchgefuehrt"),
                    h6("M=Kreditor entsperrt"),
                    h6("N=Bestellung storniert"))
           ),#row 
           fluidRow(
             column(width=12,
                    fluidRow(
                      column(width=6,
                             h4("Statistics"),
                             tableOutput("statistics")),
                      column(width=6,
                             h4("Zusammenhangsmatrix"),
                             uiOutput("select_matrix"),
                             tableOutput("matrix")) )
                    )#row
                    
                    
                    #verbatimTextOutput("matrix"))
                    #plotOutput("matrix"))
           )
    )
  ),
  #http://shiny.rstudio.com/articles/plot-interaction.html
  fluidRow(
    column(width=7,
           h4("Abdeckung"),
           plotOutput("abdeckung", height = "250px",
                      hover = "plot_hover",
                      brush = "plot_brush")
    ),
    column(width=5,
           h4("Aktivitaeten"),
           plotOutput("aktivitaeten", height="250px")
    )
  )
)

ui<- dashboardPage(header, sidebar, body)