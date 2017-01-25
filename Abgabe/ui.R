header<- dashboardHeader(title = "Processminer")

sidebar<- dashboardSidebar()

body<- dashboardBody(
  fluidRow(
    column(width=7,
           visNetworkOutput("networkVis", width="80%", height="600px")
    ),
    column(width=5,
           fluidRow(
             column(width=6,
                    h4("Process Query Language"),
                    textInput("pql", label=NULL,placeholder = "Eingabe"),
                    textOutput("pqlWarning"),
                    actionButton("pqlButton",label="Aktualisieren"),
                    h4("Beschriftung Graph"),
                    radioButtons("anzeige", label=NULL, choices = c("Dauer", "Anzahl"),inline = T)),
             column(width=6,
                    h4("Kennzahlen"),
                    tableOutput("statistics"))
           ),#row 
           fluidRow(
             column(width=12,
                    fluidRow(
                      column(width=6,
                             h4("Zusammenhang zwischen Aktivitaeten"),
                             uiOutput("select_matrix"),
                             tableOutput("matrix")) )
             )#row
           )
    )
  ),
  fluidRow(
    column(width=7,
           h4("Abdeckung"),
           plotOutput("abdeckung", height = "250px",
                      hover = "plot_hover",
                      brush = "plot_brush")
    ),
    column(width=5,
           h4("Anzahl der Aktivitaeten"),
           plotOutput("aktivitaeten", height="250px")
    )
  )
)

ui<- dashboardPage(header, sidebar, body)