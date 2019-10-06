library(shiny)
aggregate_modules <- data.frame()
report_ui <- function(){
  fluidPage(
    title = 'Selectize examples',
    sidebarLayout(
      sidebarPanel(
        selectInput("campus", label = h4("CAMPUS"), 
                    choices = list("Main Campus" = "MAIN", "Qwaqwa Campus" = "QWA","South Campus" = "SOUTH"), 
                    selected = "MAIN"),
        selectInput("faculty", label = h4("Faculty"), 
                    choices = list("HUMANITIES" = "HUMANITIES","LAW" = "LAW", "NAT.AGRIC SCIENCE" = "NATURAL AND AGRICULTURAL SCIENCES","ECONOMIC AND MANG. SCIENCE"="ECONOMIC AND MANAGEMENT SCIENCES","HEALTH"="HEALTH SCIENCE","EDUCATION"="EDUCATION"), 
                    selected = "ECONOMIC AND MANAGEMENT SCIENCES"),
        selectInput("semester", label = h4("SEMESTER"), 
                    choices = list("SEMESTER 1"="SEM1","SEMESTER 2"="SEM2"), 
                    selected = "SEM1"),
        selectInput("tutor.type", label = h4("Tutorial program"), 
                    choices = c("USD" = "USD",
                                "NOR" = "NOR",
                                "AFS"="AFS",
                                "HMP"="HMP",
                                "RTP"="RTP",
                                "PAS"="PAS"), 
                    selected = "NOR"),
         selectizeInput("modular", "Modules",
                        aggregate_modules,multiple = T)
      ),
      mainPanel(
      helpText('If the above searching fails, it is probably the Github API limit
               has been reached (5 per minute). You can try later.'),
      verbatimTextOutput('github'),
      actionButton("generate","Generate report",icon= icon("running")),
      downloadButton("downloadReport", "Generate report"),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE)
      )
        )
        )
  }
