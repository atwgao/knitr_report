#data collection page
library(tidyverse)
data_collection_ui1 <- function(){
  fluidPage(
    #Download Data########################################################
    
    #title = 'Download reports',
    sidebarLayout(
      sidebarPanel(
        # Input: Select a file ----
        fileInput("file1", "Upload tutorial attendance CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        
        # Horizontal line ----
        tags$hr(),
         
        #Input: Select number of rows to display ----
        # radioButtons("disp", "Display",
        #              choices = c(Head = "head",
        #                          All = "all"),
        #              selected = "head"),
        helpText(),
        tags$head(tags$script(src = "message-handler.js")),
        actionButton("next_button",label = HTML("<span class='small'>Next <i class='glyphicon glyphicon-arrow-right'></i></span>"))
        
      ),
      mainPanel(
        DT::dataTableOutput("contents", width = "auto")
        #tableOutput("contents")
        # fluidRow(
        #   
        #   column(8,
        #          tableOutput("contents")       
        #   ),
        #   
        #   column(4,
        #          plotOutput('regPlot')
        #   )
        # )
        
      )
    )
  )
}