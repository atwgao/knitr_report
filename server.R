#Data collection
ui = (htmlOutput("page"))
options(shiny.maxRequestSize = 30*1024^4)
function(input, output, session) {
  USER <- reactiveValues(Logged = Logged)
    observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          #Id.username <- which(my_username == Username)
          #Id.password <- which(my_password == Password)
          #if (length(Id.username) > 0 & length(Id.password) > 0) {
          
          if (str_to_lower(Username) %in% names(user_vec)) {
            if (Password == unname(user_vec[str_to_lower(Username)])) {
              #if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        } 
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE) {
      data_being_collected <<- "attendance"
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      if(data_being_collected == "attendance"){
        output$page <- renderUI({
          div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),data_collection_ui1())))
        })
        print(ui)
      }
      else{
        output$page <- renderUI({
          div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),data_collection_ui2())))
        })
        print(ui)
      }
    }
  })
  
 # observeEvent(input$next_button,{
 #   if(data_being_collected == "attendance"){
 #     output$page <- renderUI({
 #       div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),data_collection_ui1())))
 #     })
 #     print(ui)
 #   }
 #   else{
 #     output$page <- renderUI({
 #       div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),data_collection_ui2())))
 #     })
 #     print(ui)
 #   }
 # })
  
  filedata <- reactive({
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
return(df)
  })
  
  #Download DATA ########################################
  
  output$contents <- DT::renderDataTable(
      filedata(),
    options = list(scrollX = TRUE)
  )
  
  observeEvent(input$next_button, {
   session$sendCustomMessage(type = 'testmessage',
                        message = 'You\'re almost there, now upload the student performance data')
    data_being_collected <<- "performance";
    output$page <- renderUI({
      div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),data_collection_ui2())))
    })
    print(ui)
  })
  
 # output$contents <- renderTable({
    # if(input$disp == "head") {
#      return()
    # }
    # else {
    #   return(filedata())
    # }
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
#  })

    regFormula <- reactive({
      as.formula(paste('mpg ~', input$x))
    })
    
    output$regPlot <- renderPlot({
      par(mar = c(4, 4, .1, .1))
      plot(regFormula(), data = filedata(), pch = 19)
    })

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
}