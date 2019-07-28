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
    
    output$page <- renderUI({
      div(class="outer",do.call(bootstrapPage,c("",ui1())))
    })
  }
  if (USER$Logged == TRUE) 
  {
    output$page <- renderUI({
      div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),ui2())))
    })
    print(ui)
  }
})