#Data collection
ui = (htmlOutput("page"))
options(shiny.maxRequestSize = 30*1024^4)
function(input, output, session) {
  observe({
    if(file.exists("complete_data.RData")){
      e <<- 3
      output$page <- renderUI({
        div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),report_ui())))
      })
      print(ui)
    }else{
      data_being_collected <<- "attendance"
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
    options = list(scrollX = TRUE))

observe(e <<- 0)
  observeEvent(input$next_button, {
    dat_temp <- filedata()
    data_being_collected <<- "performance";
    output$page <- renderUI({
    div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),data_collection_ui2())))
    })
    print(ui)
    e <<- e + 1
    
    if(e ==  1){
      write.csv(dat_temp, file = "~/attendance.csv")
    }
    if(e == 2){
      write.csv(dat_temp, file = "~/performance.csv")
      source("data_compile.R")
    }
    
    if(e >= 2){
      output$page <- renderUI({
        div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),report_ui())))
      })
      print(ui)
    }
  })
  
  
outVar <- reactive({#%>%filter(Campus==input$campus,FACULTY==input$faculty,Term==input$semester)
  if(file.exists("complete_data.RData")) load("complete_data.RData")
  req(input$campus,input$faculty,input$semester)
  x <- GroupedData%>%filter(Campus==input$campus,FACULTY==input$faculty,Term==input$semester)%>%distinct(Module.Code)
    mydata = c("Select All",x)
  })

  observe({
    updateSelectInput(session, "modular",
                         choices = outVar()
    )})
  
  observe({
    if ("Select All" %in% input$modular) {
      updateSelectInput(session, "modular",selected="Select All",
                        choices = "Select All"
      )
    }
  })

  observeEvent(input$generate, {
    if(file.exists("complete_data.RData")) load("complete_data.RData")
    campus       <- isolate(input$campus)
    faculty      <- isolate(input$faculty)
    semester     <- isolate(input$semester)
    tutor.type   <- isolate(input$tutor.type)
    Modules      <- isolate(input$modular)
    names(GroupedData)[names(GroupedData)=="GR_12_ADSCORE"]<-"AP"
    
    Freq3<-GroupedData%>%filter(Campus==campus,FACULTY==faculty,Term==semester)%>%group_by(Attendee,Module.Code,FINAL.MARK,AP,freq)%>%dplyr::summarize()%>%as.data.frame() ##,Tutor.Type==tutor.type
    NOR_check <- GroupedData%>%filter(Campus==campus,FACULTY==faculty,Term==semester)%>%group_by(Module.Code,Tutor.Type)%>%dplyr::summarize()%>%as.data.frame() ##,Tutor.Type==tutor.type
    # 
    
    source("Decision_Functions.R")
    source("post_hoc.R")
    
    #Gather all the information needed to build the report
    our_info <- data.frame(matrix(ncol=17,nrow=(1+length(Modules))))
    names(our_info) <- c("module","evid","table1","table2","table3","table4","decision21",
                         "decision","decision3","decision4", "decision5","decision7","decision8",
                         "summary_stat","summary_decision1","summary_decision2","summary_decision4")
    #from the t-test I need 4 results, 4 from anova and 4 from corr for every module
    # if(.Module=="Select All"){Modules<-NOR_check%>%filter(Tutor.Type=="NOR")%>%distinct(Module.Code)%>%.$Module.Code%>%as.vector()
    # }else{Modules<-unlist(strsplit(.Module,","))}
    # AnMods<-vector()
    # summary_table <- array(NA,dim=c(length(Modules),4))
    group<-function(x){
      if(x==0){
        res<-"Group1"
      }else if(x>0&x<=4){
        res <-"Group2"
      }else{res<-"Group3"
      }
      return(res)
    }
    
    i <- 0
    while(i <= length(Modules)){
      if(i==0){
        if(all(Modules=="Select All")){
          Module<<-"Faculty"
          Freq <- Freq3
          Freq$Groupvec <- sapply(Freq$freq,group)
          i <- 1
          our_info[i,]$module <- Module
          our_info[i,]$summary_stat <- summary_stat(Freq)
          iii <- corr(Freq)
          our_info[i,]$table3 <- iii$table3
          our_info[i,]$table4 <- iii$table4
          our_info[i,]$summary_decision4 <- iii$summary_decision4
          our_info[i,]$decision7 <- iii$decision7
          our_info[i,]$decision8 <- iii$decision8
          
          ttt <- ttest(Freq)
          our_info[i,]$table1           <- ttt$table1
          our_info[i,]$decision         <- ttt$decision
          our_info[i,]$summary_decision1<- ttt$summary_decision1
          our_info[i,]$summary_decision2<- ttt$summary_decision2
          
          aaa <- anovatest(Freq)
          our_info[i,]$table2    <- aaa$table2
          our_info[i,]$decision3 <- aaa$decision3
          our_info[i,]$decision4 <- aaa$decision4
          our_info[i,]$decision5 <- aaa$decision5
          
          tryCatch({
            pdf(file=paste("images/pic",i,".pdf",sep=""),width = 6,height=5,pointsize = 20)
            print(funny(Freq))
          }, error = function(e) return(NULL), warning = function(w) return(NULL))
          dev.off()
          
        }else{
          i <- 1
        }
      }else{
        i <- i+1
      }
      
      our_info[i,]$module <- Module <<- Modules[i]
      Freq<-na.omit(Freq3[Freq3$Module.Code==Modules[i],])
      Freq$Groupvec<-sapply(Freq$freq,group)
      
      Discon<-Freq[Freq$FINAL.MARK==0,]
      #if(all(Freq$FINAL.MARK==0)) #warning(cat("No Marks for",paste(Module)))
      
      Freq$Groupvec<-sapply(Freq$freq,group)
      if(length(Freq$Groupvec[Freq$Groupvec=="Group1"])<5 | all(Freq$Groupvec=="Group1")){
        our_info[i,]$decision21 <- paste("Due to the small sample size there was no evidence to conclude on whether or not the final marks of students who attended at least one tutorial session are greater than the final marks of students who attended no tutorial sessions. However when conducting a correlation test on the small sample, we are able to conclude the following:")
        if(nrow(Freq)>5 & !all(Freq$Groupvec=="Group1")){
          iii <- corr(Freq)
          our_info[i,]$table3 <- iii$table3
          our_info[i,]$table4 <- iii$table4
          our_info[i,]$summary_decision4 <- iii$summary_decision4
          our_info[i,]$decision7 <- iii$decision7
          our_info[i,]$decision8 <- iii$decision8
          our_info[i,]$summary_stat <- summary_stat(Freq)
        }else{
          our_info[i,]$table1 <- our_info[i,]$table2 <- our_info[i,]$table3 <- our_info[i,]$table4 <- "Not enough data to make inference**"
          #wdNormal(paste("No conclusion can be drawn"))
        }
        our_info[i,]$evid <- paste("Less than 5 students did not attend tutorial sessions and thus the t-test and the ANOVA were not conducted")
      }else if(((length(Freq$Groupvec[Freq$Groupvec=="Group3"]))+(length(Freq$Groupvec[Freq$Groupvec=="Group2"])))<5) {
        our_info[i,]$decision21 <- paste("Due to the small sample size there was no evidence to conclude on whether or not the final marks of students who attended at least one tutorial session are greater than the final marks of students who attended no tutorial sessions. However when conducting a correlation test on the small sample, we are able to conclude the following:")
        our_info[i,]$summary_stat <- summary_stat(Freq)
        iii <- corr(Freq)
        our_info[i,]$table3 <- iii$table3
        our_info[i,]$table4 <- iii$table4
        our_info[i,]$summary_decision4 <- iii$summary_decision4
        our_info[i,]$decision7 <- iii$decision7
        our_info[i,]$decision8 <- iii$decision8
        our_info[i,]$evid <- paste("Less than 5 students attended more than one tutorial session and thus the t-test and the ANOVA were not conducted")
        our_info[i,]$table1 <- our_info[i,]$table2 <- "Not enough data to make inference**"
      }else if((length(Freq$Groupvec[Freq$Groupvec=="Group2"]))<5|(length(Freq$Groupvec[Freq$Groupvec=="Group3"]))<5) {
        our_info[i,]$table1 <- our_info[i,]$table2 <- "Not enough data to make inference**"
        our_info[i,]$summary_stat <- summary_stat(Freq)
        iii <- corr(Freq)
        our_info[i,]$table3 <- iii$table3
        our_info[i,]$table4 <- iii$table4
        our_info[i,]$summary_decision4 <- iii$summary_decision4
        our_info[i,]$decision7 <- iii$decision7
        our_info[i,]$decision8 <- iii$decision8
        
        ttt <- ttest(Freq)
        our_info[i,]$table1           <- ttt$table1
        our_info[i,]$decision         <- ttt$decision
        our_info[i,]$summary_decision1<- ttt$summary_decision1
        our_info[i,]$summary_decision2<- ttt$summary_decision2
        
        if((length(Freq$Groupvec[Freq$Groupvec=="Group3"]))<5){
          our_info[i,]$evid <- paste("Less than 5 students attended 5 or more tutorial sessions and thus an ANOVA was not conducted")
        }else{
          our_info[i,]$evid <-paste("Less than 5 students attended 1 to 4 tutorial sessions and thus an ANOVA was not conducted")
        }
      }else{
        our_info[i,]$summary_stat <- summary_stat(Freq)
        iii <- corr(Freq)
        our_info[i,]$table3 <- iii$table3
        our_info[i,]$table4 <- iii$table4
        our_info[i,]$summary_decision4 <- iii$summary_decision4
        our_info[i,]$decision7 <- iii$decision7
        our_info[i,]$decision8 <- iii$decision8
        
        ttt <- ttest(Freq)
        our_info[i,]$table1           <- ttt$table1
        our_info[i,]$decision         <- ttt$decision
        our_info[i,]$summary_decision1<- ttt$summary_decision1
        our_info[i,]$summary_decision2<- ttt$summary_decision2
        
        aaa <- anovatest(Freq)
        our_info[i,]$table2    <- aaa$table2
        our_info[i,]$decision3 <- aaa$decision3
        our_info[i,]$decision4 <- aaa$decision4
        our_info[i,]$decision5 <- aaa$decision5
      }
      tryCatch({
        pdf(file=paste("images/pic",i,".pdf",sep=""),width = 8,height=8,pointsize = 18)
        print(funny(Freq))
      }, error = function(e) return(NULL), warning = function(w) return(NULL))
      dev.off()
      print(length(Modules)-i+1)
    }
save(our_info, file = "write_info.RData")
    })   
  
output$downloadReport <- downloadHandler(
  
#############
filename = function() {
  paste('report', sep = '.', switch(
    input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
  ))
},
content = function(file) {
  #Copy the report file to a temporary directory before processing it, in
  #case we don't have write permissions to the current working dir (which
  #can happen when deployed).
  tempfolder <- file.path(tempdir())
  pictures   <- list.files("images/",pattern = ".pdf", recursive = TRUE)
  for(i in 1:length(pictures)) file.copy(paste("images/",pictures[i],sep=""),tempfolder, overwrite = TRUE)
  #move all the pictures recursively
  file.copy("write_info.RData",tempfolder, overwrite = TRUE)
  file.copy("report.Rmd", tempfolder, overwrite = TRUE)
  src <- normalizePath('report.Rmd')
  # temporarily switch to the temp dir, in case you do not have write
  # permission to the current working directory
  owd <- setwd(tempdir())
  on.exit(setwd(owd))

 
  library(rmarkdown)
  out <- rmarkdown::render(paste(tempfolder,"\\report.Rmd",sep=""), switch(
    input$format,
    PDF = pdf_document(), HTML = html_document(), Word = word_document()
  ) )
  #params = params,
  file.rename(out, file)
}
##########

  )
}