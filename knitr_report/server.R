#Data collection
ui = (htmlOutput("page"))
options(shiny.maxRequestSize = 30*1024^4)
function(input, output, session) {
  
  observe({
    if(file.exists("complete_data.RData")){
      #This e variable is declared globally and used to cache the UI being used.
      e <<- 3
      output$page <- renderUI({
       div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),report_ui())))
      })
      print(ui)
    }else{
      e <<- 0
      data_being_collected <<- "attendance"
      if(data_being_collected == "attendance"){
        output$page <- renderUI({
          div(class="outer",do.call(fluidPage,c(inverse=TRUE ,data_collection_ui1())))
        })
        print(ui)
        ####################################################
        observeEvent(c(input$sep,input$file1),{
          if(data_being_collected == "attendance"){
            att_temp <<- filedata()
          }else{
            att_temp <- att_temp
          }
          if(ncol(att_temp)>1){
          #Create a temporary attendance data frame
          att_temp %>% mutate_if(is.factor, as.character) -> att_temp
          #Extract the modules outo this temporary dataframe.
          att_temp = att_temp%>%group_by(Tutor.Type,Acad.Group)%>%distinct(Module.Code)%>%dplyr::arrange(.by_group = T)
          #allocate to these variables, all the modules to be pasted in the main panel of the data collection UI...
          ems_mods = att_temp%>%filter(Acad.Group == "MEMS",Tutor.Type == "NOR")%>%distinct(Module.Code)%>%.$Module.Code%>%as.vector()
          hum_mods = att_temp%>%filter(Acad.Group == "MHUM",Tutor.Type == "NOR")%>%distinct(Module.Code)%>%.$Module.Code
          hsc_mods = att_temp%>%filter(Acad.Group == "MHSC",Tutor.Type == "NOR")%>%distinct(Module.Code)%>%.$Module.Code
          law_mods = att_temp%>%filter(Acad.Group == "MLAW",Tutor.Type == "NOR")%>%distinct(Module.Code)%>%.$Module.Code
          edu_mods = att_temp%>%filter(Acad.Group == "MEDU",Tutor.Type == "NOR")%>%distinct(Module.Code)%>%.$Module.Code
          nas_mods = att_temp%>%filter(Acad.Group == "MNAS",Tutor.Type == "NOR")%>%distinct(Module.Code)%>%.$Module.Code
          #All this text is paste under the Modules view dataframe
          output$mods2 <- renderText({"Copy and send the following modules to DIRAP when requesting for performance data (All campuses): REQUEST THAT THEY SEND YOU PERFORMANCE DATA WITH THE FOLLOWING [precisely NAMED!!] VARIABLES: 'MODULE.CODE', 'STUDENT.NR', 'FACULTY', 'CAMPUS', 'FINAL.MARK', 'AP.SCORE'"})
          output$mods <- renderText({
            paste0("EMS Modules: ", toString(ems_mods),br(),br(),
            "HUM Modules: ", toString(hum_mods),br(),br(),
            "HSC Modules: ", toString(hsc_mods),br(),br(),
            "LAW Modules: ", toString(law_mods),br(),br(),
            "EDU Modules: ", toString(edu_mods),br(),br(),
            "NAS Modules: ", toString(nas_mods))
          })
          }
        })
        ####################################################
        
      }
      else{
        output$page <- renderUI({
          div(class="outer",do.call(fluidPage,c(inverse=TRUE,data_collection_ui2())))
        })
        print(ui)
      }
    }
    
  })
  #This data is used for generating impact reports and is generated during runtime... From the complete_data object
  FreqData <- reactive({
    if(file.exists("complete_data.RData")) load("complete_data.RData")
    campus       <- (input$campus)
    faculty      <- (input$faculty)
    semester     <- (input$semester)
    tutor.type   <- (input$tutor.type)
    .Modules     <- (input$modular)
    #Ensure that AP score is properly labeled
    names(GroupedData)[names(GroupedData)=="GR_12_ADSCORE"]<-"AP"
    Freq3<-GroupedData%>%filter(Campus==campus,FACULTY==faculty,Term==semester, Tutor.Type==tutor.type)%>%group_by(Attendee,Module.Code,FINAL.MARK,AP,freq)%>%dplyr::summarize()%>%as.data.frame() ##,Tutor.Type==tutor.type
    #Freq3
  })   

  #These are font specs for the animated plot in the main panel of the reporting ui.
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 18,
    color = "black"
  )
  x <- list(
    title = "AVG",
    titlefont = f,
    tickfont = f2
  )
  y <- list(
    title = "TUTORIALS ATTENDED",
    titlefont = f,
    tickfont = f2
  )
  
  
observeEvent(input$faculty,{
 

  output$scatter <- renderUI({
    
    output$plot1 <-  renderPlotly({

#normalize the student population to have proportionally sized points in your plot.
normalize <- function(values, actual_bounds, desired_bounds){
  desired_bounds[1] -> min.y
  desired_bounds[2] -> max.y

  actual_bounds[1] -> min.x
  actual_bounds[2] -> max.x
  result <- vector()
  for(i in 1:length(values)){
    result[i] <- min.y + (values[i]-min.x)*(max.y-min.y)/(max.x-min.x)
  }
  return(result)
}
        #Pull the Data generating function and aggregate your data for plotting...
        df <- FreqData()%>%group_by(Module.Code, gr=cut(freq, breaks= c(0,1,5,20), right=F))%>%summarise(n = n(),mean=mean(FINAL.MARK))
        p <- df %>%
        plot_ly(
          x = ~mean,
          y = ~gr,
          size = ~n,
          sizes = c(100,500),
          color = ~gr,
          frame = ~Module.Code,
          text = ~Module.Code,
          hovertemplate = paste(
            "<b>Performance per Module</b><br><br>",
            "Tutorial Attendance: %{y}<br>",
            "Mark Average: %{x:,.02f}%<br>",
            #"Scaled student population: %{marker.size:,.0f}",
            "<extra></extra>"
          ),
          # hoverinfo = "text",
          type = 'scatter',
          mode = 'markers', alpha = 0.5)%>% layout(xaxis = x, yaxis = y)%>% 
          animation_opts(
            1000, easing = "elastic", redraw = FALSE
          )
     # )
      ggplotly(p)
      
    })
    

  plotlyOutput("plot1")
  })
  x
  #Plot the geom trend lines 
  output$plt2 <- renderPlot({
    req(input$modular)

    if(input$modular == "Select All"){
      df = FreqData()%>%group_by(Module.Code,freq, FINAL.MARK,AP)
      df$AP = as.numeric(as.character(df$AP))/100
    }else{
      df = FreqData()%>%filter(Module.Code%in%input$modular)%>%group_by(Module.Code,freq, FINAL.MARK,AP)
    }
    p2 <- df%>%
    ggplot(.,aes(freq,FINAL.MARK))+stat_summary(fun.data=mean_cl_normal) + 
      stat_smooth()+xlab("Tutorial Attendance")  + ylab("Average Final Marks")  + ylim(0, 100)
    p2 #) %>% layout(xaxis = y, yaxis = x)
  })
  
  output$plt3 <- renderPlot({
    req(input$modular)
    if(input$modular == "Select All"){
      df = FreqData()%>%group_by(Module.Code,freq, FINAL.MARK,AP)
      print(class(df))
    }else{
      df = FreqData()%>%filter(Module.Code%in%input$modular)%>%group_by(Module.Code,freq, FINAL.MARK,AP)
    }
    df$AP = as.numeric(as.character(df$AP))
    df2 <<- df
   p3 <- df%>%
      ggplot(.,aes(AP,FINAL.MARK))+stat_summary(fun.data=mean_cl_normal) + 
      stat_smooth()+xlab("AP Score")+ylab("Average Final Marks")+ ylim(0, 100)
    p3#) %>% layout(xaxis = y, yaxis = x)
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })

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
  
  #Download DATA ##########################################
  
  output$contents <- DT::renderDataTable(
    filedata(),
    options = list(scrollX = TRUE))
  
  observe(e <<- 0)
  observeEvent(input$next_button, {
    showModal(modalDialog("Please be patient, ASIS is processing your data ...", footer=NULL))
    dat_temp <- filedata()
    data_being_collected <<- "performance";
    output$page <- renderUI({
      div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),data_collection_ui2())))
    })
    print(ui)
    e <<- e + 1
    
    if(e ==  1){
      print(ncol(dat_temp))
      if(ncol(dat_temp)<2){
        shinyalert("Please follow instructions!", "Either you chose the wrong delimiter or you're trying to upload incorrect data", type = "error")
      }else{
        write.csv(dat_temp, file = "~/attendance.csv")
      shinyalert("Awesome!", "Attendance data loaded succesfully, Now please load the performance data containing student marks", type = "success")
      }
      }
    if(e == 2){
      write.csv(dat_temp, file = "~/performance.csv")
      source("data_compile.R")
      shinyalert("Done !", "Done merging and aggregating your data, now you can generate your report", type = "success")
    }
    
    if(e >= 2){
      output$page <- renderUI({
        div(class="outer",do.call(fluidPage,c(inverse=TRUE,title = paste("You're logged in as", isolate(input$userName)),report_ui())))
      })
      print(ui)
    }
    removeModal()
  })
  
  
  outVar <- reactive({#%>%filter(Campus==input$campus,FACULTY==input$faculty,Term==input$semester)
    if(file.exists("complete_data.RData")) load("complete_data.RData")
    req(input$campus,input$faculty,input$semester)
    x <- GroupedData%>%filter(Campus==input$campus,FACULTY==input$faculty,Term==input$semester)%>%distinct(Module.Code)%>%.$Module.Code
    print(x)
    if(length(x) == 0){
      mydata = NULL
    }else{
      mydata = c("Select All",x)
    }

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
  

  observeEvent(input$modular,{
    shinyjs::disable("downloadReport")
  })
  
  
  observeEvent(input$next_button, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Done !!.. Your report is ready for download')
  })
 

  observeEvent(input$generate, {
    req(input$modular)
    showModal(modalDialog("Compiling...", footer=NULL))
    if(file.exists("complete_data.RData")) load("complete_data.RData")
    campus       <- isolate(input$campus)
    faculty      <- isolate(input$faculty)
    semester     <- isolate(input$semester)
    tutor.type   <- isolate(input$tutor.type)
    .Modules     <- isolate(input$modular)
    names(GroupedData)[names(GroupedData)=="GR_12_ADSCORE"]<-"AP"
    Freq3<-GroupedData%>%filter(Campus==campus,FACULTY==faculty,Term==semester, Tutor.Type==tutor.type)%>%group_by(Attendee,Module.Code,FINAL.MARK,AP,freq)%>%dplyr::summarize()%>%as.data.frame() ##,Tutor.Type==tutor.type

    # chk_mod <- Freq3 %>% group_by(Module.Code) %>% add_count(Module.Code)
    # chk_mod <- chk_mod %>% group_by(Module.Code,freq,n) %>%dplyr::summarise(zero_attendance=n())
    # #Of those students extract only the one who attended zero tutorials and 
    # #compare with the number of students who attended more than 0. Add a column 
    # #of percentage number of students who didn't attend tutorials (missing column)
    # chk_mod <- chk_mod %>% filter(freq==0)%>%mutate(missing = 100*zero_attendance/n)
    # #Remove from Freq3, modules  with %missing > 90% [more than 10% of students Modules in good_mods attendad tutorials]
    # good_mods <- chk_mod %>% filter(missing < 90) %>% dplyr::select(Module.Code)%>%distinct(Module.Code) %>% .$Module.Code 
    # Freq3 <- Freq3%>%filter(Module.Code%in%good_mods)
    
    NOR_check <- GroupedData%>%filter(Campus==campus,FACULTY==faculty,Term==semester)%>%group_by(Module.Code,Tutor.Type)%>%dplyr::summarize()%>%as.data.frame() ##,Tutor.Type==tutor.type
    # 
    
    source("Decision_Functions.R")
    source("post_hoc.R")
    
    #from the t-test I need 4 results, 4 from anova and 4 from corr for every module
    if(.Modules=="Select All"){Modules<<-NOR_check%>%filter(Tutor.Type=="NOR")%>%distinct(Module.Code)%>%.$Module.Code%>%as.vector()
    }else{Modules<<-unlist(strsplit(.Modules,","))}
    # AnMods<-vector()
    # summary_table <- array(NA,dim=c(length(Modules),4))
    
    #Gather all the information needed to build the report
    our_info <- data.frame(matrix(ncol=18,nrow=(1+length(Modules))))
    names(our_info) <- c("module","evid","table1","table2","table3","table4","decision21",
                         "decision","decision3","decision4", "decision5","decision7","decision8",
                         "summary_stat","summary_decision1","summary_decision2","summary_decision3","summary_decision4")
    
    group<-function(x){
      if(x==0){
        res<-"Group1"
      }else if(x>0&x<=4){
        res <-"Group2"
      }else{res<-"Group3"
      }
      return(res)
    }
    
    #The first entry is the Faculty mods
    #####################################################################################################
    Module <<-"Faculty"
    Freq <- Freq3
    Freq$Groupvec <- sapply(Freq$freq,group)
    our_info[1,]$module <- Module
    our_info[1,]$summary_stat <- summary_stat(Freq)
    iii <- corr(Freq)
    our_info[1,]$table3 <- iii$table3
    our_info[1,]$table4 <- iii$table4
    our_info[1,]$summary_decision3 <- iii$summary_decision3
    our_info[1,]$summary_decision4 <- iii$summary_decision4
    our_info[1,]$decision7 <- iii$decision7
    our_info[1,]$decision8 <- iii$decision8
    
    ttt <- ttest(Freq)
    our_info[1,]$table1           <- ttt$table1
    our_info[1,]$decision         <- ttt$decision
    our_info[1,]$summary_decision1<- ttt$summary_decision1
    our_info[1,]$summary_decision2<- ttt$summary_decision2
    
    aaa <- anovatest(Freq)
    our_info[1,]$table2    <- aaa$table2
    our_info[1,]$decision3 <- aaa$decision3
    our_info[1,]$decision4 <- aaa$decision4
    our_info[1,]$decision5 <- aaa$decision5
    
    tryCatch({
      png(file=paste("images/pic",1,".png",sep=""),width = 6, height = 5, units = 'in', res = 200, pointsize = 11)
      print(funny(Freq))
      dev.off()
    }, error = function(e) return(NULL), warning = function(w) return(NULL))
    #####################################################################################################
    
    
 
    for(i in 2:length(Modules)){
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
          our_info[i,]$summary_decision3 <- iii$summary_decision3
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
        our_info[i,]$summary_decision3 <- iii$summary_decision3
        our_info[i,]$summary_decision4 <- iii$summary_decision4
        our_info[i,]$decision7 <- iii$decision7
        our_info[i,]$decision8 <- iii$decision8
        our_info[i,]$evid   <- paste("Less than 5 students attended more than one tutorial session and thus the t-test and the ANOVA were not conducted")
        our_info[i,]$table1 <- our_info[i,]$table2 <- "Not enough data to make inference**"
      }else if((length(Freq$Groupvec[Freq$Groupvec=="Group2"]))<5|(length(Freq$Groupvec[Freq$Groupvec=="Group3"]))<5) {
        our_info[i,]$table1 <- our_info[i,]$table2 <- "Not enough data to make inference**"
        our_info[i,]$summary_stat <- summary_stat(Freq)
        iii <- corr(Freq)
        our_info[i,]$table3 <- iii$table3
        our_info[i,]$table4 <- iii$table4
        our_info[i,]$summary_decision3 <- iii$summary_decision3
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
        our_info[i,]$summary_decision3 <- iii$summary_decision3
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
        png(file=paste("images/pic",i,".png",sep=""),width = 6, height = 5, units = 'in', res = 200, pointsize = 11)
        print(funny(Freq))
        dev.off()
      }, error = function(e) return(NULL), warning = function(w) return(NULL))
      print(length(Modules)-i+1)
    }
    
    #Remove all the NA's and replace them with an open string
    for(i in 1:nrow(our_info)){
      for(j in 1:ncol(our_info)){
        if(is.na(our_info[i,j])){
          our_info[i,j]  = ""
        }
      }
    }
    
    save(our_info, file = "write_info.RData")
    removeModal()
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Done !!.. Your report is ready for download')
   #S if(length(input$modular) != 0 ) shinyjs::enable("downloadReport")
  })   
  

  output$downloadReport <- renderUI({
    #print(as.numeric(input$generate))
    req(input$generate)
    if(length(input$modular)!=0 & input$generate>0){
    downloadButton("downloadData01","Download Report")
}
        })
  
  
  output$downloadData01 <- downloadHandler(
    #############
    #
    filename = function() {
      paste('report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      #Copy the report file to a temporary directory before processing it, in
      #case we don't have write permissions to the current working dir (which
      #can happen when deployed).
      showModal(modalDialog("Please be patient, ASIS is putting together your document...", footer=NULL))
      tempfolder <- file.path(tempdir())
      pictures   <- list.files("images/",pattern = ".png", recursive = TRUE)
      for(i in 1:length(pictures)) file.copy(paste("images/",pictures[i],sep=""),tempfolder, overwrite = TRUE)
      #remove them from your image folder
      file.remove(paste("images/", list.files("images/"),sep=""))
      dir.create("images")
      #move all the pictures recursively
      file.copy("write_info.RData",tempfolder, overwrite = TRUE)
      file.copy("report.Rmd", tempfolder, overwrite = TRUE)
      src <- normalizePath('report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      #on.exit(unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE))
      
      library(rmarkdown)
      out <- rmarkdown::render(paste(tempfolder,"/report.Rmd",sep=""), switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ) )
      #params = params,
      file.rename(out, file)
  removeModal()
  }
    ##########
  #  removeModal()
    
  )
}

