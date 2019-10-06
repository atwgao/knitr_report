#Gather all the information needed to build the report
our_info <- data.frame(matrix(ncol=17,nrow=(1+length(Modules))))
names(our_info) <- c("module","evid","table1","table2","table3","table4","decision21",
         "decision","decision3","decision4", "decision5","decision7","decision8",
         "summary_stat","summary_decision1","summary_decision2","summary_decision4")
#from the t-test I need 4 results, 4 from anova and 4 from corr for every module
# if(.Module=="Select All"){Modules<-NOR_check%>%filter(Tutor.Type=="NOR")%>%distinct(MODULE_CODE)%>%.$MODULE_CODE%>%as.vector()
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
    if(.Module=="Select All"){
      Module<-faculty
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
    }
  }else{
    i <- i+1
  }
  
  our_info[i,]$module <- Module<-Modules[i]
  Freq$Groupvec<-sapply(Freq$freq,group)
  Freq<-na.omit(Freq3[Freq3$MODULE_CODE==Modules[i],])

  Discon<-Freq[Freq$FINAL.MARK==0,]
  if(all(Freq$FINAL.MARK==0)) warning(cat("No Marks for",paste(Module)))
  
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
  print(length(Modules)-i)

}
