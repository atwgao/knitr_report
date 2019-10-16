Executive<-function(){
wdBody("Executive Summary")
wdType("In the majority of the modules analysed, students who attended tutorials performed significantly better than students who did not.  There is some evidence to suggest that students who attend 5 or more tutorials perform even better than students who attend 1-4 tutorials, however this is not consistent over all modules. In the majority of modules, increased tutorial attendance is associated with improved academic performance even when previous academic performance is taken into consideration.",italic=TRUE,alignment="left")
wdSubsection("Introduction")
wdBody(paste("This is a report on the impact of the Academic student and excellence tutorial program (A_STEP) in the", paste(ifelse(semester=='SEM1',"first","second")),"semester of", paste(year) ,"in the",paste(faculty,".",sep=""),"  This report is based on the quantitative data analysis conducted in the modules which formed part of the A_STEP 
             The A_STEP program is based on the principles of Supplemental Instruction (SI) where high-risk modules are identified (as opposed to high risk students). In essence, when students learn collaboratively in high-quality tutorials which are facilitated by well-trained tutors they are more likely to master course material and be successful. The A_STEP encourages regular attendance of tutorials, and believes that continued participation over time plays an important role for student success. Towards the goal of continued improvement and critical self-reflection the A_STEP examines its impact on academic performance and evaluates tutors on a semester-by-semester basis. 
             We analyzed a total of",length(as.vector(na.omit(AnMods))),"modules in the faculty that had A_STEP tutorial groups. Results of the analysis for all of these are presented in this report.
             "))
# wdItemize()
# wdWrite(paste(na.omit(Others),"\n"))
wdBody("The table below summaries the primary findings from the analysis of tutorial attendance and academic performance. Results are summarized by module.")
no_missing_mods <- summary_table[which(!is.na(summary_table[,1])),]
smoke <-no_missing_mods#matrix("",length(as.vector(na.omit(AnMods))),4)
colnames(smoke) <- c("Do students who attend tutorials perform significantly better than students who do not?",
                     "Do students who attend 5+ tutorials perform better than students who attend 1-4 tutorials AND those who attend no tutorials?",
                     "Is there a significant positive correlation between tutorial attendance and academic performance?",
                     "If previous academic performance is taken into account, is there a significant positive correlation between tutorial attendance and academic performance?"
)
if(!is.null(nrow(smoke))){
rownames(smoke) <-as.vector(na.omit(AnMods))
smoke <- as.table(smoke)
wdTable(format(smoke))#three possibilities to autoformat parameter
tryCatch({wdSetFont(fontsize=9)},error=function(e){return("")},warning=function(w){return("")})
wdWrite("* Indicates a result with moderate to large effect sizes
        ** Not applicable indicates that the statistic was not determined either due to sample size or violation of assumptions. For details refer to the detailed discussion of evidence for the module.")
wdSection("Understanding tutorial attendance in relation to academic performance")
wdBody("The following section presents the findings from the analysis of tutorial attendance in relation to academic performance for each module in the faculty. Data obtained from attendance lists and academic performance is analysed to determine whether students who attend tutorials perform better academically than student who do not attend tutorials, and whether attending more than 5 tutorials makes a difference to academic performance. Furthermore, the relationship between academic performance and tutorial attendance is investigated, and the effect of previous academic performance in high school is taken into account in understanding this relationship. Finally, the relationship between tutorial attendance and academic performance is investigated within the top performing academic group and the lowest performing academic group. 
       For each module a summary of results is presented first to allow for an overall understanding of tutorial attendance in relation to academic performance. Thereafter, a section entitled ``Evidence`` is presented which details all of the results from the statistical analysis. 
       ")

}else{
  wdBody("Table can only be printed for 2 or more modules")
}
}

