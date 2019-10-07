#Decision Functions
library(ppcor)
Gformat<-function(x){#This Function formats point values to scientific or double
  if(x<0.001){
    res<-format(x, digits = 3,scientific=TRUE)
  }else{
    res<-format(x, digits = 3,scientific=FALSE)
  }
  return(res)
}
cohen<-function(d){#explanation of the practical significance value 
  if(d<0){
    res<-"very large"
  }else if(d>=0&d<=0.1){
    res<-"zero"
  }else if(d>0.1&d<=0.4){
    res<-"small"
  }else if(d>0.4&d<=0.7){
    res<-"medium"
  }else{
    res<-"large"
  }
  return(res)
}

cohen2<-function(x){#explanation of the practical significance value (second version of cohen)
  if(x>=0&x<=0.003){
    res<-"no"
  }else if(x>0.003&x<=0.039){
    res<-"a small"
  }else if(x>0.039&x<=0.11){
    res<-"an intermediate"
  }else{
    res<-"a large"
  }
  return(res)
}

#This function runs the t-test on the 2 groups att =0 and att > 0 
ttest<-function(Freq){
  Group1 <- Freq[Freq$freq==0,]$FINAL.MARK #This is the group that didn't attend all tutorials
  Group2 <- Freq[Freq$freq>0,]$FINAL.MARK # The group that attended more than 0
  test   <- t.test(Group2,Group1,"greater",0,FALSE,FALSE,0.95)#The t test function
  d      <- abs(mean(Group2)-mean(Group1))/(sqrt(var(Group2)+var(Group1))/2) #practical significance measure
  if(test$p.value<0.05){#Decision to accept or reject
    table1           <- "Yes*" #summary table
    decision         <-paste("An independent-samples t-test was conducted to compare the marks of students who attended at least one tutorial session, and students who attended none, for the module",paste(Module,".",sep=""),"The test shows that the t-statistic is equal to",round(test$statistic,4),"and the p-value is",paste(Gformat(test$p.value),".",sep=""),"Since p-value < 0.05, we reject the null hypothesis i.e. students who attended at least one tutorial session performed significantly better than students who attended none. The effect size was determined and a",cohen(d),"effect was found",paste("(d = ",round(d,3),").",sep=""))
    summary_decision1<-paste("Students who attended tutorial sessions did perform significantly better in the",Module,"module than students who did not attend tutorial sessions.")
    if(cohen(d)=="a small"){
      summary_decision2 <- paste("Although the difference was significant, the practical significance of the difference in the means was",paste(cohen(d),".",sep=""))  
    }else{
      summary_decision2 <- paste("The practical significance of the difference in the means was",paste(cohen(d),".",sep=""))  
    }
  }else {
    table1 <- "No*"
    decision<- paste("An independent-samples t-test was conducted to compare the marks of students who attended at least one tutorial session, and students who attended none, for the module",paste(Module,".",sep=""),"The test shows that the t-statistic is equal to",round(test$statistic,4),"and the p-value is",paste(Gformat(test$p.value),".",sep=""),"Since p-value > 0.05, we accept the null hypothesis i.e. students who attended at least one tutorial session didn't performed better than students who attended none. The effect size was determined and a",cohen(d),"effect was found",paste("(d = ",round(d,3),").",sep=""))
    summary_decision1<-paste("Students who attended tutorial sessions did not perform significantly better in the",Module,"module than students who did not attend tutorial sessions.")
    summary_decision2<-paste("The practical significance of the difference in the means was",paste(cohen(d),".",sep=""))  
    }
  return(list(table1 = table1, decision = decision, summary_decision1=summary_decision1,
              summary_decision2=summary_decision2))
  }


anovatest<-function(Freq){
  lm.model    <-(lm(FINAL.MARK~Groupvec,data=Freq)) #run a linear model for the anova test
  aov.model   <-anova(aov(FINAL.MARK~Groupvec,data=Freq))
  eta2        <-aov.model$`Sum Sq`[1]/sum(aov.model$`Sum Sq`)
  test        <-summary(lm.model)
  decision3   <-paste("Determination coefficient =",round(abs(test$adj.r.squared),4),"i.e.", paste(round(abs(test$adj.r.squared)*100,2),"%",sep=""),"of the variation in final marks of students can be explained by the variation in tutorial attendance.")
  sametest    <-anova(lm(Freq$FINAL.MARK~factor(Freq$Groupvec)))
  if(sametest[1,5]<0.05){
    table2    <- "Yes*"
    decision4 <-paste("A one-way analysis of variance was conducted to compare the average academic performance of students who attended no tutorial sessions (group 1), students who attended 1 to 4 tutorial sessions (group 2) and students who attended at least 5 tutorial sessions (group 3). The F-statistic was equal to",round(sametest[1,4],4),"and p-value equal to",paste(Gformat(sametest[1,5]),",",sep=""),"using a 5% significance level we reject the null hypothesis of no difference in means of final marks for students in the three groups. The effect size was determined and",cohen2(eta2),"effect was found",paste("(eta2 = ",round(eta2,3),").",sep=""))
  }else{
    table2    <- "No*"
    decision4 <-paste("A one-way analysis of variance was conducted to compare the average academic performance of students who attended no tutorial sessions (group 1), students who attended 1 to 4 tutorial sessions (group 2) and students who attended at least 5 tutorial sessions (group 3). The F-statistic was equal to",round(sametest[1,4],4),"and p-value equal to",paste(Gformat(sametest[1,5]),",",sep=""),"using a 5% significance level we accept the null hypothesis of no difference in means of final marks for students in the three groups. The effect size was determined and",cohen2(eta2),"effect was found",paste("(eta2 = ",round(eta2,3),").",sep=""))
  }
  
  #PostHOC using Games-Howell
  ########################################################
  post_hoc<-as.matrix(print(tryCatch({posthoc.tgh(y=Freq$FINAL.MARK, x=Freq$Groupvec,method="games-howell")},warning=function(w){return(matrix(1,3,3))},error=function(e){return(matrix(1,3,3))})))
  if(any(post_hoc[,3]<0.05)){
    
  G12<-post_hoc[1,3];G13<-post_hoc[2,3];G23<-post_hoc[3,3]

  C<-c("G12","G13","G23")  
  tex<-ress<-vector()
for(s in 1:3){
  if(post_hoc[s,3]<0.05){
    ress[s]<-C[s]
  }else{
    ress[s]<-NA
  }
  
switch(ress[s],#figure out where the differences were using the post.hoc tests
         G12={tex[s]<-paste("students who attended no tutorial sessions (group1) and students who attended 1 to 4 tutorial sessions (group2)")},
         G13={tex[s]<-paste("students who attended no tutorial sessions (group1) and students who attended 5 or more tutorial sessions (group3)")},
         G23={tex[s]<-paste("students who attended 1 to 4 tutorial sessions (group2) and students who attended 5 or more tutorial sessions (group3)")}
         )
}
tex<-na.omit(tex)
l<-length(tex)  
if(l==1){
  res<-paste(tex[1])
}
if(l==2){
  res<-paste(tex[1],"as well as between", tex[2])
}

if(l==3){#text connectors 
  res<-paste(tex[1],"as well as between", tex[2],"also between",tex[3])
}
decision5<-paste("Specifically, Post hoc analysis using Games-Howell test indicates a significant difference between", res)
  
}else{
  decision5 <- ""
  }
  return(list(table2=table2,decision3=decision3,decision4=decision4,decision5=decision5))
}

#explanation of correlation value in adjectives
adj<-function(x){
  if(x>0 & x<=0.3){res<-"weakly"
  }else if(x>0.3 & x<=0.5) {res<-"moderately"
  }else if(x>0.5 & x<1) {res<-"strongly"
  }else if(x<0){
    res<-"non-"
  }else{
    res<-"perfectly"
  }
  return(res) 
}
adj2<-function(x,y){
  if(sign(x)==sign(y)){res<-"remained"
  }else{res<-"became"}
  return(res)
}

#Correlation test
corr<-function(Freq){
  ptest<-cor.test(Freq$FINAL.MARK,Freq$freq)
  #state of correlation after partial correlation
  state<-function(x){
    if(x > ptest$estimate){
      res<-"increased in strength,"
    }else{
      res<-"decreased in strength,"
    }
    return(res)
  }
  if(ptest$p.value<0.05){
    table3            <- "Yes*"
    decision7         <-paste("There was a statistically significant,",adj(ptest$estimate),"positive linear relationship (at a 5% significance level) between attendance of tutorial sessions and the final mark obtained by the students (r =",paste(round(ptest$estimate,4),";",sep=""),"p =",Gformat(ptest$p.value),"<0.05).")
    summary_decision3 <-paste("Increased attendance of tutorial sessions is associated with an increase in final marks,")
  }else {
    table3            <- "No*"
    decision7         <-paste("There was a statistically non-significant,",adj(ptest$estimate),"positive linear relationship (at a 5% significance level) between attendance of tutorial sessions and the final mark obtained by the students (r =",paste(round(ptest$estimate,4),";",sep=""),"p =",Gformat(ptest$p.value),">0.05).")
    summary_decision3 <-paste("Increased attendance of tutorial sessions is not associated with an increase in final marks.")
  }
  Freq <-Freq[Freq$AP!="-",] #Remove entries with no AP
  Freq <-Freq[Freq$AP!="0",] #Remove entries with AP = 0
  cbind(Freq$FINAL.MARK,Freq$freq,as.numeric(Freq$AP)) -> cc #bind your dataframe of final marks, attendance and APs
  cc   <- cc[!is.na(cc[,3]),] #remove missing values 
  #This pcor.test functions comes from the ppcor library
  pptest<-pcor.test(cc[,1],cc[,2],cc[,3]) #controlling for AP score
  if(pptest$p.value<0.05&ptest$p.value>0.05){
    table4            <- "Yes*"
    decision8         <- paste("When controlling for the effect of previous academic performance (AP Score), the correlation",state(pptest$estimate),adj2(ptest$estimate,pptest$estimate),adj(pptest$estimate),"positive and became significant (r =",paste(round(pptest$estimate,4),";",sep=""),"p=",Gformat(pptest$p.value),"<0.05).")
    summary_decision4 <- ""
  }else if(pptest$p.value<0.05&ptest$p.value<0.05){
    table4            <- "Yes*"
    summary_decision4 <- paste("Even when previous academic performance was taken into account (AP Score).")
    decision8         <- paste("When controlling for previous academic performance (AP Score), the correlation",state(pptest$estimate),adj2(ptest$estimate,pptest$estimate),adj(pptest$estimate),"positive and remained significant (r =",paste(round(pptest$estimate,4),";",sep=""),"p =",Gformat(pptest$p.value),">0.05).")
  }else if(pptest$p.value>0.05&ptest$p.value<0.05){
    table4            <- "No*"
    summary_decision4 <- paste("however, tutorial attendence did not influence students' final mark, when controlling for previous academic performance (AP Score)")
    decision8         <- paste(" When controlling for previous academic performance (AP Score), the correlation",state(pptest$estimate),adj2(ptest$estimate,pptest$estimate),adj(pptest$estimate),"positive and became insignificant (r =",paste(round(pptest$estimate,4),";",sep=""),"p =",Gformat(pptest$p.value),">0.05).")
  }else {
    table4            <- "No*"
    summary_decision4 <- ""
    decision8         <- paste(" When controlling for previous academic performance (AP Score), the correlation",state(pptest$estimate),adj2(ptest$estimate,pptest$estimate),adj(pptest$estimate),"positive and remained insignificant (r =",paste(round(pptest$estimate,4),";",sep=""),"p =",Gformat(pptest$p.value),">0.05).")
  }
  return(list(table3=table3,table4=table4,summary_decision4=summary_decision4,decision7=decision7,decision8=decision8))
}

summary_stat<-function(Freq){
  if(nrow(Freq)>1){
if(nrow(Freq[Freq$freq==0,])!=0){    
  decision9   <- paste(paste("In total,", nrow(Freq),"students were enrolled for", paste(Module,",",sep=""),"of these,",nrow(Freq[Freq$freq==0,]),"students never attended any of the tutorial sessions, obtained an average mark of",paste(round(mean(Freq[Freq$freq==0,]$FINAL.MARK),2),"%.",sep=""),"The remaining",nrow(Freq[Freq$freq>0,]),"students attended at least one tutorial session, and obtained an average mark of", paste(round(mean(Freq[Freq$freq>0,]$FINAL.MARK),2),"%.",sep="")))
}else{ 
  decision9   <- paste(paste("In total,", nrow(Freq),"students were enrolled for", paste(Module,",",sep=""),"all",nrow(Freq[Freq$freq>0,]),"students attended at least one tutorial session, and obtained an average mark of",paste(round(mean(Freq[Freq$freq>0,]$FINAL.MARK),2),"%.",sep="")))
}
  
  }else{
    decision9 <- paste(paste("Only,", nrow(Freq),"student was enrolled for",paste(Module,",",sep=""),"and obtained a final mark of,",paste(round(mean(Freq[Freq$freq==0,]$FINAL.MARK),2),"%.",sep="")))}
  return(decision9)
}

funny<-function(Freq){
  dat<-c(round(mean(Freq[Freq$freq==0,]$FINAL.MARK),2),round(mean(Freq[Freq$freq<=4&Freq$freq>0,]$FINAL.MARK),2),round(mean(Freq[Freq$freq>4,]$FINAL.MARK),2))
  .size <- c(round(length(Freq[Freq$freq==0,]$FINAL.MARK),2),round(length(Freq[Freq$freq<=4&Freq$freq>0,]$FINAL.MARK),2),round(length(Freq[Freq$freq>4,]$FINAL.MARK),2))
  par(mar = c(8,8,5,4))
  par(mgp=c(6, 4, 1))
  if(dat[1]=="NaN"&dat[2]!="NaN"&dat[3]!="NaN"){
    plot(dat[2:3],xlab="Tutorial Attendance",ylab="Final Marks (%)",main=paste("Means of students' final marks against tutorial attendence\nfor the module",Module),cex.lab=1.5,font.lab=2,ylim=c(10,110),axes = FALSE,type="l",lwd=16,col="lightgreen")
    axis(2);axis(side=1,at=c(1,2),labels=c("Students who\nattended 1-4\ntutorials","Students who\nattended 5\n or more tutorials"))
    lines(dat[2:3],lty=1,lwd=6,col="darkred")
    lines(dat[2:3],pch=17,lwd=3,col=terrain.colors(4),cex=c(3+3*.size[2:3]/sum(.size[2:3])),type="p")
    yy =as.vector(t(dat[2:3])); grid()
    text(x=c(1.1,1.9), y=yy+15,labels=paste(as.character(yy),"%",sep=""),pos = 1,lwd=2, cex =1.2, col = "black",font=2)
  }else if(dat[2]=="NaN"&dat[1]!="NaN"&dat[3]!="NaN"){
    plot(c(dat[1],dat[3]),xlab="Tutorial Attendance",ylab="Final Marks (%)",main=paste("Means of students' final marks against tutorial attendence\nfor the module",Module),cex.lab=1.5,font.lab=2,ylim=c(10,110),axes = FALSE,type="l",lwd=16,col="lightgreen")
    axis(2);axis(side=1,at=c(1,2),labels=c("Students who\nnever attended\ntutorials","Students who\nattended 5\n or more tutorials"))
    lines(c(dat[1],dat[3]),lty=1,lwd=6,col="darkred")
    lines(c(dat[1],dat[3]),pch=17,lwd=3,col=terrain.colors(4),cex=c(3+3*.size[c(1,3)]/sum(.size[c(1,3)])),type="p")
    yy =as.vector(t(c(dat[1],dat[3]))); grid()
    text(x=c(1.1,1.9), y=yy+15,labels=paste(as.character(yy),"%",sep=""),pos = 1,lwd=2, cex =1.2, col = "black",font=2)
  }else if(dat[3]=="NaN"&dat[1]!="NaN"&dat[2]!="NaN"){
    plot(dat[1:2],xlab="Tutorial Attendance",ylab="Final Marks (%)",main=paste("Means of students' final marks against tutorial attendence\nfor the module",Module),cex.lab=1.5,font.lab=2,ylim=c(10,110),axes = FALSE,type="l",lwd=16,col="lightgreen")
    axis(2);axis(side=1,at=c(1,2),labels=c("Students who\nnever attended\ntutorials","Students who\nattended 1-4\ntutorials"))
    lines(dat[1:2],lty=1,lwd=6,col="darkred")
    lines(dat[1:2],pch=17,lwd=3,col=terrain.colors(4),cex=c(3+3*.size[c(1,2)]/sum(.size[c(1,2)])),type="p")
    yy =as.vector(t(dat[1:2])); grid()
    text(x=c(1.1,1.9), y=yy+15,labels=paste(as.character(yy),"%",sep=""),pos = 1,lwd=2, cex =1.2, col = "black",font=2)
  }else if(dat[1]!="NaN"&dat[2]!="NaN"&dat[3]!="NaN"){
    plot(dat,xlab="Tutorial Attendance",ylab="Final Marks (%)",main=paste("Means of students' final marks against tutorial attendence\nfor the module",Module),cex.lab=1.5,font.lab=2,ylim=c(10,110),axes = FALSE,type="l",lwd=16,col="lightgreen")
    axis(2);axis(side=1,at=c(1,2,3),labels=c("Students who\nnever attended\ntutorials","Students who\nattended 1-4\ntutorials","Students who\nattended 5\n or more tutorials"))
    lines(dat,lty=1,lwd=6,col="darkred")
    lines(dat,pch=17,lwd=3,col=terrain.colors(4),cex=c(3+3*.size/sum(.size)),type="p")
    yy =as.vector(t(dat)); grid()
    text(x=c(1.1,2,2.9), y=yy+15,labels=paste(as.character(yy),"%",sep=""),pos = 1,lwd=2, cex =1.2, col = "black",font=2)
  }else{
    type<-c("Students who\nnever attended\ntutorials","Students who\nattended 1-4\ntutorials","Students who\nattended 5\n or more tutorials")
    plot(dat[dat!="NaN"],xlab="Tutorial Attendance",ylab="Final Marks (%)",main=paste("Means of students' final marks against tutorial attendence\nfor the module",Module),cex.lab=1.5,font.lab=2,ylim=c(10,110),axes = FALSE,type="l",lwd=16,col="lightgreen")
    axis(2);axis(side=1,at=1,labels=type[which(dat!="NaN")])
    lines(dat[dat!="NaN"],pch=17,lwd=8,col=terrain.colors(4),cex=c(10*.size[.size>0]/sum(.size[.size>0])),type="p")
    yy =as.vector(t(dat[dat!="NaN"])); grid()
    text(x=c(1.1), y=yy+15,labels=paste(as.character(yy),"%",sep=""),pos = 1,lwd=2, cex =1.2, col = "black",font=2)
  }
}