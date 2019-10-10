#***********************************************load Performance Data
mark <- read.csv("~/performance.csv")
#*********************************************load Attendence data
att <-read.csv("~/attendance.csv")

FindMod <- function(x){
  a1<-att$Module.Code[!duplicated(att$Module.Code)][grepl(x,att$Module.Code[!duplicated(att$Module.Code)])]
  a2<-mark$Module.Code[!duplicated(mark$Module.Code)][grepl(x,mark$Module.Code[!duplicated(mark$Module.Code)])]
if(any(a1%in%a2)==T){
    return(paste(a1[(a1%in%a2)]))
  }
}

x<-att$Term
i<-c(1:length(x))
att$YEAR <- sapply(i,function(i) return(paste("201",substr(x[i],3,3),sep="")))

mcode <- agrep("Module.Code", names(mark),value=T,max=3,ignore.case = TRUE)
stdno <- agrep("STUDENT.NR", names(mark),value=T,max=2)
emplid <- agrep("EMPLID", names(mark),value=T,max=2)
stdno <- ifelse(length(emplid)==0,stdno,emplid)
facult <- agrep("FACULTY", names(mark),value=T,max=3,ignore.case = TRUE)
camp <- agrep("Campus", names(mark),value=T,max=3,ignore.case = TRUE)

names(mark)[names(mark)==mcode] <- "Module.Code"
names(mark)[names(mark)==stdno] <- "Attendee"
names(mark)[names(mark)==facult]<- "FACULTY"
names(mark)[names(mark)==camp]  <- "Campus"

mark$UID = paste(mark$YEAR,mark$Attendee,mark$Module.Code,sep="")
att$UID  = paste(att$YEAR,att$Attendee,att$Module.Code,sep="")

mark <- mark %>% mutate_if(is.factor, as.character)
mark.trim <- mark #
att <- att %>% mutate_if(is.factor, as.character)

Modules     <- att %>% distinct(Module.Code) %>% .$Module.Code %>% as.vector()
NOR_Modules <- att %>% filter(Tutor.Type=="NOR")%>%distinct(Module.Code)%>%.$Module.Code%>%as.vector()
consol_data <- dplyr::right_join(mark, att, by = "UID", suffix = c("", "_new"))
#after right joining, some modules will not exist in the marks data, so remove all these non joined values

consol_data <- consol_data[consol_data$Module.Code%in%Modules,]

.Excluded<-(mark.trim[!mark.trim$UID%in%att[att$Tutor.Type=="NOR",]$UID,])

.GroupedData1<-consol_data %>% group_by(Attendee,Module.Code,YEAR,Campus,FACULTY,GR_12_ADSCORE,FINAL.MARK,Tutor.Type)%>%summarise(freq=n())
.GroupedData2<-.Excluded %>% group_by(Attendee,Module.Code,YEAR,Campus,FACULTY,GR_12_ADSCORE,FINAL.MARK)%>%summarise(Tutor.Type="NOR",freq=0)#%>%mutate(Tutor.Type="NOR",freq=0)

colnames(.GroupedData2)<-colnames(.GroupedData1)
GroupedData<-rbind(.GroupedData1,.GroupedData2)

#Take only the NOR Modules

GroupedData <- GroupedData%>%filter(Tutor.Type=="NOR")

#count how many students in each module attended 0,1 or more tutorials
chk_mod <- GroupedData %>% group_by(Module.Code) %>% add_count(Module.Code)

chk_mod <- chk_mod %>% group_by(Module.Code,freq,n) %>%dplyr::summarise(zero_attendance=n())


#Of those students extract only the one who attended zero tutorials and 
#compare with the number of students who attended more than 0. Add a column 
#of percentage number of students who didn't attend tutorials (missing column)

chk_mod <- chk_mod %>% filter(freq==0)%>%mutate(missing = 100*zero_attendance/n)

#Remove from Groupdata, modules  with %missing > 90% [more than 10% of students Modules in good_mods attendad tutorials]
good_mods <- chk_mod %>% filter(missing < 90) %>% dplyr::select(Module.Code)%>%distinct(Module.Code) %>% .$Module.Code 

GroupedData <- GroupedData%>%filter(Module.Code%in%good_mods)

GroupedData%>%group_by(Module.Code,freq) %>% dplyr::summarise()%>% tally()
x<-as.character(GroupedData$Module.Code)
.FM<-as.character(GroupedData$FINAL.MARK)
.i<-c(1:length(x))
#add their term by taking the second last number and checking if it's even or odd
GroupedData$Term<-sapply(.i,function(i) return(ifelse(as.numeric(substr(x[i],(nchar(x[i])-1),(nchar(x[i])-1)))%%2==0,"SEM2","SEM1")))
GroupedData<-GroupedData%>%mutate(success=ifelse(FINAL.MARK>=50,1,0))
GroupedData<-as.data.frame(GroupedData)
save(GroupedData,file="complete_data.RData")