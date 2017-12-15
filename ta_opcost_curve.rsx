##[TA]=group
##NPV=file
##cost_threshold=number 2
##TA_opcost_database=output table
##report

library(pander)
library(knitr)
library(markdown)
library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(foreign)
library(scales)
library(tcltk)

user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)

#====READ QUES-C DB FROM LUMENS DB====
quesc_list<-as.data.frame(ls(pattern="QUESC_database_"))
n<-nrow(quesc_list)
if(n==0){
  msgBox <- tkmessageBox(title = "SCIENDO",
                         message = "No QUES-C database found",
                         icon = "info",
                         type = "ok")
  quit()
}
data.y<-NULL
data.w<-NULL
for (q in 1:n) {
  n_dta<-nchar(as.character(factor(quesc_list[q,1])))
  data.x<-substr(as.character(factor(quesc_list[q,1])), (n_dta-8), (n_dta-5))
  data.z<-substr(as.character(factor(quesc_list[q,1])), (n_dta-3), n_dta)
  if(data.z > data.x){
    data.y<-c(data.y,data.x)
    data.w<-c(data.w,data.z)
  } else {
    data.y<-c(data.y,data.z)
    data.w<-c(data.w,data.x)
  }
}
qdata<-as.data.frame(cbind(data.y,data.w))

#====SELECT QUES-C DATABASE TO BE ANALYZED====
quesc_list$usage<-0
colnames(quesc_list)[1]="database"
repeat{
  quesc_list<-edit(quesc_list)
  if(sum(quesc_list$usage)==1){
    break
  } else {
    msgBox <- tkmessageBox(title = "Based on period",
                           message = "Choose one QUES-C database. Retry?",
                           icon = "question",
                           type = "retrycancel", default="retry")
    if(as.character(msgBox)=="cancel"){
      quit()
    }
  }
}
qdata<-cbind(quesc_list,qdata)
qdata2<-qdata[which(qdata$usage==1),]
qdata2$usage<-NULL
quesc_db<-as.character(qdata2[1,1])
T1<-as.numeric(as.character(qdata2[1,2]))
T2<-as.numeric(as.character(qdata2[1,3]))
n_dta<-nchar(as.character(factor(qdata2[1,1])))
pu_name<-substr(as.character(factor(qdata2[1,1])), 16:(n_dta-10), (n_dta-10))

#====CREATE FOLDER AND WORKING DIRECTORY====
TA1.index=TA1.index+1
hist_folder<-paste("OpCost_", pu_name,"_", T1,"_",T2,"_",TA1.index,sep="")
wd<-paste(dirname(proj.file),"/TA/", sep="")
setwd(wd)
dir.create(hist_folder)

wd<-paste(wd, hist_folder, sep='')
setwd(wd)

# load datasets
data<-eval(parse(text=(paste(quesc_db))))
lookup_npv <- read.table(NPV, header=TRUE, sep=",")
t1=T1
t2=T2
period<-t2-t1
#iteration=5

#prepare NPV look up table
lookup_n<-lookup_npv
lookup_n$CLASS<-NULL
colnames(lookup_n)[1] ="ID_LC1"
colnames(lookup_n)[2] ="NPV1"
data<-merge(data,lookup_n,by="ID_LC1")
colnames(lookup_n)[1] ="ID_LC2"
colnames(lookup_n)[2] ="NPV2"
data<-merge(data,lookup_n,by="ID_LC2")
tot_area<-sum(data$COUNT)

#Select data where emission happened and count>0
data_em_sel <- data[ which(data$ck_em == "TRUE"),]
data_em_sel <- data_em_sel[ which(data_em_sel$em > 0),]
data_em_sel<-within(data_em_sel, {
  em_rate<-((CARBON_t1-CARBON_t2)*(COUNT*3.67))/(tot_area*period)
  em_tot<- (CARBON_t1-CARBON_t2)*3.67
  sq_rate<-((CARBON_t2-CARBON_t1)*(COUNT*3.67))/(tot_area*period)
  sq_tot<- (CARBON_t2-CARBON_t1)*3.67
  opcost<-(NPV1-NPV2)/em_tot
  opcost_sq<-(NPV1-NPV2)/sq_tot
  cumsum_em<-cumsum(em_rate)
  cumsum_sq<-cumsum(sq_rate)
})

#Build opcost table
lcc_col<-as.data.frame(data_em_sel$LU_CHG)
zone_col<-as.data.frame(data_em_sel$Z_NAME)
opcost_col<-as.data.frame(data_em_sel$opcost)
em_col<-as.data.frame(data_em_sel$em_rate)
opcost_tab<-cbind(lcc_col,zone_col)
opcost_tab<-cbind(opcost_tab,opcost_col)
opcost_tab<-cbind(opcost_tab,em_col)
names(opcost_tab)[1] <- "luchg"
names(opcost_tab)[2] <- "zone"
names(opcost_tab)[3] <- "opcost"
names(opcost_tab)[4] <- "emrate"

#BUILD POSITIVE OPCOST TABLE
opcost_tab_p<- opcost_tab[ which(opcost_tab$opcost >= 0),]
opcost_tab_p<- opcost_tab_p[order(opcost_tab_p$opcost),]
opcost_tab_p$cum_emrate<-cumsum(opcost_tab_p$emrate)
TA_opcost_database<-opcost_tab_p
write.dbf(TA_opcost_database,"TA_opcost_database.dbf")
opcost_tab_p$opcost_log<-log10(opcost_tab_p$opcost)
is.na(opcost_tab_p) <- sapply(opcost_tab_p, is.infinite)
opcost_tab_p[is.na(opcost_tab_p)] <- 0

#BUILD NEGATIVE OPCOST TABLE
opcost_tab_n<- opcost_tab[ which(opcost_tab$opcost < 0),]
opcost_tab_n<- opcost_tab_n[order(opcost_tab_n$opcost),]
opcost_tab_n$cum_emrate<-cumsum(opcost_tab_n$emrate)
opcost_tab_n$opcost_log<-opcost_tab_n$opcost*-1
opcost_tab_n$opcost_log<-log10(opcost_tab_n$opcost_log)*-1

#-----MODIFIED UP TO THIS LINE------------------------------------------

#COMBINE POS && NEG OPCOST
opcost_all<-rbind(opcost_tab_n, opcost_tab_p)

#opcost_tab_p$cum_emrate2<-as.factor(opcost_tab_p$cum_emrate)
#opcost_tab_n$cum_emrate2<-as.factor(opcost_tab_n$cum_emrate)
opcost_all$cum_emrate2<-as.factor(opcost_all$cum_emrate)

#find cost threshold
opcost_all2<- opcost_all
opcost_all2$order<-c(1:nrow(opcost_all2))
find_x_val<-subset(opcost_all2, opcost_log>=log10(cost_threshold))
x_val<-find_x_val$order[1]

#opcost_all
#x<-qplot(x=cum_emrate2, y=opcost_log, fill=zone,data=opcost_all, geom="bar", xlab="Emission Per-Ha Area (ton CO2-eq/ha.year)", ylab="Opportunity Cost ($/ton CO2-eq)" )
#x<-x+geom_hline(aes(yintercept=cost_threshold), linetype="dashed")
#x<-x+geom_vline(aes(xintercept=x_val))
#x<-x+theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0))
#x<-x+scale_y_continuous(breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5))

#emission
#x<-qplot(x=cum_emrate2, y=opcost_log, fill=zone, data=opcost_tab_p, geom="bar", xlab="Emission Per-Ha Area (ton CO2-eq/ha.year)", ylab="Opportunity Cost ($/ton CO2-eq)")
#x<-x+theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0))

#sequestration
#y<-qplot(x=cum_emrate2, y=opcost_log, fill=zone, data=opcost_tab_n, geom="bar", xlab="Emission Per-Ha Area (ton CO2-eq/ha.year)", ylab="Opportunity Cost ($/ton CO2-eq)" )
#y<-y+theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0))


#-----------------------------------------------------------------------

reports<-paste("
               Land Use Planning for Multiple Environmental Services
               ========================================================
               ***
               
               # Lembar hasil analisis TA-Opportunity Cost:
               # Perhitungan opportunity cost berdasarkan data profitabilitas
               
               ***
               
               # Opportunity cost oleh masing-masing unit perencanaan
               ```{r fig.width=12, fig.height=10, echo=FALSE}
               barplot(opcost_all$opcost_log, axes=F, xlab='Emission Per-Ha Area (ton CO2-eq/ha.year)', ylab='Opportunity Cost ($/ton CO2-eq)', col=rainbow(20), space=0.01)
               box()
               axis(1)
               axis(2,at=log10(c(-10000, -1000, -100, -10, -1, -0.1, -0.01, -0.001, -0.0001, 0.0001, 0.001, 0.01, 0.1, 1, cost_threshold, 10, 100, 1000, 10000)),
               label=c(-10000, -1000, -100, -10, -1, -0.1, -0.01, -0.001, -0.0001, 0.0001, 0.001, 0.01, 0.1, 1, cost_threshold, 10, 100, 1000, 10000))
               abline(h=log10(1), col='black')
               abline(h=log10(cost_threshold), lty=3)
               abline(v=x_val+4)
               ```
               ***
               # Intisari opportunity cost
               ```{r fig.width=10, fig.height=9, echo=FALSE}
               pandoc.table(TA_opcost_database)
               
               ```
               ***
               ")


#WRITE REPORT
write(reports,file="reporthtml.Rmd")
knit2html("reporthtml.Rmd", options=c("use_xhml"))