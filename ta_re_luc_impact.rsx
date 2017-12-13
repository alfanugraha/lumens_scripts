#Impact of Land Using to Regional Economy Indicator Analysis
##TA-PostgreSQL=group
##int_con_file=string
##add_val_file=string
##fin_dem_file=string
##add_val_struc_file=string
##fin_dem_struc_file=string
##sector_file=string
##land.distribution_file=string
##land.requirement_file=string
##lc.list_file=string
##labour_file=string
##unit=string
##area_name=string
##I_O_period=number 2000
##location=string

library(reshape2)
library(ggplot2)
library(raster)
library(foreign)
library(rtf)
library(magick)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
work_dir<-paste(log.file[1,1], "/", log.file[1,2],"/TA/LandUseChangeImpact", sep="")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)
dir.create(work_dir)

#SET WORKING DIRECTORY
setwd(work_dir)

#READ LANDUSE DATA
per<-as.data.frame(ls(pattern="freq"))
n<-nrow(per)
if(n==0){
  msgBox <- tkmessageBox(title = "Pre-QUES",
                         message = "No Land Use/Cover found",
                         icon = "info",
                         type = "ok")
  quit()
}
data<-per
data.y<-NULL
for (q in 1:n) {
  data.x<-substr(as.character(factor(data[q,1])), 5, 14)
  data.y<-c(data.y,data.x)
  
}
data<-as.data.frame(data.y)

n<-nrow(data)
command1<-NULL
command2<-NULL
for(i in 1:n) {
  if (i!=n){
    command1<-paste(command1,"period", i, ",", sep="")
    command2<-paste(command2,"landuse_t", i, ",", sep="")
  } else {
    command1<-paste(command1,"period", i, sep="")
    command2<-paste(command2,"landuse_t", i, sep="")
  }
}

#SELECT DATA TO BE ANALYZED
eval(parse(text=(paste("year<-c(", command1, ")", sep=""))))
data<-as.data.frame(cbind(data,year))
data$t1<-0
data$t2<-0
colnames(data)[1]<-"data"
data$data<-as.character(data$data)
repeat{
  data_temp<-edit(data)
  if(sum(data_temp$t1)==1 & sum(data_temp$t2)==1){
    data_temp$sum<-data_temp$t1+data_temp$t2
    data_temp <- data_temp[which(data_temp$sum==1),]
    n_temp<-nrow(data_temp)
    if(n_temp!=0) {
      data<-data_temp
      break  
    }
  } else {
    msgBox <- tkmessageBox(title = "TA",
                           message = "Choose data to be analyzed. Retry?",
                           icon = "question", 
                           type = "retrycancel", default="retry")
    if(as.character(msgBox)=="cancel"){
      quit()
    }
  }
}

data$t1<-NULL
data$t2<-NULL
data$sum<-NULL

eval(parse(text=(paste("lu_name1 <- names(", data[1,1], ")", sep=""))))
eval(parse(text=(paste("landuse_t1 <- ", data[1,1], sep=""))))
eval(parse(text=(paste("lu_name2 <- names(", data[2,1], ")", sep=""))))
eval(parse(text=(paste("landuse_t2 <- ", data[2,1], sep=""))))

#WRITING TA PROJECT FILE
filename<-paste("TA_projec_", eval(parse(text=(paste("Sys.Date ()")))), ".lms", sep="")
date<-Sys.Date()
sink(filename)
cat("LUMENS TA Module Project File")
cat("\n")
cat(as.character(date))
cat("\n")
cat("\n")
cat(lu_name1)
cat("\n")
cat(lu_name2)
cat("\n")
cat(int_con_file)
cat("\n")
cat(add_val_file)
cat("\n")
cat(labour_file)
cat("\n")
cat(fin_dem_file)
cat("\n")
cat(add_val_struc_file)
cat("\n")
cat(fin_dem_struc_file)
cat("\n")
cat(sector_file)
cat("\n")
cat(land.distribution_file)
cat("\n")
cat(land.requirement_file)
cat("\n")
cat(lc.list_file)
cat("\n")
cat(unit)
cat("\n")
cat(area_name)
cat("\n")
cat(I_O_period)
sink()

#READ INPUT FILE
nodata_val<-0
int_con<- read.table(int_con_file, header=FALSE, sep=",")
add_val<- read.table(add_val_file, header=FALSE, sep=",")
fin_dem<- read.table(fin_dem_file, header=FALSE, sep=",")
fin_dem_struc<- read.table(fin_dem_struc_file, header=FALSE, sep=",")
add_val_struc<- read.table(add_val_struc_file, header=FALSE, sep=",")
sector<- read.table(sector_file, header=FALSE, sep=",")
labour<- read.table(labour_file, header=FALSE, sep=",")
land.distribution<-read.dbf(land.distribution_file, as.is = FALSE)
land.requirement.db<-read.dbf(land.requirement_file, as.is = FALSE)
lc.list<-read.table(lc.list_file, header=FALSE, sep=",")
int_con.m<-as.matrix(int_con)
add_val.m<-as.matrix(add_val)
dim<-ncol(int_con.m)

#CALCULATE INVERS LEONTIEF
int_con.ctot<-colSums(int_con.m)
add_val.ctot<-colSums(add_val.m)
fin_con<- 1/(int_con.ctot+add_val.ctot)
fin_con[is.infinite(fin_con)]<-0
t.input.invers<-diag(fin_con)
A<-int_con.m %*% t.input.invers
I<-as.matrix(diag(dim))
I_A<-I-A
Leontief<-solve(I_A)

#DIRECT BACKWARD LINKAGES
DBL<-colSums(Leontief)
DBL<-DBL/(mean(DBL))
DBL<-cbind(sector,DBL)
colnames(DBL)[3] <- "DBL"
order_DBL <- as.data.frame(DBL[order(-DBL$DBL),])
order_DBL10<-head(order_DBL,n=20)
colnames(order_DBL10)[1] <- "SECTOR"
colnames(order_DBL10)[2] <- "CATEGORY"
BPD_graph<-ggplot(data=order_DBL10, aes(x=SECTOR, y=DBL, fill=CATEGORY)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Value")

#DIRECT FORWARD LINKAGES
DFL<-rowSums(Leontief)
DFL<-DFL/(mean(DFL))
DFL<-cbind(sector,DFL)
colnames(DFL)[3] <- "DFL"
order_DFL <- as.data.frame(DFL[order(-DFL$DFL),])
order_DFL10<-head(order_DFL,n=20)
colnames(order_DFL10)[1] <- "SECTOR"
colnames(order_DFL10)[2] <- "CATEGORY"
FPD_graph<-ggplot(data=order_DFL10, aes(x=SECTOR, y=DFL, fill=CATEGORY)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Value")


#CREATE LINKAGES TABLE
DBL_temp<-colSums(Leontief)
BPD_temp<-DBL_temp/(mean(as.matrix(DBL_temp)))
DFL_temp<-rowSums(Leontief)
FPD_temp<-DFL_temp/(mean(as.matrix(DFL_temp)))
DBL_temp<-as.data.frame(round(DBL_temp, digits=2))
BPD_temp<-as.data.frame(round(BPD_temp, digits=2))
DFL_temp<-as.data.frame(round(DFL_temp, digits=2))
FPD_temp<-as.data.frame(round(FPD_temp, digits=2))
Linkages_table<-cbind(sector,DBL_temp,DFL_temp,BPD_temp,FPD_temp)
colnames(Linkages_table)[1] <- "SECTOR"
colnames(Linkages_table)[2] <- "CATEGORY"
colnames(Linkages_table)[3] <- "DBL"
colnames(Linkages_table)[4] <- "DFL"
colnames(Linkages_table)[5] <- "BPD"
colnames(Linkages_table)[6] <- "FPD"
PRS_graph<-ggplot(Linkages_table, aes(x=BPD, y=FPD, color=CATEGORY)) + geom_point(shape=19, size=5) + geom_hline(aes(yintercept=1), colour="#BB0000", linetype="dashed") + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")


#SELECTION OF PRIMARY SECTOR
P.sector<-cbind(DBL,DFL)
colnames (P.sector) [1]<-"Sectors"
P.sector[4]<-NULL
P.sector[4]<-NULL
P.sector.selected <- P.sector[ which(P.sector$DBL >= 1),]
P.sector.selected <- P.sector.selected[ which(P.sector.selected$DFL >= 1),]
colnames(P.sector.selected)[1] <- "SECTOR"
colnames(P.sector.selected)[2] <- "CATEGORY"


#GDP
GDP.val<-as.data.frame(add_val.m[2,]+add_val.m[3,])
GDP.val.m<-as.matrix(GDP.val)
GDP.val.m<-as.numeric(GDP.val.m)
OUTPUT.val<-as.data.frame(add_val.m[2,]+add_val.m[3,]+add_val.m[1,]+int_con.ctot)
OUTPUT.val.m<-as.matrix(OUTPUT.val)
OUTPUT.val.m<-as.numeric(OUTPUT.val.m)
GDP<-cbind(sector,GDP.val,OUTPUT.val)
colnames(GDP)[1] <- "SECTOR"
colnames(GDP)[2] <- "CATEGORY"
colnames(GDP)[3] <- "GDP"
colnames(GDP)[4] <- "OUTPUT"
GDP$GDP_PROP<-GDP$GDP/GDP$OUTPUT
GDP[is.na(GDP)]<-0
colnames(GDP)[5] <- "P_OUTPUT"
GDP_tot<-as.matrix(GDP$GDP)
GDP_tot<-colSums(GDP_tot)
GDP$P_GDP<-round((GDP$GDP/GDP_tot), digits=2)
order_GDP <- as.data.frame(GDP[order(-GDP$GDP),])
order_GDP10<-head(order_GDP,n=20)
GDP_graph<-ggplot(data=order_GDP10, aes(x=SECTOR, y=GDP, fill=SECTOR)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("GDP")
GDP$GDP<-round(GDP$GDP, digits=1)
GDP$OUTPUT<-round(GDP$OUTPUT, digits=1)
GDP$P_OUTPUT<-round(GDP$P_OUTPUT, digits=2)
GDP$P_GDP<-round(GDP$P_GDP, digits=2)


#OUTPUT MULTIPLIER
Out.multiplier<-colSums(Leontief)
Out.multiplier<-cbind(sector,Out.multiplier)
order_Out.multiplier <- as.data.frame(Out.multiplier[order(-Out.multiplier$Out.multiplier),])
order_Out.multiplier <-head(order_Out.multiplier,n=20)
OMPL_graph<-ggplot(data=order_Out.multiplier, aes(x=V1, y=Out.multiplier, fill=V2)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Output multiplier")

#INCOME MULTIPLIER
V.income<-as.matrix(GDP.val*fin_con)
Inc.multiplier<-Leontief%*%V.income
multiplier<-cbind(Out.multiplier,Inc.multiplier)
Inc.multiplier<-cbind(sector,Inc.multiplier)
colnames(Inc.multiplier)[3]<-"Inc.multiplier"
order_Inc.multiplier <- as.data.frame(Inc.multiplier[order(-Inc.multiplier$Inc.multiplier),])
order_Inc.multiplier <-head(order_Inc.multiplier,n=20)
IMPL_graph<-ggplot(data=order_Inc.multiplier, aes(x=V1, y=Inc.multiplier, fill=V2)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Income multiplier")

#LABOUR MULTIPLIER
labour.m<-as.matrix(labour*fin_con)
labour.m<-labour.m/1000000
Lab.multiplier<-Leontief%*%labour.m
multiplier<-cbind(multiplier,Lab.multiplier)
colnames(multiplier)[1] <- "SECTOR"
colnames(multiplier)[2] <- "CATEGORY"
colnames(multiplier)[5] <- "Lab.multiplier"
multiplier$Out.multiplier<-round(multiplier$Out.multiplier, digits=3)
Lab.multiplier<-cbind(sector,Lab.multiplier)
colnames(Lab.multiplier)[3]<-"Lab.multiplier"
order_Lab.multiplier <- as.data.frame(Lab.multiplier[order(-Lab.multiplier$Lab.multiplier),])
order_Lab.multiplier <-head(order_Lab.multiplier,n=20)
LMPL_graph<-ggplot(data=order_Lab.multiplier, aes(x=V1, y=Lab.multiplier, fill=V2)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Labour multiplier")
colnames(multiplier)[4]<-"Inc.multiplier"
multiplier$Inc.multiplier<-round(multiplier$Inc.multiplier, digits=3)

#Read land use map and calculate area of land use
landuse<-ratify(landuse_t2, filename='landuse.grd',count=TRUE,overwrite=TRUE)
landuse0<-ratify(landuse_t1, filename='landuse.grd',count=TRUE,overwrite=TRUE)
landuse_area<-as.data.frame(levels(landuse))
landuse_area0<-as.data.frame(levels(landuse0))
landuse_area<-subset(landuse_area,ID !=nodata_val)
landuse_area0<-subset(landuse_area0,ID !=nodata_val)
landuse_area<-subset(landuse_area,ID !=15)
landuse_area<-as.matrix(landuse_area$COUNT)
landuse_area0<-as.matrix(landuse_area0$COUNT)
landuse_table<-cbind(lc.list,landuse_area0, landuse_area)
landuse_area_diag<-diag(as.numeric(as.matrix(landuse_area)))
colnames(landuse_table)[1] <- "LAND_USE"
colnames(landuse_table)[2] <- "T1_HA"
colnames(landuse_table)[3] <- "T2_HA"
landuse_table<-edit(landuse_table)
landuse_table$CHANGE<-landuse_table$T2_HA-landuse_table$T1_HA

#MODEL FINAL DEMAND
land.distribution.prop<-as.matrix(land.distribution)
land.distribution.scen<-land.distribution.prop %*% landuse_area_diag
land.requirement.scen<-rowSums(land.distribution.scen)
fin_dem.rtot<-rowSums(fin_dem)
int_con.rtot<-rowSums(int_con)
demand<-fin_dem.rtot+int_con.rtot
land.requirement.coeff<-land.requirement.db$LRC
land.productivity.coeff<-land.requirement.db$LPC
fin_dem.scen<-land.requirement.scen/land.productivity.coeff
fin_dem.scen[is.infinite(fin_dem.scen)]<-0
fin_dem.scen[is.na(fin_dem.scen)]<-0

#CALCULATE FINAL DEMAND AND GDP FROM SCENARIO OF LAND USE CHANGE
fin.output.scen<-Leontief %*% fin_dem.scen
fin.output.scen<-round(fin.output.scen, digits=1)
colnames(fin.output.scen)[1]<-"OUTPUT_Scen"
GDP.prop.from.output<-GDP.val/demand
GDP.prop.from.output[is.na(GDP.prop.from.output)]<-0
GDP.scen<-GDP.prop.from.output*fin.output.scen
GDP.scen<-round(GDP.scen, digits=1)
GDP.scen[is.na(GDP.scen)]<-0
colnames(GDP.scen)[1] <- "GDP_scen"
GDP.diff<-GDP.scen-GDP$GDP
GDP.diff<-round(GDP.diff, digits=1)
colnames(GDP.diff)[1] <- "GDP_diff"
GDP.rate<-GDP.diff/GDP.val
GDP.rate[is.na(GDP.rate)]<-0
GDP.rate<-round(GDP.rate, digits=2)
colnames(GDP.rate)[1] <- "GDP_rate"
GDP_summary<-cbind(GDP,GDP.scen,fin.output.scen,GDP.diff, GDP.rate)
GDP_summary$P_OUTPUT<-NULL
GDP_summary$P_GDP<-NULL

#calculate total GDP
GDP_tot_scen<-as.matrix(GDP_summary$GDP_scen)
GDP_tot_scen<-colSums(GDP_tot_scen)
GDP_tot_diff<-GDP_tot_scen-GDP_tot
GDP_tot_rate<-GDP_tot_diff/GDP_tot
text1<-"Total GDP"
text2<-"Scenario GDP"
text3<-"GDP difference"
text4<-"Rate of difference"
GDP_overall1<-rbind(text1,text2,text3,text4)
GDP_overall2<-rbind(GDP_tot, GDP_tot_scen,GDP_tot_diff,GDP_tot_rate)
GDP_overall<-cbind(GDP_overall1,GDP_overall2)

order_GDP_scen <- as.data.frame(GDP_summary[order(-GDP_summary$GDP_scen),])
order_GDP_scen10<-head(order_GDP_scen,n=20)
GDP_summary.melt <- melt(data = order_GDP_scen10, id.vars=c('SECTOR'), measure.vars=c('GDP','GDP_scen'))
GDP_graph<-ggplot(data=GDP_summary.melt, aes(x=SECTOR, y=value, fill=variable)) +
  geom_bar(colour="black", stat="identity", position="dodge")+
  guides(fill=FALSE) + xlab("Sectors") + ylab("GDP") +ggtitle("Comparison of GDP Baseline and Scenario")+ theme(axis.text.x  = element_text(angle=90, size=6))

LC_graph<-ggplot(data=landuse_table, aes(x=LAND_USE, y=CHANGE)) +
  geom_bar(colour="black", stat="identity", position="dodge")+
  guides(fill=FALSE) + xlab("Land use") + ylab("Change") +ggtitle("Land Use Change")+ theme(axis.text.x  = element_text(angle=90, size=6))

#CALCULATE TOTAL LABOUR
Labour_table<-Lab.multiplier
Labour_table$Lab.multiplier<-as.numeric(format(Labour_table$Lab.multiplier, digits=3, width=5))
Labour_table<-cbind(Labour_table,fin.output.scen)
Labour_table<-cbind(Labour_table,labour)
colnames(Labour_table)[1] <- "SECTOR"
colnames(Labour_table)[2] <- "CATEGORY"
colnames(Labour_table)[4] <- "OUT_scen"
colnames(Labour_table)[5] <- "Lab_base"
test<-Labour_table$Lab.multiplier*Labour_table$OUT_scen*1000000
test<-round(test, digits=0)
Labour_table$Lab_scen<-test
Labour_table$Lab_req<-Labour_table$Lab_scen-Labour_table$Lab_base
test2<-Labour_table$Lab_req
test2<-cbind(sector, test2)
LAB_graph<-ggplot(data=test2, aes(x=V1, y=test2, fill=V2)) +
  geom_bar(colour="black", stat="identity", position="dodge")+
  guides(fill=FALSE) + xlab("Sector") + ylab("Labour requirement") +ggtitle("Impact of LU Change to Labour")+ theme(axis.text.x  = element_text(angle=90, size=6))


#EXPORT OUTPUT
GDP_scen_file<-"GDP_scenario_summary.dbf"
write.dbf(GDP_summary, GDP_scen_file,  factor2char = TRUE, max_nchar = 254)
Labour_scen_file<-"LAB_scenario_summary.dbf"
write.dbf(Labour_table, Labour_scen_file,  factor2char = TRUE, max_nchar = 254)

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Trade-off Analysis (TA) Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 2: Regional economic-Impact of land use change\\b0\\fs20"
date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1<-"\\b\\fs24 Impact of land use change to GDP \\b0\\fs20"

# ==== Report 0. Cover=====
rtffile <- RTF("LUMENS_TA-3_LUC-Impact_report.doc", font.size=11, width = 8.267, height = 11.692, omi = c(0,0,0,0))
# INPUT
img_location <- "C:/LUMENS_modified_scripts/Report/Slide3.PNG"
# loading the .png image to be edited
cover <- image_read(img_location)
# to display, only requires to execute the variable name, e.g.: "> cover"
# adding text at the desired location
text_submodule <- paste("Sub-Modul Ekonomi Regional\n\nSimulasi PDRB dari Perubahan Penggunaan Lahan\n", location, ", ", "Tahun ", I_O_period, sep="")
cover_image <- image_annotate(cover, text_submodule, size = 23, gravity = "southwest", color = "white", location = "+46+220", font = "Helvetica")
cover_image <- image_write(cover_image)
# 'gravity' defines the 'baseline' anchor of annotation. "southwest" defines the text shoul be anchored on bottom left of the image
# 'location' defines the relative location of the text to the anchor defined in 'gravity'
# configure font type
addPng(rtffile, cover_image, width = 8.267, height = 11.692)
addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1,1,1,1))

if (file.exists("C:/Program Files (x86)/LUMENS")){
  addPng (rtffile, "C:/Program Files (x86)/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
} else{
  addPng (rtffile, "C:/Program Files/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
}
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 1. Land use change\\b0\\fs20.")
addTable(rtffile,landuse_table,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,LC_graph)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 2. Impact of land use change to GDP\\b0\\fs20.")
addTable(rtffile,GDP_summary,font.size=6)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,GDP_graph)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 3. Impact of land use change to labour requirement\\b0\\fs20.")
addTable(rtffile,Labour_table,font.size=6)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,LAB_graph)
done(rtffile)




