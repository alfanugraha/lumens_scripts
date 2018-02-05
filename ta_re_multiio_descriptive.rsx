#Regional Economy Multiple Time Series I-O Descriptive Analysis
##TA-PostgreSQL=group
##int_con_file_1=string
##int_con_file_2=string
##add_val_file_1=string
##add_val_file_2=string
##fin_dem_file_1=string
##fin_dem_file_2=string
##add_val_struc_file=string
##fin_dem_struc_file=string
##sector_file=string
##labour_file_1=string
##labour_file_2=string
##unit=string
##area_name=string
##I_O_period_1=number 2000
##I_O_period_2=number 2000
##location=string

library(reshape2)
library(ggplot2)
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
work_dir<-paste(log.file[1,1], "/", log.file[1,2],"/TA/MultipleTimeSeries_IO", sep="")
dir.create(work_dir)

# SET WORKING DIRECTORY
setwd(work_dir)

# WRITING TA PROJECT FILE
filename<-paste("TA_projec_", eval(parse(text=(paste("Sys.Date ()")))), ".lms", sep="")
date<-Sys.Date()
sink(filename)
cat("LUMENS TA Module Project File")
cat("\n")
cat(as.character(date))
cat("\n")
cat(int_con_file_1)
cat("\n")
cat(int_con_file_2)
cat("\n")
cat(add_val_file_1)
cat("\n")
cat(add_val_file_2)
cat("\n")
cat(fin_dem_file_1)
cat("\n")
cat(fin_dem_file_2)
cat("\n")
cat(add_val_struc_file)
cat("\n")
cat(fin_dem_struc_file)
cat("\n")
cat(sector_file)
cat("\n")
cat(labour_file_1)
cat("\n")
cat(labour_file_2)
cat("\n")
cat(unit)
cat("\n")
cat(area_name)
cat("\n")
cat(I_O_period_1)
sink()

# READ INPUT FILE
int_con_1<- read.table(int_con_file_1, header=FALSE, sep=",",)
int_con_2<- read.table(int_con_file_2, header=FALSE, sep=",",)
add_val_1<- read.table(add_val_file_1, header=FALSE, sep=",",)
add_val_2<- read.table(add_val_file_2, header=FALSE, sep=",",)
fin_dem_1<- read.table(fin_dem_file_1, header=FALSE, sep=",",)
fin_dem_2<- read.table(fin_dem_file_2, header=FALSE, sep=",",)
fin_dem_struc<- read.table(fin_dem_struc_file, header=FALSE, sep=",",)
add_val_struc<- read.table(add_val_struc_file, header=FALSE, sep=",",)
sector<- read.table(sector_file, header=FALSE, sep=",",)
labour_1<- read.table(labour_file_1, header=FALSE, sep=",",)
labour_2<- read.table(labour_file_2, header=FALSE, sep=",",)
int_con_1.m<-as.matrix(int_con_1)
int_con_2.m<-as.matrix(int_con_2)
add_val_1.m<-as.matrix(add_val_1)
add_val_2.m<-as.matrix(add_val_2)
dim<-ncol(int_con_1.m)

#CALCULATE LEONTIEF INVERSE FOR PERIOD 1
int_con_1.ctot<-colSums(int_con_1.m)
add_val_1.ctot<-colSums(add_val_1.m)
fin_con_1<- 1/(int_con_1.ctot+add_val_1.ctot)
fin_con_1[is.infinite(fin_con_1)]<-0
t.input.invers.1<-diag(fin_con_1)
A_1<-int_con_1.m %*% t.input.invers.1
I<-as.matrix(diag(dim))
I_A_1<-I-A_1
Leontief_1<-solve(I_A_1)

#CALCULATE LEONTIEF INVERSE FOR PERIOD 2
int_con_2.ctot<-colSums(int_con_2.m)
add_val_2.ctot<-colSums(add_val_2.m)
fin_con_2<- 1/(int_con_2.ctot+add_val_2.ctot)
fin_con_2[is.infinite(fin_con_2)]<-0
t.input.invers.2<-diag(fin_con_2)
A_2<-int_con_2.m %*% t.input.invers.2
I_A_2<-I-A_2
Leontief_2<-solve(I_A_2)

#DIRECT BACKWARD LINKAGES FOR PERIOD 1
DBL_1<-colSums(Leontief_1)
DBL_1<-DBL_1/(mean(DBL_1))
DBL_1<-cbind(sector,DBL_1)
colnames(DBL_1)[3] <- "DBL"
order_DBL_1 <- as.data.frame(DBL_1[order(-DBL_1$DBL),])
order_DBL20_1<-head(order_DBL_1,n=20)
colnames(order_DBL20_1)[1] <- "SECTOR"
colnames(order_DBL20_1)[2] <- "CATEGORY"
BPD_graph_1<-ggplot(data=order_DBL20_1, aes(x=SECTOR, y=DBL, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() +  xlab("Sectors") + ylab("Value") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

#DIRECT BACKWARD LINKAGES FOR PERIOD 2
DBL_2<-colSums(Leontief_2)
DBL_2<-DBL_2/(mean(DBL_2))
DBL_2<-cbind(sector,DBL_2)
colnames(DBL_2)[3] <- "DBL"
order_DBL_2 <- as.data.frame(DBL_2[order(-DBL_2$DBL),])
order_DBL20_2<-head(order_DBL_2,n=20)
colnames(order_DBL20_2)[1] <- "SECTOR"
colnames(order_DBL20_2)[2] <- "CATEGORY"
BPD_graph_2<-ggplot(data=order_DBL20_2, aes(x=SECTOR, y=DBL, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() +  xlab("Sectors") + ylab("Value") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

#DIRECT FORWARD LINKAGES FOR PERIOD 1
DFL_1<-rowSums(Leontief_1)
DFL_1<-DFL_1/(mean(DFL_1))
DFL_1<-cbind(sector,DFL_1)
colnames(DFL_1)[3] <- "DFL"
order_DFL_1 <- as.data.frame(DFL_1[order(-DFL_1$DFL),])
order_DFL20_1<-head(order_DFL_1,n=20)
colnames(order_DFL20_1)[1] <- "SECTOR"
colnames(order_DFL20_1)[2] <- "CATEGORY"
FPD_graph_1<-ggplot(data=order_DFL20_1, aes(x=SECTOR, y=DFL, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() +  xlab("Sectors") + ylab("Value") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

#DIRECT FORWARD LINKAGES FOR PERIOD 1
DFL_2<-rowSums(Leontief_2)
DFL_2<-DFL_2/(mean(DFL_2))
DFL_2<-cbind(sector,DFL_2)
colnames(DFL_2)[3] <- "DFL"
order_DFL_2 <- as.data.frame(DFL_2[order(-DFL_2$DFL),])
order_DFL20_2<-head(order_DFL_2,n=20)
colnames(order_DFL20_2)[1] <- "SECTOR"
colnames(order_DFL20_2)[2] <- "CATEGORY"
FPD_graph_2<-ggplot(data=order_DFL20_2, aes(x=SECTOR, y=DFL, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Value") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))


#CREATE LINKAGES TABLE FOR PERIOD 1
DBL_temp_1<-colSums(Leontief_1)
BPD_temp_1<-DBL_temp_1/(mean(as.matrix(DBL_temp_1)))
DFL_temp_1<-rowSums(Leontief_1)
FPD_temp_1<-DFL_temp_1/(mean(as.matrix(DFL_temp_1)))
DBL_temp_1<-as.data.frame(round(DBL_temp_1, digits=2))
BPD_temp_1<-as.data.frame(round(BPD_temp_1, digits=2))
DFL_temp_1<-as.data.frame(round(DFL_temp_1, digits=2))
FPD_temp_1<-as.data.frame(round(FPD_temp_1, digits=2))
Linkages_table_1<-cbind(sector,DBL_temp_1,DFL_temp_1,BPD_temp_1,FPD_temp_1)
colnames(Linkages_table_1)[1] <- "SECTOR"
colnames(Linkages_table_1)[2] <- "CATEGORY"
colnames(Linkages_table_1)[3] <- "DBL"
colnames(Linkages_table_1)[4] <- "DFL"
colnames(Linkages_table_1)[5] <- "BPD"
colnames(Linkages_table_1)[6] <- "FPD"
PRS_graph_1<-ggplot(Linkages_table_1, aes(x=BPD, y=FPD, color=CATEGORY)) + geom_point(shape=19, size=5) + geom_hline(aes(yintercept=1), colour="#BB0000", linetype="dashed") + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")

#CREATE LINKAGES TABLE FOR PERIOD 2
DBL_temp_2<-colSums(Leontief_2)
BPD_temp_2<-DBL_temp_2/(mean(as.matrix(DBL_temp_2)))
DFL_temp_2<-rowSums(Leontief_2)
FPD_temp_2<-DFL_temp_2/(mean(as.matrix(DFL_temp_2)))
DBL_temp_2<-as.data.frame(round(DBL_temp_2, digits=2))
BPD_temp_2<-as.data.frame(round(BPD_temp_2, digits=2))
DFL_temp_2<-as.data.frame(round(DFL_temp_2, digits=2))
FPD_temp_2<-as.data.frame(round(FPD_temp_2, digits=2))
Linkages_table_2<-cbind(sector,DBL_temp_2,DFL_temp_2,BPD_temp_2,FPD_temp_2)
colnames(Linkages_table_2)[1] <- "SECTOR"
colnames(Linkages_table_2)[2] <- "CATEGORY"
colnames(Linkages_table_2)[3] <- "DBL"
colnames(Linkages_table_2)[4] <- "DFL"
colnames(Linkages_table_2)[5] <- "BPD"
colnames(Linkages_table_2)[6] <- "FPD"
PRS_graph_2<-ggplot(Linkages_table_2, aes(x=BPD, y=FPD, color=CATEGORY)) + geom_point(shape=19, size=5) + geom_hline(aes(yintercept=1), colour="#BB0000", linetype="dashed") + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")

#CREATE INTERPERIOD LINKAGES TABLE
Linkages_table_lab<-Linkages_table_1[,1:2]
Linkages_table_Out<-round((Linkages_table_2[,3:6]-Linkages_table_1[,3:6]), digits=2)
Linkages_table_intp<-cbind(Linkages_table_lab, Linkages_table_Out)

#SELECTION OF PRIMARY SECTOR FOR PERIOD 1
P.sector_1<-cbind(DBL_1,DFL_1)
colnames (P.sector_1) [1]<-"Sectors"
colnames (P.sector_1) [3]<-"BPD"
colnames (P.sector_1) [6]<-"FPD"
P.sector_1[4]<-NULL
P.sector_1[4]<-NULL
P.sector.selected_1 <- P.sector_1[ which(P.sector_1$BPD >= 1),]
P.sector.selected_1 <- P.sector.selected_1[ which(P.sector.selected_1$FPD >= 1),]
colnames(P.sector.selected_1)[1] <- "SECTOR"
colnames(P.sector.selected_1)[2] <- "CATEGORY"
P.sector.selected_1$BPD<-round(P.sector.selected_1$BPD, digits=5)
P.sector.selected_1$FPD<-round(P.sector.selected_1$FPD, digits=5)

#SELECTION OF PRIMARY SECTOR FOR PERIOD 2
P.sector_2<-cbind(DBL_2,DFL_2)
colnames (P.sector_2) [1]<-"Sectors"
colnames (P.sector_2) [3]<-"BPD"
colnames (P.sector_2) [6]<-"FPD"
P.sector_2[4]<-NULL
P.sector_2[4]<-NULL
P.sector.selected_2 <- P.sector_2[ which(P.sector_2$BPD >= 1),]
P.sector.selected_2 <- P.sector.selected_2[ which(P.sector.selected_2$FPD >= 1),]
colnames(P.sector.selected_2)[1] <- "SECTOR"
colnames(P.sector.selected_2)[2] <- "CATEGORY"
P.sector.selected_2$BPD<-round(P.sector.selected_2$BPD, digits=5)
P.sector.selected_2$FPD<-round(P.sector.selected_2$FPD, digits=5)

#GDP FOR PERIOD 1
GDP.val_1<-as.data.frame(add_val_1.m[2,]+add_val_1.m[3,])
GDP.val_1.m<-as.matrix(GDP.val_1)
GDP.val_1.m<-as.numeric(GDP.val_1.m)
OUTPUT.val_1<-as.data.frame(add_val_1.m[2,]+add_val_1.m[3,]+add_val_1.m[1,]+int_con_1.ctot)
OUTPUT.val_1.m<-as.matrix(OUTPUT.val_1)
OUTPUT.val_1.m<-as.numeric(OUTPUT.val_1.m)
GDP_1<-cbind(sector,GDP.val_1,OUTPUT.val_1)
colnames(GDP_1)[1] <- "SECTOR"
colnames(GDP_1)[2] <- "CATEGORY"
colnames(GDP_1)[3] <- "GDP"
colnames(GDP_1)[4] <- "OUTPUT"
GDP_1$GDP_PROP<-GDP_1$GDP/GDP_1$OUTPUT
GDP_1[is.na(GDP_1)]<-0
colnames(GDP_1)[5] <- "P_OUTPUT"
GDP_tot_1<-as.matrix(GDP_1$GDP)
GDP_tot_1<-colSums(GDP_tot_1)
GDP_tot_1<-round(GDP_tot_1, digits=2)
GDP_1$P_GDP<-round((GDP_1$GDP/GDP_tot_1), digits=2)
order_GDP_1 <- as.data.frame(GDP_1[order(-GDP_1$GDP),])
order_GDP10_1<-head(order_GDP_1,n=20)
GDP_graph_1<-ggplot(data=order_GDP10_1, aes(x=SECTOR, y=GDP, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("GDP") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6),
axis.text.x = element_text(size = 8))
GDP_1$GDP<-round(GDP_1$GDP, digits=1)
GDP_1$OUTPUT<-round(GDP_1$OUTPUT, digits=1)
GDP_1$P_OUTPUT<-round(GDP_1$P_OUTPUT, digits=2)
GDP_1$P_GDP<-round(GDP_1$P_GDP, digits=2)

#GDP FOR PERIOD 2
GDP.val_2<-as.data.frame(add_val_2.m[2,]+add_val_2.m[3,])
GDP.val_2.m<-as.matrix(GDP.val_2)
GDP.val_2.m<-as.numeric(GDP.val_2.m)
OUTPUT.val_2<-as.data.frame(add_val_2.m[2,]+add_val_2.m[3,]+add_val_2.m[1,]+int_con_2.ctot)
OUTPUT.val_2.m<-as.matrix(OUTPUT.val_2)
OUTPUT.val_2.m<-as.numeric(OUTPUT.val_2.m)
GDP_2<-cbind(sector,GDP.val_2,OUTPUT.val_2)
colnames(GDP_2)[1] <- "SECTOR"
colnames(GDP_2)[2] <- "CATEGORY"
colnames(GDP_2)[3] <- "GDP"
colnames(GDP_2)[4] <- "OUTPUT"
GDP_2$GDP_PROP<-GDP_2$GDP/GDP_2$OUTPUT
GDP_2[is.na(GDP_2)]<-0
colnames(GDP_2)[5] <- "P_OUTPUT"
GDP_tot_2<-as.matrix(GDP_2$GDP)
GDP_tot_2<-colSums(GDP_tot_2)
GDP_tot_2<-round(GDP_tot_2, digits=2)
GDP_2$P_GDP<-round((GDP_2$GDP/GDP_tot_2), digits=2)
order_GDP_2 <- as.data.frame(GDP_2[order(-GDP_2$GDP),])
order_GDP10_2<-head(order_GDP_2,n=20)
GDP_graph_2<-ggplot(data=order_GDP10_2, aes(x=SECTOR, y=GDP, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("GDP") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6),
axis.text.x = element_text(size = 8))
GDP_2$GDP<-round(GDP_2$GDP, digits=1)
GDP_2$OUTPUT<-round(GDP_2$OUTPUT, digits=1)
GDP_2$P_OUTPUT<-round(GDP_2$P_OUTPUT, digits=2)
GDP_2$P_GDP<-round(GDP_2$P_GDP, digits=2)

#INTERPERIOD GDP CHANGE
GDP_tot_chg<-GDP_tot_2-GDP_tot_1
GDP_tot_chg<-round(GDP_tot_chg, digits=2)
GDP_chg_lab<-GDP_1[,1:2]
GDP_chg_out<-GDP_2[,3:6]-GDP_1[,3:6]
GDP_tot_intp<-cbind(GDP_chg_lab, GDP_chg_out)
GDP_tot_intp$GDP<-round(GDP_tot_intp$GDP, digits=1)
GDP_tot_intp$OUTPUT<-round(GDP_tot_intp$OUTPUT, digits=1)
GDP_tot_intp$P_OUTPUT<-round(GDP_tot_intp$GDP/GDP_tot_intp$OUTPUT , digits=2)
GDP_tot_intp$P_GDP<-round((GDP_tot_intp$GDP/GDP_tot_chg), digits=2)
order_GDP_intp <- as.data.frame(GDP_tot_intp[order(-GDP_tot_intp$GDP),])
order_GDP10_intp<-head(order_GDP_intp,n=20)
GDP_graph_intp<-ggplot(data=order_GDP10_intp, aes(x=SECTOR, y=GDP, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("GDP") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6),
axis.text.x = element_text(size = 8))

#OUTPUT MULTIPLIER FOR PERIOD 1
Out.multiplier_1<-colSums(Leontief_1)
Out.multiplier_1<-cbind(sector,Out.multiplier_1)
colnames(Out.multiplier_1)[1]<-"SECTOR"
colnames(Out.multiplier_1)[2]<-"CATEGORY"
order_Out.multiplier_1 <- as.data.frame(Out.multiplier_1[order(-Out.multiplier_1$Out.multiplier_1),])
order_Out.multiplier_1 <-head(order_Out.multiplier_1,n=20)
OMPL_graph_1<-ggplot(data=order_Out.multiplier_1, aes(x=SECTOR, y=Out.multiplier_1, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Output multiplier") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6),
axis.text.x = element_text(size = 8))

#OUTPUT MULTIPLIER FOR PERIOD 2
Out.multiplier_2<-colSums(Leontief_2)
Out.multiplier_2<-cbind(sector,Out.multiplier_2)
colnames(Out.multiplier_2)[1]<-"SECTOR"
colnames(Out.multiplier_2)[2]<-"CATEGORY"
order_Out.multiplier_2 <- as.data.frame(Out.multiplier_2[order(-Out.multiplier_2$Out.multiplier_2),])
order_Out.multiplier_2 <-head(order_Out.multiplier_2,n=20)
OMPL_graph_2<-ggplot(data=order_Out.multiplier_2, aes(x=SECTOR, y=Out.multiplier_2, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() +  xlab("Sectors") + ylab("Output multiplier") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

#INCOME MULTIPLIER FOR PERIOD 1
V.income_1<-as.matrix(GDP.val_1*fin_con_1)
Inc.multiplier_1<-Leontief_1%*%V.income_1
multiplier_1<-cbind(Out.multiplier_1,Inc.multiplier_1)
Inc.multiplier_1<-cbind(sector,Inc.multiplier_1)
colnames(Inc.multiplier_1)[1]<-"SECTOR"
colnames(Inc.multiplier_1)[2]<-"CATEGORY"
colnames(Inc.multiplier_1)[3]<-"Inc.multiplier"
order_Inc.multiplier_1 <- as.data.frame(Inc.multiplier_1[order(-Inc.multiplier_1$Inc.multiplier),])
order_Inc.multiplier_1 <-head(order_Inc.multiplier_1,n=20)
IMPL_graph_1<-ggplot(data=order_Inc.multiplier_1, aes(x=SECTOR, y=Inc.multiplier, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Income multiplier") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

#INCOME MULTIPLIER FOR PERIOD 2
V.income_2<-as.matrix(GDP.val_2*fin_con_2)
Inc.multiplier_2<-Leontief_2%*%V.income_2
multiplier_2<-cbind(Out.multiplier_2,Inc.multiplier_2)
Inc.multiplier_2<-cbind(sector,Inc.multiplier_2)
colnames(Inc.multiplier_2)[1]<-"SECTOR"
colnames(Inc.multiplier_2)[2]<-"CATEGORY"
colnames(Inc.multiplier_2)[3]<-"Inc.multiplier"
order_Inc.multiplier_2 <- as.data.frame(Inc.multiplier_2[order(-Inc.multiplier_2$Inc.multiplier),])
order_Inc.multiplier_2 <-head(order_Inc.multiplier_2,n=20)
IMPL_graph_2<-ggplot(data=order_Inc.multiplier_2, aes(x=SECTOR, y=Inc.multiplier, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Income multiplier") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

#LABOUR MULTIPLIER FOR PERIOD 1
labour_1.m<-as.matrix(labour_1*fin_con_1)
labour_1.m<-labour_1.m/1000000
Lab.multiplier_1<-Leontief_1%*%labour_1.m
multiplier_1<-cbind(multiplier_1,Lab.multiplier_1)
colnames(multiplier_1)[1] <- "SECTOR"
colnames(multiplier_1)[2] <- "CATEGORY"
colnames(multiplier_1)[3] <- "Out.multiplier"
colnames(multiplier_1)[4] <- "Inc.multiplier"
colnames(multiplier_1)[5] <- "Lab.multiplier"
multiplier_1$Out.multiplier<-round(multiplier_1$Out.multiplier, digits=3)
Lab.multiplier_1<-cbind(sector,Lab.multiplier_1)
colnames(Lab.multiplier_1)[1]<-"SECTOR"
colnames(Lab.multiplier_1)[2]<-"CATEGORY"
colnames(Lab.multiplier_1)[3]<-"Lab.multiplier"
order_Lab.multiplier_1 <- as.data.frame(Lab.multiplier_1[order(-Lab.multiplier_1$Lab.multiplier),])
order_Lab.multiplier_1 <-head(order_Lab.multiplier_1,n=20)
LMPL_graph_1<-ggplot(data=order_Lab.multiplier_1, aes(x=SECTOR, y=Lab.multiplier, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Labour multiplier") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
multiplier_1$Inc.multiplier<-round(multiplier_1$Inc.multiplier, digits=3)
multiplier_1$Lab.multiplier<-as.numeric(format(multiplier_1$Lab.multiplier, digits=3, width=5))

#LABOUR MULTIPLIER FOR PERIOD 2
labour_2.m<-as.matrix(labour_2*fin_con_2)
labour_2.m<-labour_2.m/1000000
Lab.multiplier_2<-Leontief_2%*%labour_2.m
multiplier_2<-cbind(multiplier_2,Lab.multiplier_2)
colnames(multiplier_2)[1] <- "SECTOR"
colnames(multiplier_2)[2] <- "CATEGORY"
colnames(multiplier_2)[3] <- "Out.multiplier"
colnames(multiplier_2)[4] <- "Inc.multiplier"
colnames(multiplier_2)[5] <- "Lab.multiplier"
multiplier_2$Out.multiplier<-round(multiplier_2$Out.multiplier, digits=3)
Lab.multiplier_2<-cbind(sector,Lab.multiplier_2)
colnames(Lab.multiplier_2)[1]<-"SECTOR"
colnames(Lab.multiplier_2)[2]<-"CATEGORY"
colnames(Lab.multiplier_2)[3]<-"Lab.multiplier"
order_Lab.multiplier_2 <- as.data.frame(Lab.multiplier_2[order(-Lab.multiplier_2$Lab.multiplier),])
order_Lab.multiplier_2 <-head(order_Lab.multiplier_2,n=20)
LMPL_graph_2<-ggplot(data=order_Lab.multiplier_2, aes(x=SECTOR, y=Lab.multiplier, fill=CATEGORY)) +
geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Labour multiplier") +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
multiplier_2$Inc.multiplier<-round(multiplier_2$Inc.multiplier, digits=3)
multiplier_2$Lab.multiplier<-as.numeric(format(multiplier_2$Lab.multiplier, digits=3, width=5))

#INTERPERIOD MULTIPLIER CHANGE
multiplier_intp_lab<-multiplier_1[,1:2]
multiplier_intp_out<-multiplier_2[,3:5]-multiplier_1[,3:5]
multiplier_intp_out[,1:2]<-round(multiplier_intp_out[,1:2], digits=2)
multiplier_intp_out[,3]<-as.numeric(format(multiplier_intp_out[,3], digits=3, width=5))
multiplier_intp_chg<-cbind(multiplier_intp_lab, multiplier_intp_out)

#COMBINE MULTIPLIER FOR PERIOD 1
sel.multiplier_1<-multiplier_1[ which(multiplier_1$Out.multiplier > 1),]
sel.multiplier_1<-sel.multiplier_1[ which(sel.multiplier_1$Inc.multiplier > 1),]

#COMBINE MULTIPLIER FOR PERIOD 2
sel.multiplier_2<-multiplier_2[ which(multiplier_2$Out.multiplier > 1),]
sel.multiplier_2<-sel.multiplier_2[ which(sel.multiplier_2$Inc.multiplier > 1),]


#EXPORT OUTPUT FOR PERIOD 1
Leontief_df_1<-as.data.frame(Leontief_1)
Leontief_matrix_1<-"Leontief_matrix_1"
write.dbf(Leontief_df_1, Leontief_matrix_1, factor2char = TRUE, max_nchar = 254)
Linkages_1<-"Sectoral_linkages_1"
write.dbf(Linkages_table_1, Linkages_1, factor2char = TRUE, max_nchar = 254)
PDRB_1<-"Sectoral_GDP_1"
write.dbf(GDP_1, PDRB_1,factor2char = TRUE, max_nchar = 254)
PENGGANDA_1<-"Sectoral_multiplier_1"
write.dbf(multiplier_1, PENGGANDA_1,factor2char = TRUE, max_nchar = 254)

#EXPORT OUTPUT FOR PERIOD 2
Leontief_df_2<-as.data.frame(Leontief_2)
Leontief_matrix_2<-"Leontief_matrix_2"
write.dbf(Leontief_df_2, Leontief_matrix_2, factor2char = TRUE, max_nchar = 254)
Linkages_2<-"Sectoral_linkages_2"
write.dbf(Linkages_table_2, Linkages_2, factor2char = TRUE, max_nchar = 254)
PDRB_2<-"Sectoral_GDP_2"
write.dbf(GDP_2, PDRB_2,factor2char = TRUE, max_nchar = 254)
PENGGANDA_2<-"Sectoral_multiplier_2"
write.dbf(multiplier_2, PENGGANDA_2,factor2char = TRUE, max_nchar = 254)

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Trade-off Analysis (TA) Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 2: Regional Economic-Descriptive Analysis (Time Series I-O)\\b0\\fs20"
date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1<-"\\b\\fs24 1.Analysis of Sectoral Linkages \\b0\\fs20"
chapter2<-"\\b\\fs24 2.Analysis of GDP \\b0\\fs20"
chapter2_1<-"\\b\\fs20 Total GDP \\b0\\fs20"
chapter3<-"\\b\\fs24 2.Analysis of multiplier \\b0\\fs20"
area_name_rep<-paste("\\b", "\\fs20", area_name, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", I_O_period_1)
I_O_period_2_rep<-paste("\\b","\\fs20", I_O_period_2)

# ==== Report 0. Cover=====
rtffile <- RTF("LUMENS_TA-1_MultiIO_report.doc", font.size=11, width = 8.267, height = 11.692, omi = c(0,0,0,0))
# INPUT
img_location <- "C:/LUMENS_modified_scripts/Report/Slide3.PNG"
# loading the .png image to be edited
cover <- image_read(img_location)
# to display, only requires to execute the variable name, e.g.: "> cover"
# adding text at the desired location
text_submodule <- paste("Sub-Modul Ekonomi Regional\n\nAnalisis Deskriptif Sektor Ekonomi\n", location, ", ", "Periode ", I_O_period_1_rep, "-", I_O_period_2_rep, sep="")
cover_image <- image_annotate(cover, text_submodule, size = 23, gravity = "southwest", color = "white", location = "+46+220", font = "Arial")
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
addParagraph(rtffile, paste("\\b \\fs20 Table 1.1 Sectoral linkages for \\b0 \\fs20", area_name_rep, I_O_period_1_rep, sep=" "))
addTable(rtffile,Linkages_table_1,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 1.2 Sectoral linkages for \\b0 \\fs20", area_name_rep, I_O_period_2_rep, sep=" "))
addTable(rtffile,Linkages_table_2,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 1.3\\b0 \\fs20 ", area_name_rep, "\\b \\fs20Sectoral linkages changes during \\b0 \\fs20", I_O_period_1_rep, "-", I_O_period_2_rep, sep=" "))
addTable(rtffile,Linkages_table_intp,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,BPD_graph_1)
addParagraph(rtffile, paste("\\b \\fs20 Figure 1.1 Twenty sectors with highest Backward power of dispersion for \\b0 \\fs20", area_name_rep, I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,BPD_graph_2)
addParagraph(rtffile, paste("\\b \\fs20 Figure 1.2 Twenty sectors with highest Backward power of dispersion for \\b0 \\fs20", area_name_rep, I_O_period_2_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,FPD_graph_1)
addParagraph(rtffile, paste("\\b \\fs20 Figure 2.1 Twenty sectors with highest Forward power of dispersion for \\b0 \\fs20", area_name_rep, I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,FPD_graph_2)
addParagraph(rtffile, paste("\\b \\fs20 Figure 2.2 Twenty sectors with highest Forward power of dispersion for \\b0 \\fs20", area_name_rep, I_O_period_2_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,PRS_graph_1)
addParagraph(rtffile, paste("\\b \\fs20 Figure 3.1 Sectoral typology based on linkages analysis for \\b0 \\fs20", area_name_rep, I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,PRS_graph_2)
addParagraph(rtffile, paste("\\b \\fs20 Figure 3.2 Sectoral typology based on linkages analysis for \\b0 \\fs20", area_name_rep, I_O_period_2_rep, sep=" "))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 2.1 Primary sectors based on potential linkage for \\b0 \\fs20", area_name_rep, I_O_period_1_rep, sep=" "))
addTable(rtffile,P.sector.selected_1,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 2.2 Primary sectors based on potential linkage for \\b0 \\fs20", area_name_rep, I_O_period_2_rep, sep=" "))
addTable(rtffile,P.sector.selected_2,font.size=8)
addNewLine(rtffile)
addPageBreak(rtffile)
addParagraph(rtffile, chapter2)
addNewLine(rtffile)
addParagraph(rtffile, paste(chapter2_1 ,"\\b \\fs20 for \\b0 \\fs20",area_name_rep, I_O_period_1_rep, sep=" "))
addParagraph(rtffile, GDP_tot_1)
addNewLine(rtffile)
addParagraph(rtffile, paste(chapter2_1 ,"\\b \\fs20 for \\b0 \\fs20",area_name_rep, I_O_period_2_rep, sep=" "))
addParagraph(rtffile, GDP_tot_2)
addNewLine(rtffile)
addParagraph(rtffile, paste(area_name_rep, "\\b \\fs20 Total GDP changes during \\b0 \\fs20", I_O_period_1_rep, "-" , I_O_period_2_rep, sep=" "))
addParagraph(rtffile, GDP_tot_chg)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 3.1 Sectoral GDP for \\b0 \\fs20", area_name_rep, I_O_period_1_rep, sep=" "))
addTable(rtffile,GDP_1,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 3.2 Sectoral GDP for \\b0 \\fs20", area_name_rep, I_O_period_2_rep, sep=" "))
addTable(rtffile,GDP_2,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 3.3 \\b0 \\fs20",area_name_rep, "\\b \\fs20 Sectoral GDP changes during \\b0 \\fs20", I_O_period_1_rep, "-" , I_O_period_2_rep, sep=" "))
addTable(rtffile,GDP_tot_intp[,1:4],font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,GDP_graph_1)
addParagraph(rtffile, paste("\\b \\fs20 Figure 4.1 Twenty sectors with highest GDP for \\b0 \\fs20", area_name_rep, I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,GDP_graph_2)
addParagraph(rtffile, paste("\\b \\fs20 Figure 4.2 Twenty sectors with highest GDP for \\b0 \\fs20", area_name_rep, I_O_period_2_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,GDP_graph_intp)
addParagraph(rtffile, paste("\\b \\fs20 Figure 4.3 Twenty sectors with highest GDP change for \\b0 \\fs20", area_name_rep, "\\b \\fs20 during \\b0 \\fs20", I_O_period_1_rep, "-", I_O_period_2_rep, sep=" "))
addPageBreak(rtffile)
addParagraph(rtffile, chapter3)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 5.1 Sectoral multiplier for \\b0 \\fs20", area_name_rep, I_O_period_1_rep, sep=" "))
addTable(rtffile,multiplier_1,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 5.2 Sectoral multiplier for \\b0 \\fs20", area_name_rep, I_O_period_2_rep, sep=" "))
addTable(rtffile,multiplier_2,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 5.3 \\b0 \\fs20", area_name_rep, "\\b \\fs20 Sectoral multiplier change during \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
addTable(rtffile,multiplier_intp_chg,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,OMPL_graph_1)
addParagraph(rtffile, paste("\\b \\fs20 Figure 5.1 Twenty sectors with highest Output multiplier for \\b0 \\fs20", area_name_rep, I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,OMPL_graph_2)
addParagraph(rtffile, paste("\\b \\fs20 Figure 5.2 Twenty sectors with highest Output multiplier for \\b0 \\fs20", area_name_rep, I_O_period_2_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,IMPL_graph_1)
addParagraph(rtffile, paste("\\b\\fs20 Figure 6.1 Twenty sectors with highest Income multiplier for\\b0\\fs20.", area_name_rep, I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,IMPL_graph_2)
addParagraph(rtffile, paste("\\b\\fs20 Figure 6.2 Twenty sectors with highest Income multiplier for\\b0\\fs20.", area_name_rep, I_O_period_2_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,LMPL_graph_1)
addParagraph(rtffile, paste("\\b\\fs20 Figure 7.1 Twenty sectors with highest Labour multiplier for\\b0\\fs20.", area_name_rep, I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,LMPL_graph_2)
addParagraph(rtffile, paste("\\b\\fs20 Figure 7.2 Twenty sectors with highest Labour multiplier for\\b0\\fs20.", area_name_rep, I_O_period_2_rep, sep=" "))
addNewLine(rtffile)
done(rtffile)




