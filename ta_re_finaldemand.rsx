#Regional Economy Final Demand Change Multiplier Analysis
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
##fin_demand_scenario_file=string
##labour_file=string
##unit=string
##area_name=string
##I_O_period=number 2000
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
work_dir<-paste(log.file[1,1], "/", log.file[1,2],"/TA/FinalDemandScenario", sep="")
dir.create(work_dir)

#SET WORKING DIRECTORY
setwd(work_dir)

#WRITING TA PROJECT FILE
filename<-paste("TA_projec_", eval(parse(text=(paste("Sys.Date ()")))), ".lms", sep="")
date<-Sys.Date()
sink(filename)
cat("LUMENS TA Module Project File")
cat("\n")
cat(as.character(date))
cat("\n")
cat(int_con_file)
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
cat(fin_demand_scenario_file)
cat("\n")
cat(unit)
cat("\n")
cat(area_name)
cat("\n")
cat(I_O_period)
sink()

#READ INPUT FILE
int_con<- read.table(int_con_file, header=FALSE, sep=",",)
add_val<- read.table(add_val_file, header=FALSE, sep=",",)
fin_dem<- read.table(fin_dem_file, header=FALSE, sep=",",)
fin_dem_struc<- read.table(fin_dem_struc_file, header=FALSE, sep=",",)
add_val_struc<- read.table(add_val_struc_file, header=FALSE, sep=",",)
sector<- read.table(sector_file, header=FALSE, sep=",",)
labour<- read.table(labour_file, header=FALSE, sep=",",)
land.distribution<-read.dbf(land.distribution_file, as.is = FALSE)
land.requirement.db<-read.dbf(land.requirement_file, as.is = FALSE)
lc.list<-read.table(lc.list_file, header=FALSE, sep=",",)
int_con.m<-as.matrix(int_con)
add_val.m<-as.matrix(add_val)
dim<-ncol(int_con.m)
fin_demand_scenario<- read.table(fin_demand_scenario_file, header=FALSE, sep=",",)

#CALCULATE INVERS LEONTIEF
int_con.ctot<-colSums(int_con.m)
add_val.ctot<-colSums(add_val.m)
fin_con<- 1/(int_con.ctot+add_val.ctot)
t.input.invers<-diag(fin_con)
A<-int_con.m %*% t.input.invers
I<-as.matrix(diag(dim))
I_A<-I-A
Leontief<-solve(I_A)

#GDP
GDP.val<-as.data.frame(add_val.m[2,]+add_val.m[3,])
GDP.val.m<-as.matrix(GDP.val)
GDP.val.m<-as.numeric(GDP.val.m)
OUTPUT.val<-as.data.frame(add_val.m[2,]+add_val.m[3,]+add_val.m[1,]+int_con.ctot)
OUTPUT.val.m<-as.matrix(OUTPUT.val)
OUTPUT.val.m<-as.numeric(OUTPUT.val.m)
GDP<-cbind(sector,GDP.val,OUTPUT.val)
colnames(GDP)[1] <- "SECTOR"
colnames(GDP)[2] <- "SECTOR CLASS"
colnames(GDP)[3] <- "GDP"
colnames(GDP)[4] <- "OUTPUT"
GDP$GDP_PROP<-GDP$GDP/GDP$OUTPUT
order_GDP <- as.data.frame(GDP[order(-GDP$GDP),])
order_GDP10<-head(order_GDP,n=20)
GDP_tot<-as.matrix(GDP$GDP)
GDP_tot<-colSums(GDP_tot)

#MODEL FINAL DEMAND
fin_dem.rtot<-rowSums(fin_dem)
int_con.rtot<-rowSums(int_con)
demand<-fin_dem.rtot+int_con.rtot
land.requirement.coeff<-land.requirement.db$LRC
land.productivity.coeff<-land.requirement.db$LPC
land.distribution.prop<-as.matrix(land.distribution)
land.distribution.prop.r<-t(land.distribution.prop)


#CALCULATE LAND USE REQUIREMENT FROM CHANGE IN FINAL DEMAND SCENARIO

#base on absolute number in financial unit
temp_output0<-as.matrix(fin_demand_scenario)
temp_output0<-as.numeric(temp_output0)
temp_output0<-Leontief %*% temp_output0
temp_output<-cbind(OUTPUT.val,temp_output0)
colnames(temp_output)[1] <- "OUTPUT"
colnames(temp_output)[2] <- "SCEN_OUTPUT"
temp_output$delt_output<-temp_output$SCEN_OUTPUT-temp_output$OUTPUT
delt_output<-as.numeric(as.matrix(temp_output$delt_output))
R_C<-land.distribution.prop.r %*% diag(land.requirement.coeff)
delta_L<-R_C  %*% delt_output
delta_L2<-round(delta_L, digits=3)
delta_L2<-cbind(lc.list,delta_L2)

#PRODUCE OUTPUT
GDP_base<-GDP$GDP
GDP_table<-cbind(sector, temp_output$OUTPUT, temp_output$SCEN_OUTPUT, GDP_base)
colnames(GDP_table)[1] <- "SECTOR"
colnames(GDP_table)[2] <- "CATEGORY"
colnames(GDP_table)[3] <- "OUT_base"
colnames(GDP_table)[4] <- "OUT_scen"
GDP_table$GDP_scen<-(GDP_table$GDP_base/GDP_table$OUT_base)*GDP_table$OUT_scen
GDP_table$GDP_change<-GDP_table$GDP_scen-GDP_table$GDP_base
GDP_table$GDP_base<-round(GDP_table$GDP_base, digits=2)
GDP_table$GDP_scen<-round(GDP_table$GDP_scen, digits=2)
GDP_table$GDP_change<-round(GDP_table$GDP_change, digits=2)
GDP_table$OUT_base<-round(GDP_table$OUT_base, digits=2)
GDP_table$OUT_scen<-round(GDP_table$OUT_scen, digits=2)

#EXPORT OUTPUT
LUC_scen_file<-"LUC_scenario_summary.dbf"
write.dbf(delta_L2, LUC_scen_file,  factor2char = TRUE, max_nchar = 254)
GDP_scen_file<-"GDP_scenario_summary.dbf"
write.dbf(GDP_table, GDP_scen_file,  factor2char = TRUE, max_nchar = 254)

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Trade-off Analysis (TA) Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 2: Regional economic-Impact of GDP change\\b0\\fs20"
date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1<-"\\b\\fs24 Impact of GDP change to land use requirement \\b0\\fs20"

# ==== Report 0. Cover=====
rtffile <- RTF("LUMENS_TA-3_Final_Demand_report.doc", font.size=11, width = 8.267, height = 11.692, omi = c(0,0,0,0))
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
addParagraph(rtffile, "\\b\\fs20 Table 1. GDP change\\b0\\fs20.")
addTable(rtffile,GDP_table,font.size=6)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 2. Impact of GDP change to land use requirement\\b0\\fs20.")
addTable(rtffile,delta_L2,font.size=7)
done(rtffile)







