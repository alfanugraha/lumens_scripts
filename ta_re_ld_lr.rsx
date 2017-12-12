#Regional Economy Land Distribution and Requirement Analysis
##TA-PostgreSQL=group
##int_con_file=string
##add_val_file=string
##fin_dem_file=string
##add_val_struc_file=string
##fin_dem_struc_file=string
##sector_file=string
##land.distribution_file=string
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
work_dir<-paste(log.file[1,1], "/", log.file[1,2],"/TA/LandRequirement", sep="")
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
data$land_analysis<-0
colnames(data)[1]<-"data"
data$data<-as.character(data$data)
repeat{
  data<-edit(data)
  if(sum(data$land_analysis)==1){
      break  
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

data2<-data[which(data$land_analysis==1),]
data2$land_analysis<-NULL

eval(parse(text=(paste("lu_name <- names(", data[1,1], ")", sep=""))))
eval(parse(text=(paste("landuse <- ", data[1,1], sep=""))))

#WRITING TA PROJECT FILE
filename<-paste("TA_projec_", eval(parse(text=(paste("Sys.Date ()")))), ".lms", sep="")
date<-Sys.Date()
sink(filename)
cat("LUMENS TA Module Project File")
cat("\n")
cat(as.character(date))
cat("\n")
cat(lu_name)
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
land.distribution<-read.table(land.distribution_file, header=FALSE, sep=",")
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
GDP[is.na(GDP)]<-0
order_GDP <- as.data.frame(GDP[order(-GDP$GDP),])
order_GDP10<-head(order_GDP,n=20)
GDP_tot<-as.matrix(GDP$GDP)
GDP_tot<-colSums(GDP_tot)

#LINK LAND SISTRIBUTION FILE WITH LAND USE MAP
#Read land use map and calculate area of land use distribution matrix
landuse<-ratify(landuse, filename='landuse.grd',count=TRUE,overwrite=TRUE)
landuse_area<-as.data.frame(levels(landuse))
landuse_area<-subset(landuse_area,ID !=nodata_val)
landuse_area<-as.matrix(landuse_area$COUNT)
land.distribution_t<-as.matrix(land.distribution)
landuse_area_diag<-diag(as.numeric(as.matrix(landuse_area)))
land.distribution.val<-land.distribution_t %*% landuse_area_diag

#CALCULATE LAND DISTRIBUTION COEFFICIENT MATRIX
land.distribution.ctot<-colSums(land.distribution.val)
land.distribution.rtot<-rowSums(land.distribution.val)
land.distribution.prop<-land.distribution.val %*% diag(1/land.distribution.ctot)
land.distribution.prop[is.na(land.distribution.prop)]<-0
land.distribution.prop.r<-t(land.distribution.val) %*% diag(1/land.distribution.rtot)
land.distribution.prop.r[is.na(land.distribution.prop.r)]<-0
land.requirement<-rowSums(land.distribution.val)
fin_dem.rtot<-rowSums(fin_dem)
int_con.rtot<-rowSums(int_con)
demand<-fin_dem.rtot+int_con.rtot
land.requirement.coeff<-land.requirement/demand
land.requirement.coeff[is.infinite(land.requirement.coeff)]<-0
land.productivity.coeff<-land.requirement/fin_dem.rtot
land.productivity.coeff[is.infinite(land.productivity.coeff)]<-0

#PRODUCE OUTPUT
land.requirement_table<-as.data.frame(land.requirement)
land.requirement.tot<-sum(land.requirement)
land.requirement_table_prop<-as.data.frame(land.requirement/land.requirement.tot)
land.requirement_table<-cbind(sector,land.requirement_table,land.requirement_table_prop,demand,fin_dem.rtot,land.requirement.coeff, land.productivity.coeff)
colnames(land.requirement_table)[1] <- "SECTOR"
colnames(land.requirement_table)[2] <- "CATEGORY"
colnames(land.requirement_table)[3] <- "LR"
colnames(land.requirement_table)[4] <- "LR_PROP"
colnames(land.requirement_table)[5] <- "OUTPUT"
colnames(land.requirement_table)[6] <- "DEMAND"
colnames(land.requirement_table)[7] <- "LRC"
colnames(land.requirement_table)[8] <- "LPC"
land.requirement_table$LR<-round(land.requirement_table$LR)
land.requirement_table$LR_PROP<-round(land.requirement_table$LR_PROP, digits=2)
land.requirement_table$OUTPUT<-round(land.requirement_table$OUTPUT)
land.requirement_table$DEMAND<-round(land.requirement_table$DEMAND)
land.requirement_table$LRC<-round(land.requirement_table$LRC, digits=2)
land.requirement_table$LPC<-round(land.requirement_table$LPC, digits=2)

#land.requirement_table$LRC<-round(land.requirement_table$LRC, digits=2)-->catasthropic error !!!!
#land.requirement_table$LPC<-round(land.requirement_table$LPC, digits=2)-->catasthropic error !!!!
order_land.requirement <- as.data.frame(land.requirement_table[order(-land.requirement_table$LRC),])
order_land.requirement <-head(order_land.requirement,n=20)
LRC_graph<-ggplot(data=order_land.requirement, aes(x=SECTOR, y=LRC, fill=CATEGORY)) +
  geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Land requirement coefficient") +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 6),
         axis.text.x = element_text(size = 8))

#EXPORT OUTPUT
land_distribution_file<-"Land_distribution_matrix.dbf"
write.dbf(land.distribution.prop, land_distribution_file,  factor2char = TRUE, max_nchar = 254)
land_requirement_file<-"Land_requirement_coefficient.dbf"
write.dbf(land.requirement_table, land_requirement_file, factor2char = TRUE, max_nchar = 254)

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Trade-off Analysis (TA) Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 2: Regional economic-Land Requirement Coefficient\\b0\\fs20"
date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1<-"\\b\\fs24 Land Requirement Coefficient \\b0\\fs20"

# ==== Report 0. Cover=====
rtffile <- RTF("LUMENS_TA-2_Land_Requirement_report.doc", font.size=11, width = 8.267, height = 11.692, omi = c(0,0,0,0))
# INPUT
img_location <- "C:/LUMENS_modified_scripts/Report/Slide3.PNG"
# loading the .png image to be edited
cover <- image_read(img_location)
# to display, only requires to execute the variable name, e.g.: "> cover"
# adding text at the desired location
text_submodule <- paste("Sub-Modul Ekonomi Regional\n\nAnalisis Kebutuhan dan Produktifitas Lahan\n", location, ", ", "Tahun ", I_O_period, sep="")
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
addParagraph(rtffile, "\\b\\fs20 Table 1. Land requirement\\b0\\fs20.")
addTable(rtffile,land.requirement_table,font.size=7.5)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300,LRC_graph)
done(rtffile)
