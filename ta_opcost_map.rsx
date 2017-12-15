##[TA]=group
##npv_lookup=file

library(SDMTools)
library(tiff)
library(foreign)
library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(spatial.tools)
library(rtf)

#READ LUMENS LOG FILE
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS/LUMENS.log", sep="")
LUMENS_temp_user <- paste(user_temp_folder,"/LUMENS/temp", sep="")
dir.create(LUMENS_temp_user, mode="0777")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)

#====READ LANDUSE DATA FROM LUMENS DATABASE====
per<-as.data.frame(ls(pattern="freq"))
n<-nrow(per)
if(n==0){
  msgBox <- tkmessageBox(title = "TA",
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

#if pu is not exist, use p.admin.df as planning unit reference
data2<-as.data.frame(as.character(ls(pattern="pu_pu")))
n_pu<-nrow(data2)
if (n_pu==0) {
  msgBox <- tkmessageBox(title = "TA",
                         message = "No planning unit found. Do you want to use administrative boundary as planning unit?",
                         icon = "question", 
                         type = "yesno", default="yes")
  if(as.character(msgBox)=="no"){
    quit()
  }
  ref[ref==0]<-NA
  lut.pu<-p.admin.df[2]
  lut.pu[2]<-p.admin.df[1]
  pu<-"ref"
} else {
  command3<-NULL
  for(i in 1:n_pu) {
    if (i!=n_pu){
      command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
      command3<-c(command3,command3a)
    } else {
      command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
      command3<-c(command3,command3a)
    }
  }
}

rr<-nrow(per)
command4<-NULL
for(i in 1:rr) {
  if (i!=rr){
    command4<-paste(command4,"freqLanduse_", i, ",", sep="")
  } else {
    command4<-paste(command4,"freqLanduse_", i, sep="")
  }
}
#end create command

#====SELECT DATA TO BE ANALYZED====
eval(parse(text=(paste("year<-c(", command1, ")", sep=""))))
data<-as.data.frame(cbind(data,year))
data$t1<-0
data$t2<-0
colnames(data)[1]<-"data"
data$data<-as.character(data$data)
data3<-data
a<-nrow(data3)
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
    msgBox <- tkmessageBox(title = "Pre-QUES",
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

n<-nrow(data)
command1<-NULL
T1<-data[1,2]
T2<-data[2,2]

#====SELECT PLANNING UNIT TO BE ANALYZED====
if(n_pu!=0){
  data2<-as.data.frame(cbind(data2,command3))
  data2$usage<-0
  colnames(data2)[1]<-"data"
  colnames(data2)[2]<-"sources"
  data2$data<-as.character(data2$data)
  data2$sources<-as.character(data2$sources)
  data2<-rbind(data2, c("ref", "Administrative", 0))
  data2$usage<-as.integer(data2$usage)  
  repeat{
    data2<-edit(data2)
    if(sum(data2$usage)==1){
      break
    } else {
      msgBox <- tkmessageBox(title = "Pre-QUES",
                             message = "Choose one data as a planning unit. Retry?",
                             icon = "question", 
                             type = "retrycancel", default="retry")
      if(as.character(msgBox)=="cancel"){
        quit()
      }
    }
  } 
  data2 <- data2[which(data2$usage==1),]
  data2$usage<-NULL
  pu<-as.character(data2[1,1])
  if(pu=="ref"){
    ref[ref==0]<-NA
    lut.pu<-p.admin.df[2]
    lut.pu[2]<-p.admin.df[1]
    pu_selected<-0
  } else {
    pu_selected<-substr(pu, 6, 7)
    eval(parse(text=(paste("lut.pu<-lut.pu", pu_selected, sep=""))))
  }
}

#====PROJECTION HANDLING====
for(j in 1:n) {
  input <- as.character(data[j,1])
  eval(parse(text=(paste(input,"[",input, "==", raster.nodata, "]<-NA", sep=""))))
  command1<-paste(command1,input, ",", sep="")
}

#====projection handling====
if (grepl("+units=m", as.character(ref@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref)[1]*res(ref)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(ref@crs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else{
  msgBox <- tkmessageBox(title = "QUES",
                         message = "Raster map projection is unknown",
                         icon = "info",
                         type = "ok")
  quit()
}

#====Load Datasets====
setwd(LUMENS_temp_user)
ref_name<-names(ref)
writeRaster(ref, filename="ref.tif", format="GTiff", overwrite=TRUE)
ref<-raster("ref.tif")
names(ref)<-ref_name

eval(parse(text=(paste("landuse1<-", data[1,1], sep=""))))
eval(parse(text=(paste("landuse2<-", data[2,1], sep=""))))
eval(parse(text=(paste("zone<-", data2[1,1],sep=""))))

#====Load Lookup Tables====
lookup_c<- lut.c
lookup_z <- lut.pu
lookup_lc<-lookup_c
colnames(lookup_lc)<-c("ID","LC","CARBON")
colnames(lookup_z)<-c("ID", "Z_NAME")

#====Set Project Properties====
title=location
tab_title<-as.data.frame(title)
period1=data[1,2]
period2=data[2,2]
period=period2-period1
proj_prop<-as.data.frame(title)
proj_prop$period1<-period1
proj_prop$period2<-period2
proj_prop$period <- do.call(paste, c(proj_prop[c("period1", "period2")], sep = " - "))

#====Carbon Accounting Process====
rcl.m.c1<-as.matrix(lookup_c[,1])
rcl.m.c2<-as.matrix(lookup_c[,3])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
carbon1<-reclassify(landuse1, rcl.m)
carbon2<-reclassify(landuse2, rcl.m)
chk_em<-carbon1>carbon2
chk_sq<-carbon1<carbon2
emission<-((carbon1-carbon2)*3.67)*chk_em
sequestration<-((carbon2-carbon1)*3.67)*chk_sq

#===CHECK EXISTING RASTER BRICK OR CROSSTAB====
setwd(LUMENS_temp_user)
command1<-paste(command1,pu, sep="")
eval(parse(text=(paste("pu_name<-names(",pu[1],")", sep=''))))
check_lucdb<-FALSE
eval(parse(text=(paste("check_crosstab<-file.exists('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
eval(parse(text=(paste("check_rbrick<-file.exists('r.brick_", pu_name ,"_", T1, "_", T2, ".grd')", sep=""))))  
if(check_crosstab){
  eval(parse(text=(paste("data_merge<-read.dbf('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
  #eval(parse(text=(paste("lu.db<-read.dbf('lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))
} else if(check_rbrick){
  eval(parse(text=(paste("r.brick<-brick('r.brick_", pu_name ,"_", T1, "_", T2, ".grd')", sep=""))))
  lu.db<-crosstab(r.brick,long=TRUE,useNA=FALSE,progress='-')
  check_lucdb<-TRUE
} else {
  eval(parse(text=(paste("r.brick<-brick(stack(", command1, "), filename='r.brick_",pu_name,"_",T1, "_", T2, "')", sep=""))))
  lu.db<-crosstab(r.brick,long=TRUE,useNA=FALSE,progress='-')
  check_lucdb<-TRUE
}

refStack<-brick(landuse1,landuse2, ref)
refCross<-as.data.frame(crosstab(refStack,long=TRUE,useNA=FALSE,progress='-'))
colnames(refCross)[1] ="ID_LC1"
colnames(refCross)[2] = "ID_LC2"
colnames(refCross)[3] = "ZONE"
colnames(refCross)[4] = "COUNT"
refCross$COUNT<-refCross$COUNT*Spat_res
colnames(lookup_c)[1]="ID_LC1"
colnames(lookup_c)[2]="LC_t1"
colnames(lookup_c)[3]="CARBON_t1"
refDB <- merge(refCross,lookup_c,by="ID_LC1")
colnames(lookup_c)[1]="ID_LC2"
colnames(lookup_c)[2]="LC_t2"
colnames(lookup_c)[3]="CARBON_t2"
refDB <- as.data.frame(merge(refDB,lookup_c,by="ID_LC2"))
colnames(lookup_z)[1]="ZONE"
colnames(lookup_z)[2]="Z_NAME"
refDB <- as.data.frame(merge(refDB,lookup_z,by="ZONE"))
refMelt<-melt(data = refDB, id.vars=c('ZONE'), measure.vars=c('COUNT'))
refArea<-dcast(data = refMelt, formula = ZONE ~ ., fun.aggregate = sum)

if(check_lucdb) {
  colnames(lu.db)[1] ="ID_LC1"
  colnames(lu.db)[2] = "ID_LC2"
  colnames(lu.db)[3] = "ZONE"
  colnames(lu.db)[4] = "COUNT"
  colnames(lookup_c)[1]="ID_LC1"
  colnames(lookup_c)[2]="LC_t1"
  colnames(lookup_c)[3]="CARBON_t1"
  data_merge <- merge(lu.db,lookup_c,by="ID_LC1")
  
  colnames(lookup_c)[1]="ID_LC2"
  colnames(lookup_c)[2]="LC_t2"
  colnames(lookup_c)[3]="CARBON_t2"
  data_merge <- as.data.frame(merge(data_merge,lookup_c,by="ID_LC2"))
  
  colnames(lookup_z)[1]="ZONE"
  colnames(lookup_z)[2]="Z_NAME"
  data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
  
  original_data<-subset(data_merge, select=-c(CARBON_t1, CARBON_t2))
  eval(parse(text=(paste("write.dbf(original_data, 'lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep="")))) 
  rm(lu.db, original_data)
} else {
  carbon_table <- subset(lookup_lc, select=-LC)
  colnames(carbon_table)[1]="ID_LC1"
  colnames(carbon_table)[2]="CARBON_t1"
  data_merge <- merge(data_merge,carbon_table,by="ID_LC1") 
  colnames(carbon_table)[1]="ID_LC2"
  colnames(carbon_table)[2]="CARBON_t2"
  data_merge <- merge(data_merge,carbon_table,by="ID_LC2") 
}


#====CREATE FOLDER AND WORKING DIRECTORY====
TA1.index=TA1.index+1
hist_folder<-paste("OpCostMap_", pu_name,"_", T1,"_",T2,"_",TA1.index,sep="")
working_directory<-paste(dirname(proj.file),"/TA/", sep="")
setwd(working_directory)
dir.create(hist_folder)

working_directory<-paste(working_directory, hist_folder, sep='')
setwd(working_directory)

# load look up tables
lookup_n <- read.table(npv_lookup, header=TRUE, sep=",")

# create raster attribute table usign land cover file look up table
levels(landuse1)<-merge((levels(landuse1)),lookup_n,by="ID")
levels(landuse2)<-merge((levels(landuse2)),lookup_n,by="ID")
levels(zone) <- merge(area_zone,lookup_z,by="ID")
area_lc1<-as.data.frame(levels(landuse1))
area_lc2<-as.data.frame(levels(landuse2))
area_zone<-as.data.frame(levels(zone))

#====NPV Accounting Process====
rcl.m.npv1<-as.matrix(lookup_n[,1])
rcl.m.npv2<-as.matrix(lookup_n[,3])
rcl.m.npv<-cbind(rcl.m.npv1,rcl.m.npv2)
npv1<-reclassify(landuse1, rcl.m.npv)
npv2<-reclassify(landuse2, rcl.m.npv)

npv_chg<-npv2-npv1
opcost<-npv_chg/emission

#export analysis result
carbontiff1<-carbon1
carbontiff2<-carbon2
npvtiff1<-npv1
npvtiff2<-npv2
npvchgtiff<-npv_chg
opcosttiff<-opcost

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Trade-off Analysis (TA) Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 1: Opportunity Cost Map \\b0\\fs20"
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1<-"\\b\\fs24 1.Carbon stock maps \\b0\\fs20"
chapter2<-"\\b\\fs24 2.NPV maps \\b0\\fs20"
chapter3<-"\\b\\fs24 3.Opportunity cost maps \\b0\\fs20"
rtffile <- RTF("LUMENS_TA-1_report.lpr", font.size=9)
if (file.exists("C:/Program Files (x86)/LUMENS")){
  addPng (rtffile, "C:/Program Files (x86)/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
} else{
  addPng (rtffile, "C:/Program Files/LUMENS/lumens_header_report.png", width=6.43, height=0.43)
}
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addNewLine(rtffile)
C1 <- levelplot(carbon1, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C1 )
addParagraph(rtffile, "\\b\\fs20 Figure 1. Carbon density maps t1\\b0\\fs20.")
addNewLine(rtffile)
C2 <- levelplot(carbon2, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C2 )
addParagraph(rtffile, "\\b\\fs20 Figure 2. Carbon density maps t2\\b0\\fs20.")
addNewLine(rtffile)
C3 <- levelplot(emission, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C3 )
addParagraph(rtffile, "\\b\\fs20 Figure 3. Emission maps t1-t2\\b0\\fs20.")
addNewLine(rtffile)
C4 <- levelplot(emission, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C4 )
addParagraph(rtffile, "\\b\\fs20 Figure 4. Emission maps t1-t2\\b0\\fs20.")
addNewLine(rtffile)
C5 <- levelplot(sequestration, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C5 )
addParagraph(rtffile, "\\b\\fs20 Figure 5. Sequestration maps t1-t2\\b0\\fs20.")
addNewLine(rtffile)
C6 <- levelplot(sequestration, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C6 )
addParagraph(rtffile, "\\b\\fs20 Figure 6. Sequestration maps t1-t2\\b0\\fs20.")
addNewLine(rtffile)
addParagraph(rtffile, chapter2)
addNewLine(rtffile)
C7 <- levelplot(npv1, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C7 )
addParagraph(rtffile, "\\b\\fs20 Figure 7. NPV map t1\\b0\\fs20.")
addNewLine(rtffile)
C8<- levelplot(npv2, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C8 )
addParagraph(rtffile, "\\b\\fs20 Figure 8. NPV map t2\\b0\\fs20.")
addNewLine(rtffile)
C9 <- levelplot(npv_chg, col.regions= function(x)rev(terrain.colors(x)))
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C9 )
addParagraph(rtffile, "\\b\\fs20 Figure 9. NPV change map t1-t2\\b0\\fs20.")
addNewLine(rtffile)
addParagraph(rtffile, chapter3)
addNewLine(rtffile)
C10 <-gplot(opcost) + geom_tile(aes(fill = value)) + scale_fill_gradient(low = 'white', high = 'blue') + coord_equal()
addPlot(rtffile, plot.fun=print, width=6, height=5, res=300, C10 )
addParagraph(rtffile, "\\b\\fs20 Figure 10. Opcost map t1-t2\\b0\\fs20.")
addNewLine(rtffile)

done(rtffile)

