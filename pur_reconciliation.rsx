##PUR-PostgreSQL=group
##proj.file=string
##recon_file=vector
##unresolved_table=string
##statusoutput=output table

# proj.file="D:/MW/DATA/LUMENS/TESTING/LUMENS_test_june23/LUMENS_test_june23.lpj"
# recon_file = "D:/MW/DATA/LUMENS/LUMENS_dev/lumens_updates/r/pur/recon_file/PURrec1shp.shp"
# unresolved_table = "D:/MW/DATA/LUMENS/LUMENS_dev/lumens_updates/r/pur/unresolved_table.csv"

#=Load library
library(grid)
library(gridExtra)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(foreign)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(stringr)
library(magick)
library(raster)
library(sf)
library(terra)

#=Load active project
load(proj.file)

# set driver connection
driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

#=Set PUR directory
working_directory<-paste(dirname(proj.file), "/PUR/", idx_PUR, "_PUR_analysis", sep="")
setwd(working_directory)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#=Load reconciliation phase 1 and attribute table
wd_usertemp<-paste(working_directory,"/temp", sep="")
wd_user <- getwd()
sa<- st_read(recon_file)
attribute<-paste(working_directory, "/PUR_attribute.csv", sep="")
# get resolved action and merge with attribute table
attribute<- read.table(attribute, header=TRUE, sep=",")
unresolved_edit<- read.table(unresolved_table, header=TRUE, sep=",")
unresolved_edit.c1<-as.data.frame(unresolved_edit$ID)
unresolved_edit.c2<-as.data.frame(unresolved_edit$Reconcile.Action)
colnames(unresolved_edit.c1)[1]<-"ID"
colnames(unresolved_edit.c2)[1]<-"resolved"
unresolved_edit.join<-cbind(unresolved_edit.c1,unresolved_edit.c2)
attribute.edit<-merge(attribute,unresolved_edit.join, by="ID", all=TRUE)

test<-as.data.frame(unique(unresolved_edit$Reconcile.Action))
test2<-as.data.frame(unique(attribute$Rec_phase1b))
colnames(test)[1]<-"add"
colnames(test2)[1]<-"add"
test3<-rbind(test,test2)
levels(attribute.edit$resolved)<-levels(test3$add)
colnames(attribute.edit)[1]<-"PU_name"

len<-nrow(attribute.edit)
for(s in 1:len){
  if (is.na(attribute.edit$resolved[s])==TRUE) {
    attribute.edit$resolved[s]<-attribute.edit$Rec_phase1b[s]
    attribute.edit$res_id[s]<-attribute.edit$PU_name[s]
  }
}

unique_class<-as.data.frame(unique(attribute.edit$resolved))
colnames(unique_class)[1]<-"resolved"
countrow<-nrow(unique_class)
unique_class$PU_ID<-seq(countrow)
attribute.edit<-merge(attribute.edit, unique_class, by="resolved")
# save PUR final reconciliation shapefile
sa <- merge(sa,attribute.edit, by="PU_name", all=TRUE)
st_write(sa, paste0(working_directory, "/pur_recon_ciliation.shp"), driver="ESRI Shapefile")# and create the raster version

# save PUR final reconciliation raster file
pur_final_recon_rast <- terra::rasterize(vect(sa), rast(ref), field="PU_ID", res=res(ref)[1], background=NA)
pur_final_recon_rast2 <- terra::rasterize(vect(sa), rast(ref), field="PU_name", res=res(ref)[1], background=NA)

writeRaster(pur_final_recon_rast, paste(working_directory,"/", "PUR_final_reconciliation.tif", sep=""))

# create summary of final reconciliation
test4<-raster(pur_final_recon_rast)
test4<-ratify(test4, filename=paste0(LUMENS_path_user, '/PUR.grd'), count=TRUE, overwrite=TRUE)
summary_PUR<-as.data.frame(levels(test4))
colnames(summary_PUR)[1]<-"PU_ID"
summary_PUR<-merge(summary_PUR,unique_class, by="PU_ID")

#=Write results to PostgreSQL
idx_pu<-idx_pu+1
index1<-idx_pu
description<-paste0("PUR Final Reconciliation ", idx_PUR)

raster_temp<-reclassify(test4, cbind(NA, 255)) # need to set as a dynamic variable
raster_temp_name<-paste0(LUMENS_path_user, "/raster_temp.tif")
writeRaster(raster_temp, filename=raster_temp_name, format="GTiff", overwrite=TRUE)

pur_attribute_table<-summary_PUR
colnames(pur_attribute_table)<-c("ID", "COUNT", "Legend")
eval(parse(text=(paste("in_pu_lut", idx_pu, "<-pur_attribute_table",  sep=""))))

eval(parse(text=(paste("list_of_data_pu<-data.frame(RST_DATA='in_pu", idx_pu,"', RST_NAME='", description, "', LUT_NAME='in_pu_", "lut", idx_pu, "', row.names=NULL)", sep=""))))

InPuLUT_i <- paste("in_pu_lut", idx_pu, sep="")
InPu_i <- paste("in_pu", idx_pu, sep="")

#append list
dbWriteTable(DB, "list_of_data_pu", list_of_data_pu, append=TRUE, row.names=FALSE)
dbWriteTable(DB, InPuLUT_i, eval(parse(text=(paste(InPuLUT_i, sep="" )))), append=TRUE, row.names=FALSE)

#write to csv
list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
csv_file<-paste(LUMENS_path_user,"/csv_planning_unit.csv", sep="")
write.table(list_of_data_pu, csv_file, quote=FALSE, row.names=FALSE, sep=",")

addRasterToPG(project, raster_temp_name, InPu_i, srid)

resave(idx_pu, file=proj.file)

# save RDS for report -----------------------------------------------------
dir.create(path = paste0(wd_user, "/report_files"))
saveRDS(pur_final_recon_rast, "report_files/pur_final_recon_rast.rds")
saveRDS(summary_PUR, "report_files/summary_PUR.rds")

# #=Create Map for report
# # arrange numerous colors with RColorBrewer
# myColors1 <- brewer.pal(9,"Set1")
# myColors2 <- brewer.pal(8,"Accent")
# myColors3 <- brewer.pal(12,"Paired")
# myColors4 <- brewer.pal(9, "Pastel1")
# myColors5 <- brewer.pal(8, "Set2")
# myColors6 <- brewer.pal(8, "Dark2")
# myColors7 <- brewer.pal(11, "Spectral")
# myColors  <-c(myColors1,myColors7, myColors2, myColors3, myColors4, myColors5, myColors6)
# rm(myColors1,myColors7, myColors2, myColors3, myColors4, myColors5, myColors6)

#Plot 6 (Peta hasil rekonsiliasi)
PUR.Rec.lab<-unique_class
#PUR.Rec.lab$COUNT<-NULL
myColors.PUR.Rec <- 1:length(unique(PUR.Rec.lab$PU_ID))
ColScale.PUR.Rec<-scale_fill_manual(name="Planning Unit",breaks=PUR.Rec.lab$PU_ID, labels=PUR.Rec.lab$resolved, values = myColors.PUR.Rec )
plot.PUR.Rec  <- gplot(test4, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  ColScale.PUR.Rec + coord_equal() + ggtitle(paste("Final Reconciliation Map")) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#barplot(reconciliation phase 1 summary)
area_rec1<-summary_PUR
myColors.RPB <- 1:length(unique(area_rec1$resolved))
names(myColors.RPB) <- unique(area_rec1$resolved)
ColScale.RPB<-scale_fill_manual(values = myColors.RPB)
Rec.phs.bar<-ggplot(data=area_rec1, aes(x=resolved, y=COUNT, fill=resolved)) + geom_bar(stat="identity") +coord_flip() + ColScale.RPB +
  geom_text(data=area_rec1, aes(x=resolved, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Hasil akhir rekonsiliasi" )) + guides(fill=FALSE) + ylab("Luas (ha)") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

#=Writing final status message (code, message)
statuscode<-1
statusmessage<-"PUR final reconciliation successfully completed!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
