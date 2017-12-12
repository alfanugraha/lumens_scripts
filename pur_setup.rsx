##PUR-PostgreSQL=group
##proj.file=string
##ref_data=string
##ref_class=string
##ref_mapping=string
##pu_units=string
##PUR_rec1=output raster 
##PUR_rec1_shp=output vector
##data_attribute=output table
##tabel_acuan=output table
##PUR_dbfinal=output table
##statusoutput=output table

#=Load library
library(foreign)
library(grid)
library(gridExtra)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(rtf)
library(spatial.tools)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
# library(stringr)

#=Load active project
load(proj.file)

# set driver connection
driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

# raising index then create working directory based on index
idx_PUR=idx_PUR+1
resave(idx_PUR, file=proj.file)
wd_user<-paste(dirname(proj.file), "/PUR/", idx_PUR, "_PUR_analysis", sep="")
dir.create(wd_user, mode="0777")
setwd(wd_user)

#=Prepare reference data
list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
data_pu<-list_of_data_pu[which(list_of_data_pu$RST_NAME==ref_data),]
ref<-getRasterFromPG(pgconf, project, data_pu$RST_DATA, paste(data_pu$RST_DATA, '.tif', sep=''))
ref<-reclassify(ref, cbind(255, 0))

lookup_ref<-dbReadTable(DB, c("public", data_pu$LUT_NAME)) 
colnames(lookup_ref)[ncol(lookup_ref)]="REFERENCE"

#=Load and merge data with reference map
tabel_acuan<-read.table(ref_class, header=FALSE, sep=",")
colnames(tabel_acuan)[1]="acuan_kelas"
colnames(tabel_acuan)[2]="acuan_kode"
tabel_mapping<-read.table(ref_mapping, header=FALSE, sep=",") 
colnames(tabel_mapping)[1]="REFERENCE"
colnames(tabel_mapping)[2]="IDS"
tabel_mapping<-merge(tabel_mapping, lookup_ref, by="REFERENCE")
tabel_mapping$COUNT<-NULL
colnames(tabel_mapping)[3]="IDO"

#=Save reference table and map to temporary folder
target_file<-paste(wd_user,"/", data_pu$LUT_NAME, ".csv", sep="")
write.table(tabel_mapping, target_file, quote=FALSE, row.names=FALSE, sep=",")
# write.table(tabel_acuan, "kelas_referensi.csv", quote=FALSE, row.names=FALSE, sep=",")
# wd_usertemp<-paste(wd_user,"/temp", sep="")

#=Prepare reference data
datalist2<-as.data.frame(list.files(path=wd_user, pattern="in_pu", full.names=TRUE))
ref.name<-names(ref) 

#=Prepare planning units
pu_list<-read.table(pu_units, header=FALSE, sep=",")
n_pu_list<-nrow(pu_list)
cmd <- paste()
command1 <- paste()
central_attr<-NULL
for(i in 1:n_pu_list){
  # set planning unit parameter 
  data_name<-as.character(pu_list[i,1])
  pu_data<-as.character(pu_list[i,2])
  Type<-as.character(pu_list[i,4])
  lut_data<-paste("in_pu_lut", substring(pu_data, 6), sep="")
  
  # get planning unit data 
  eval(parse(text=(paste0(lut_data, "<-dbReadTable(DB, c('public', lut_data)) "))))
  eval(parse(text=(paste0(pu_data, "<-getRasterFromPG(pgconf, project, pu_data, paste0(LUMENS_path_user, '/' , pu_data, '.tif'))"))))
  
  central_attr<-append(central_attr, data_name)
  # reclass NA and 255 to zero value
  eval(parse(text=(paste0(pu_data, "[is.na(", pu_data, ")]<-0", sep="")))) 
  eval(parse(text=(paste0(pu_data, "<-reclassify(", pu_data, ", cbind(255, 0))"))))
  eval(parse(text=(paste0("names(", pu_data, ")<-'", data_name, "'"))))
  
  j=n_pu_list+1-i
  eval(parse(text=(paste("R", i, "<-", pu_data, "*(100^(", j, "))", sep=""))))
  cmd<-paste(cmd,"R", i, "+", sep="")
  
  if(i!=n_pu_list){
    command1<-paste(command1, pu_data, ",", sep="")
  } else {
    command1<-paste(command1, pu_data, sep="")
  }
}
ref.number <- n_pu_list+1 
eval(parse(text=(paste("R", ref.number, "<-ref*1", sep=""))))
cmd<-paste(cmd,"R", ref.number, sep="")

#=Combine reference and planning units
# stacking planning unit
command1 <- paste(command1, ",ref", sep="") 
eval(parse(text=(paste("PUR_stack <- stack(", command1, ")", sep="")))) 

# create raster attribute table from combined planning unit
eval(parse(text=(paste("PUR<-", cmd, sep=""))))
PUR <- ratify(PUR, count=TRUE)
PUR_db<-levels(PUR)[[1]]

# reclassify attribute ID
ORI_ID<-PUR_db$ID
NEW_ID<-seq(nrow(PUR_db)) 
rclmat<-cbind(as.matrix(ORI_ID), as.matrix(NEW_ID)) 
PUR<-reclassify(PUR, rclmat) 
PUR<-ratify(PUR, count=TRUE)

# extract all ids
PUR_db$NEW_ID<-NEW_ID
PUR_db$TEMP_ID<-PUR_db[,1]
k<-0
while(k < ref.number) {
  eval(parse(text=(paste("PUR_db$Var", n_pu_list-k, "<-PUR_db$TEMP_ID %% 100", sep=""))))  
  PUR_db$TEMP_ID<-floor(PUR_db$TEMP_ID/100)
  k=k+1
}
PUR_db$TEMP_ID<-NULL

#=Conduct reconciliation
colnames(PUR_db)[1]="unique_id"
colnames(PUR_db)[2]="Freq"
#colnames(PUR_db)[3]="NEW_ID"
colnames(PUR_db)[4]=ref.name
m<-0
for(l in 1:n_pu_list) {
  pu_data<-as.character(pu_list[l,2])
  var_num<-n_pu_list+4-m
  eval(parse(text=(paste("colnames(PUR_db)[",var_num,"]<-names(", pu_data, ")", sep=""))))
  m=m+1
}
colnames(tabel_mapping)[3]<-ref.name
PUR_dbmod<-merge(PUR_db, tabel_mapping, by=ref.name)
for(j in 1:(n_pu_list)) {
  data_name<-as.character(pu_list[j,1])
  eval(parse(text=(paste("PUR_dbmod<-within(PUR_dbmod,{cek", j, "<-as.numeric(", data_name, "==IDS)})",sep=""))))
}

# check planning unit which is overlapped refer to reference data 
#   if there is no overlapping data then attribute equals to reference,
#   else attribute would become unresolved
command4<-paste()
for (p in 1:n_pu_list) {
  if (p!=n_pu_list) {
    eval(parse(text=(paste("command4<-paste(command4,", '"cek', p, '+', '")', sep=""))))
  } else {
    eval(parse(text=(paste("command4<-paste(command4,", '"cek', p, '")', sep=""))))
  }
}
PUR_dbmod<-within(PUR_dbmod, {reconcile1<-eval(parse(text=(command4)))})
PUR_dbmod<-within(PUR_dbmod, {reconcile_attr<-ifelse(reconcile1==0,as.character(REFERENCE), "unresolved")})

# put an ID in overlapped/reconcile attribute 
command5<-paste()
for (r in 1:n_pu_list) {
  if (r!=n_pu_list) {
    eval(parse(text=(paste("command5<-paste(command5, ", '"(cek",', r,',"*",' , r, ', ")+", sep="")', sep="" ))))
  }
  else {
    eval(parse(text=(paste("command5<-paste(command5, ", '"(cek",', r,',"*",' , r, ', ")", sep="")', sep="" ))))
  }
}
PUR_dbmod<-within(PUR_dbmod, {reconcile_attr2<-ifelse(reconcile1==1, reconcile_attr2<-eval(parse(text=(command5))),100)})

# create central attribute of planning units
central_attr<-as.data.frame(central_attr)
numb_ca<-nrow(central_attr)
numb_ca<-as.data.frame(seq(numb_ca))
central_attr<-cbind(numb_ca,central_attr)
central_attrmod<-central_attr
colnames(central_attrmod)[2]="Rec_phase1"
colnames(central_attrmod)[1]="reconcile_attr2"
add5<- c("none")
add6<- c(100)
add_22<- data.frame(add5,add6)
colnames(add_22)[1]="Rec_phase1"
colnames(add_22)[2]="reconcile_attr2"
central_attrmod<-rbind(central_attrmod, add_22)

# change reconcile attribute into a unique one
PUR_dbfinal<-merge(PUR_dbmod,central_attrmod, by='reconcile_attr2')
PUR_dbfinal<-within(PUR_dbfinal, {
  Rec_phase1<-ifelse(Rec_phase1=="none", as.character(reconcile_attr), as.character(Rec_phase1))})
len <- nrow(PUR_dbfinal)
angka = 1
PUR_dbfinal$Rec_phase1b<-PUR_dbfinal$Rec_phase1
for(s in 1:len){
  if(as.character(PUR_dbfinal$Rec_phase1[s])=="unresolved"){
    eval(parse(text=(paste("PUR_dbfinal$Rec_phase1b[", s, "]<-'unresolved_case", angka, "'", sep='')))) 
    angka = angka + 1
  }
}

PUR_dbfinal2<-PUR_dbfinal[,c('NEW_ID','Rec_phase1b')]
colnames(PUR_dbfinal2)[1]= "ID"
test1<-unique(PUR_dbfinal2)[1]
test2<-unique(PUR_dbfinal2)[2]
test3<-cbind(test1,test2)
levels(PUR)<-merge((levels(PUR)),test3,by="ID") 
PUR_rec1<-deratify(PUR,'Rec_phase1b')
gc()
PUR_rec2<-ratify(PUR_rec1, filename=paste0(LUMENS_path_user, '/PUR_rec1.grd'), count=TRUE, overwrite=TRUE) 
levels(PUR_rec1)<-merge((levels(PUR_rec1)),levels(PUR_rec2),by="ID")
#PUR_rec3<-stack(PUR, PUR_rec1)

# write PUR reconciliation phase 1 raster
write.dbf(PUR_dbfinal, "PUR-build_database.dbf")
writeRaster(PUR_rec1, filename="PUR_reconciliation_result", format="GTiff", overwrite=TRUE)
# convert raster to shapefile using gdal polygonize
tif_dir<-paste(wd_user,"/PUR_reconciliation_result.tif", sep="")
file_out<-paste(wd_user, "/PUR_reconciliation_result.shp", sep="")
gdalpolygon<-paste0("\"", LUMENS_path, "\\bin\\gdal_polygonize.py\"")
# gdalpolygon<-str_replace_all(string=gdalpolygon, pattern="\\\\", repl='/')
osgeo_comm<-paste('python', gdalpolygon, tif_dir,'-f "ESRI Shapefile"', file_out, 'PUR_reconciliation_result PU_name', sep=" ")
system(osgeo_comm)
PUR_rec1_shp<-readOGR(".", "PUR_reconciliation_result")

#=Save PUR final database and unresolved case(s) 
database_final<-PUR_dbfinal
database_unresolved<-subset(PUR_dbfinal, Rec_phase1 == "unresolved")
test_unresolve<-nrow(database_unresolved)
database_final<-as.data.frame(levels(PUR_rec1))
data_attribute<-database_final[,c(1,2)]

write.table(data_attribute, "PUR_attribute.csv", quote=FALSE, row.names=FALSE, sep=",")
# write.dbf(data_attribute, "PUR_attribute.dbf")

if (test_unresolve!=0) {
  len <- nrow(database_unresolved)
  for(r in 1:n_pu_list){
    pu_data<-as.character(pu_list[r,2])
    eval(parse(text=(paste("database_unresolved$PU_", r, '<-"NULL"', sep=""))))
    word1<-paste("cek", r, sep="")
    word2<-paste("PU_", r, sep="")
    for(s in 1:len){
      eval(parse(text=(paste("if((database_unresolved$", word1, "[", s, "])>0){database_unresolved$", word2, "[", s, "]<-names(", pu_data, ")} else {database_unresolved$", word2, "[", s, ']<-"-"}', sep=""))))
    }
  }
  
  numberx<-ncol(database_unresolved)
  numbery<-numberx-(n_pu_list)
  database_unresolved_out<-database_unresolved[,c(numbery:numberx)]
  dat1<-as.data.frame(database_unresolved$unique_id)
  dat2<-as.data.frame(database_unresolved$Freq)
  dat3<-as.data.frame(database_unresolved$REFERENCE)
  colnames(dat1)[1]<-"ID"
  colnames(dat2)[1]<-"COUNT"
  colnames(dat3)[1]<-"REFERENCE"
  database_unresolved_out<-cbind(database_unresolved_out, dat3, dat2)
  database_unresolved_out<-merge(data_attribute, database_unresolved_out, by="Rec_phase1b")
  write.table(database_unresolved_out, "PUR_unresolved_case.csv", quote=FALSE, row.names=FALSE, sep=",")
} else {
  database_unresolved_out<-as.data.frame("There are no unresolved area in this analysis session")
  colnames(database_unresolved_out)[1]<-"Reconciliation result"
}

#=Create Map for report
# arrange numerous colors with RColorBrewer
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- brewer.pal(11, "Spectral")
myColors  <-c(myColors1,myColors7, myColors2, myColors3, myColors4, myColors5, myColors6)
rm(myColors1,myColors7, myColors2, myColors3, myColors4, myColors5, myColors6)

#Plot 6 (Peta hasil rekonsiliasi)
PUR.Rec.lab<-database_final
PUR.Rec.lab$COUNT<-NULL
myColors.PUR.Rec <- myColors[1:length(unique(PUR.Rec.lab$ID))]
ColScale.PUR.Rec<-scale_fill_manual(name="Planning Unit",breaks=PUR.Rec.lab$ID, labels=PUR.Rec.lab$Rec_phase1, values = myColors.PUR.Rec )
plot.PUR.Rec  <- gplot(PUR_rec2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  ColScale.PUR.Rec + coord_equal() + ggtitle(paste("Reconciliation Map")) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#barplot(reconciliation phase 1 summary)
area_rec1<-database_final
myColors.RPB <- myColors[1:length(unique(area_rec1$Rec_phase1b))]
names(myColors.RPB) <- unique(area_rec1$Rec_phase1b)
ColScale.RPB<-scale_fill_manual(values = myColors.RPB)
Rec.phs.bar<-ggplot(data=area_rec1, aes(x=Rec_phase1b, y=COUNT, fill=Rec_phase1b)) + geom_bar(stat="identity") +coord_flip() + ColScale.RPB +
  geom_text(data=area_rec1, aes(x=Rec_phase1b, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Unit Perencanaan Fase 1" )) + guides(fill=FALSE) + ylab("Luas (ha)") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

#write report
title<-"\\b\\fs40 LUMENS-PUR Project Report\\b0\\fs20"
sub_title<-"\\b\\fs32 REKONSILIASI UNIT PERENCANAAN MENGGUNAKAN DATA ACUAN\\b0\\fs20"
date<-paste("Date : ", date(), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
rtffile <- RTF("PUR-Build_report.doc", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Rekonsiliasi unit perencanaan adalah proses untuk menyelesaikan tumpang-tindih ijin dengan merujuk pada peta acuan/referensi fungsi. Rekonsiliasi dilakukan dengan menganalisa kesesuaian fungsi antara data-data ijin dengan data referensi. Data ijin yang dimaksud datapat berupa data konsesi pengelolaan hutan, ijin perkebunan, ijin tambang dan lain sebagainya, Sedangkan data referensi yang digunakan dapat berupa data rencana tata ruang atau penunjukan kawasan. ")
addNewLine(rtffile)
addParagraph(rtffile, "\\b \\fs32 DATA YANG DIGUNAKAN \\b0 \\fs20")
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "\\b Data acuan \\b0")
addNewLine(rtffile)
addParagraph(rtffile, "Data acuan adalah data yang digunakan sebagai referensi dalam melakukan pengecekan kesesuaian fungsi peta-peta unit perencanaan dengan fungsi referensi. Peta ini dapat berupa peta acuan penunjukan kawasan atau peta tata ruang. Pada prinsipnya, data referensi adalah data dengan tingkat kepastian hukum tertinggi atau data yang apling dipercaya sebagai acuan fungsi unit perencanaan di sebuah daerah")
addNewLine(rtffile)

#datalist2[1]<-NULL
addTable(rtffile, datalist2)
#datalist[1]<-NULL
addNewLine(rtffile)
addParagraph(rtffile, "\\b Data ijin \\b0")
addNewLine(rtffile)
addParagraph(rtffile, "Data ijin adalah data-data unit perencanaan yang akan digunakan untuk menunjukkan konfigurasi perencanaan penggunaan lahan di sebuah daerah. Data-data dalam bentuk peta ini menggambarkan arahan pengelolaan atau perubahan penggunaan lahan pada sebuah bagian bentang lahan")
addNewLine(rtffile)
addTable(rtffile, pu_list)
addNewLine(rtffile)
addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3.73, res=150, plot(PUR_stack))
addNewLine(rtffile)

addParagraph(rtffile, "\\b \\fs32 HASIL REKONSILIASI \\b0 \\fs20")
addParagraph(rtffile, line)
addParagraph(rtffile, "Pada bagian ini ditunjukkan hasil proses rekonsiliasi dengan menggunakan peta referensi ")
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3.73, res=150, plot.PUR.Rec )
addNewLine(rtffile)
addTable(rtffile, database_final)
addNewLine(rtffile)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3.73, res=150, Rec.phs.bar )
addNewLine(rtffile)

addParagraph(rtffile, "\\b \\fs32 DATA IJIN YANG TIDAK TERREKONSILIASI \\b0 \\fs20")
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Data ijin yang tidak dapat terekonsiliasi akan tercantum pada bagian ini. Rekonsiliasi berbasis acuan fungsi, tidak dapat dilakukan jika ditemukan dua atau lebih unit perencanaan yang memiliki kesuaian fungsi dengan data acuan/referensi. Jika hal ini terjadi maka proses rekonsiliasi harus dilanjutkan melalui diskusi dengan semau pemangku kepentingan yang terkait ")
addNewLine(rtffile)
addTable(rtffile, database_unresolved_out, font.size=7)
addNewLine(rtffile)

done(rtffile)

# command<-paste("start ", "winword ", wd_user, "/LUMENS_PUR_report_reconcile.doc", sep="" )
# shell(command)

#=Writing final status message (code, message)
statuscode<-1
statusmessage<-"PUR reconciliation successfully completed!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
