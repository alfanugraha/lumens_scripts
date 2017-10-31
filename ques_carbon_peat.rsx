##QUES-PostgreSQL=group
proj.file="C:/LUMENS_new/Lumens_02/Lumens_02.lpj"
landuse_1="lu00_48s_100m"
landuse_2="lu05_48s_100m"
planning_unit="pu_IDH_48s_100m"
peatmap="peat_48s_30mF"
# lookup_z="C:/LUMENS_new/Data/3_Tabular/pu_IDH_lut.csv"
# lookup_lc="C:/LUMENS_new/Data/3_Tabular/landuse_lut.csv"
lookup_lut="lut_cstock"
lookup_c_peat="C:/LUMENS_new/Data/3_Tabular/factor_emisi_peat.csv"
# QUESC_database="C:/LUMENS_new/Lumens_02/QUES/QUES-C/1_QUESC___/QUES-C_database.dbf"
raster.nodata=0
##statusoutput=output table

library(tiff)
library(foreign)
library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(grid)
library(ggplot2)
library(spatial.tools)
library(rtf)
library(jsonlite)
library(splitstackshape)
library(stringr)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(tcltk)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#=Load active project
load(proj.file)

lut.peat<-read.table(lookup_c_peat, sep=",", header=TRUE)

# set driver connection
driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

#=Retrieve all list of data that are going to be used
list_of_data_luc<-dbReadTable(DB, c("public", "list_of_data_luc"))
list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
list_of_data_lut<-dbReadTable(DB, c("public", "list_of_data_lut"))
list_of_data_peat<-dbReadTable(DB, c("public", "list_of_data_peat"))

# return the selected data from the list
data_luc1<-list_of_data_luc[which(list_of_data_luc$RST_NAME==landuse_1),]
data_luc2<-list_of_data_luc[which(list_of_data_luc$RST_NAME==landuse_2),]
data_pu<-list_of_data_pu[which(list_of_data_pu$RST_NAME==planning_unit),]
data_lut<-list_of_data_lut[which(list_of_data_lut$TBL_NAME==lookup_lut),]
data_peat<-list_of_data_peat[which(list_of_data_peat$RST_NAME==peatmap),]

T1<-data_luc1$PERIOD
T2<-data_luc2$PERIOD

#=Set Working Directory
pu_name<-data_pu$RST_NAME
peat_name<-data_peat$RST_NAME
idx_QUESC_peat<-1
#idx_QUESC_peat<-idx_QUESC_peat+1
dirQUESC<-paste(dirname(proj.file), "/QUES/QUES-C/", idx_peat, "_QUESC_Peat_", T1, "_", T2, "_", pu_name, sep="")
dir.create(dirQUESC, mode="0777")

# create temp directory
dir.create(LUMENS_path_user, mode="0777")
setwd(LUMENS_path_user)

#=Set initial variables
# reference map
ref.obj<-exists('ref')
ref.path<-paste(dirname(proj.file), '/ref.tif', sep='')
if(!ref.obj){
  if(file.exists(ref.path)){
    ref<-raster(ref.path)
  } else {
    ref<-getRasterFromPG(pgconf, project, 'ref_map', 'ref.tif')
  }
}
# planning unit
if (data_pu$RST_DATA=="ref") {
  zone<-ref
  lookup_z<-dbReadTable(DB, c("public", data_pu$LUT_NAME)) 
} else {
  zone<-getRasterFromPG(pgconf, project, data_pu$RST_DATA, paste(data_pu$RST_DATA, '.tif', sep=''))
  lookup_z<-dbReadTable(DB, c("public", data_pu$LUT_NAME)) 
}
# peat area
if (data_peat$RST_DATA=="ref") {
  zone<-ref
  lookup_c_peat<-dbReadTable(DB, c("public", data_peat$LUT_NAME)) 
} else {
  peat<-getRasterFromPG(pgconf, project, data_peat$RST_DATA, paste(data_peat$RST_DATA, '.tif', sep=''))
  lookup_c_peat<-dbReadTable(DB, c("public", data_peat$LUT_NAME)) 
}

# landuse first time period
landuse1<-getRasterFromPG(pgconf, project, data_luc1$RST_DATA, paste(data_luc1$RST_DATA, '.tif', sep=''))
# landuse second time period
landuse2<-getRasterFromPG(pgconf, project, data_luc2$RST_DATA, paste(data_luc2$RST_DATA, '.tif', sep=''))
# landcover lookup table
lookup_c<-dbReadTable(DB, c("public", data_lut$TBL_DATA)) 
# set lookup table
lookup_c<-lookup_c[which(lookup_c[1] != raster.nodata),]
lookup_lc<-lookup_c
lookup_ref<-lut_ref
lookup_c.peat<-lut.peat[which(lut.peat[1] != raster.nodata),]
colnames(lookup_lc)<-c("ID","LC","CARBON")
colnames(lookup_z)<-c("ID", "COUNT_ZONE", "ZONE")
colnames(lookup_ref)<-c("REF", "REF_NAME")

#=Projection handling
if (grepl("+units=m", as.character(ref@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref)[1]*res(ref)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C peat will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(ref@crs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C peat will automatically generate data in Ha unit")
} else{
  msgBox <- tkmessageBox(title = "QUES",
                         message = "Raster map projection is unknown",
                         icon = "info",
                         type = "ok")
  quit()
}

#=Check peat data
check.peat<-as.integer(exists("idx_peat"))
check.peat<-as.data.frame(as.character(ls(pattern="idx_peat")))
if(nrow(check.peat)!=0){
  peatmap<-peat*100
  zone_peat_map<-zone+peatmap

  legend_zone_peat<-as.data.frame(freq(zone_peat_map))
  colnames(legend_zone_peat)[1]="ID"
  legend_zone_peat<-merge(lookup_z, legend_zone_peat, by="ID", all=T)
  colnames(legend_zone_peat)[3]="Z_NAME_PEAT"
  legend_zone_peat$COUNT_ZONE<-NULL
  legend_zone_peat<-legend_zone_peat[which(legend_zone_peat$count != "NA"),]
  legend_zone_peat<-legend_zone_peat[which(legend_zone_peat[1] != raster.nodata),]
  n_legend_zone_peat<-nrow(legend_zone_peat)

  #fill Z_NAME_peat with peat
  legend_zone_peat$Z_NAME_PEAT<-as.character(legend_zone_peat$Z_NAME_PEAT)
  for(i in 1:(n_legend_zone_peat)){
    if(legend_zone_peat[i,1] > 100){
      id<-legend_zone_peat[i,1]-100
      temp<-lookup_z[which(lookup_z$ID == id),]
      legend_zone_peat[i,2]<-paste(as.character(temp[1,3]), "_Gambut", sep="")
    }
  }
  zone<-zone_peat_map
  lut.pu_peat<-legend_zone_peat
  lut.pu_peat$ck_peat<-ifelse(lut.pu_peat$ID >= 100, "TRUE", "FALSE")
}

#=Set project properties
title=location
tab_title<-as.data.frame(title)
period1=T1
period2=T2
period=period2-period1
proj_prop<-as.data.frame(title)
proj_prop$period1<-period1
proj_prop$period2<-period2
proj_prop$period <- do.call(paste, c(proj_prop[c("period1", "period2")], sep = " - "))

nLandCoverId<-nrow(lookup_lc)
nPlanningUnitId<-nrow(lookup_z)
nRefId<-nrow(lookup_ref)
nEmFacPeatId<-nrow(lut.pu_peat)

#=Create land use change data dummy
#=Create cross-tabulation for reference
dummy1<-data.frame(nPU=lookup_ref$REF, divider=nLandCoverId*nLandCoverId)
dummy1<-expandRows(dummy1, 'divider')

dummy2<-data.frame(nT1=lookup_c.peat$ID, divider=nLandCoverId)
dummy2<-expandRows(dummy2, 'divider')
dummy2<-data.frame(nT1=rep(dummy2$nT1, nRefId))

dummy3<-data.frame(nT2=rep(rep(lookup_lc$ID, nLandCoverId), nRefId))

landUseChangeRefDummy<-cbind(dummy1, dummy2, dummy3)
colnames(landUseChangeRefDummy)<-c('REF', 'ID_LC1', 'ID_LC2')

R1<-(ref*1) + (landuse1*100^1) + (landuse2*100^2) 
ref.db<-as.data.frame(freq(R1))
ref.db<-na.omit(ref.db)
n<-3
k<-0
ref.db$value_temp<-ref.db$value
while(k < n) {
  eval(parse(text=(paste("ref.db$Var", n-k, "<-ref.db$value_temp %% 100", sep=""))))  
  ref.db$value_temp<-floor(ref.db$value_temp/100)
  k=k+1
}
ref.db$value_temp<-NULL
colnames(ref.db) = c("ID_CHG", "COUNT", "REF", "ID_LC1", "ID_LC2")
ref.db<-merge(landUseChangeRefDummy, ref.db, by=c('REF', 'ID_LC1', 'ID_LC2'), all=TRUE)
ref.db$ID_CHG<-ref.db$REF*1 + ref.db$ID_LC1*100^1 + ref.db$ID_LC2*100^2
ref.db<-replace(ref.db, is.na(ref.db), 0)

#=Create cross-tabulation for peat zone
xtab<-tolower(paste('xtab_', peat_name, "_", T1, "_", T2, sep=''))
data_xtab<-list_of_data_lut[which(list_of_data_lut$TBL_NAME==xtab),]
if(nrow(data_xtab)==0){
  dummy1<-data.frame(nPU=lut.pu_peat$ID, divider=nLandCoverId*nLandCoverId)
  dummy1<-expandRows(dummy1, 'divider')
  
  dummy2<-data.frame(nT1=lookup_c.peat$ID, divider=nLandCoverId)
  dummy2<-expandRows(dummy2, 'divider')
  dummy2<-data.frame(nT1=rep(dummy2$nT1, nEmFacPeatId))
  
  dummy3<-data.frame(nT2=rep(rep(lookup_c.peat$ID, nLandCoverId), nEmFacPeatId))
  
  landUseChangeMapDummy<-cbind(dummy1, dummy2, dummy3)
  colnames(landUseChangeMapDummy)<-c('ZONE_PEAT', 'ID_LC1', 'ID_LC2')
  
  R2<-(zone*1) + (landuse1*100^1) + (landuse2*100^2) 
  lu.db<-as.data.frame(freq(R2))
  lu.db<-na.omit(lu.db)
  n<-3
  k<-0
  lu.db$value_temp<-lu.db$value
  while(k < n) {
    eval(parse(text=(paste("lu.db$Var", n-k, "<-lu.db$value_temp %% 100", sep=""))))  
    lu.db$value_temp<-floor(lu.db$value_temp/100)
    k=k+1
  }
  lu.db$value_temp<-NULL
  colnames(lu.db) = c("ID_CHG", "COUNT", "ZONE_PEAT", "ID_LC1", "ID_LC2")
  lu.db<-merge(landUseChangeMapDummy, lu.db, by=c('ZONE_PEAT', 'ID_LC1', 'ID_LC2'), all=TRUE)
  lu.db$ID_CHG<-lu.db$ZONE*1 + lu.db$ID_LC1*100^1 + lu.db$ID_LC2*100^2
  lu.db<-replace(lu.db, is.na(lu.db), 0)
  
  idx_lut<-idx_lut+1
  eval(parse(text=(paste("in_lut", idx_lut, " <- lu.db", sep=""))))
  
  eval(parse(text=(paste("list_of_data_lut<-data.frame(TBL_DATA='in_lut", idx_lut,"', TBL_NAME='", xtab, "', row.names=NULL)", sep=""))))
  # save to PostgreSQL
  InLUT_i <- paste('in_lut', idx_lut, sep="")
  dbWriteTable(DB, InLUT_i, eval(parse(text=(paste(InLUT_i, sep="" )))), append=TRUE, row.names=FALSE)
  dbWriteTable(DB, "list_of_data_lut", list_of_data_lut, append=TRUE, row.names=FALSE)
  
  setwd(dirQUESC)
  idx_factor<-idx_factor+1
  chg_map<-tolower(paste('chgmap_', peat_name, "_", T1, "_", T2, sep=''))
  eval(parse(text=(paste("writeRaster(R2, filename='", chg_map, ".tif', format='GTiff', overwrite=TRUE)", sep=""))))
  eval(parse(text=(paste("factor", idx_factor, "<-'", chg_map, "'", sep=''))))  
  eval(parse(text=(paste("list_of_data_f<-data.frame(RST_DATA='factor", idx_factor,"', RST_NAME='", chg_map, "', row.names=NULL)", sep=""))))  
  InFactor_i <- paste("factor", idx_factor, sep="")  
  dbWriteTable(DB, "list_of_data_f", list_of_data_f, append=TRUE, row.names=FALSE)
  #write to csv
  list_of_data_f<-dbReadTable(DB, c("public", "list_of_data_f"))
  csv_file<-paste(dirname(proj.file),"/csv_factor_data.csv", sep="")
  write.table(list_of_data_f, csv_file, quote=FALSE, row.names=FALSE, sep=",")  
  addRasterToPG(project, paste0(chg_map, '.tif'), InFactor_i, srid)
} else {
  lu.db<-dbReadTable(DB, c("public", data_xtab$TBL_DATA))
}
# rename column
colnames(lookup_c.peat) = c("ID_LC1", "LC_t1", "FacEm_t1")
data_merge <- merge(lu.db,lookup_c.peat,by="ID_LC1")
colnames(lookup_c.peat) = c("ID_LC2", "LC_t2", "FacEm_t2")
data_merge <- as.data.frame(merge(data_merge,lookup_c.peat,by="ID_LC2"))
lookup_z_peat<-lut.pu_peat
colnames(lookup_z_peat)[3]="COUNT_ZONE"
colnames(lookup_z_peat)[1]="ZONE_PEAT"
colnames(lookup_z_peat)[2]="Z_NAME_PEAT"
data_merge <- as.data.frame(merge(data_merge,lookup_z_peat,by="ZONE_PEAT"))
data_merge$COUNT<-data_merge$COUNT*Spat_res
data_merge$COUNT_ZONE<-data_merge$COUNT_ZONE*Spat_res
refMelt<-melt(data = ref.db, id.vars=c('REF'), measure.vars=c('COUNT'))
refArea<-dcast(data = refMelt, formula = REF ~ ., fun.aggregate = sum)

#=Carbon accounting process
NAvalue(landuse1)<-raster.nodata
NAvalue(landuse2)<-raster.nodata
rcl.m.c1<-as.matrix(lookup_c.peat[,1])
rcl.m.c2<-as.matrix(lookup_c.peat[,3])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
rcl.m<-rbind(rcl.m, c(0, NA))
FacEm1<-reclassify(landuse1, rcl.m)
FacEm2<-reclassify(landuse2, rcl.m)
emission<-((FacEm1+FacEm2)/2)

#=Modify factor emission density for each time series
data_merge$em<-((data_merge$FacEm_t1+data_merge$FacEm_t2)/2)*data_merge$COUNT
data_merge$LU_CHG <- do.call(paste, c(data_merge[c("LC_t1", "LC_t2")], sep = " to "))
data_merge$null<-0
data_merge$nullCek<-data_merge$em

#Write QUESC_peat database
csv_file<-paste(dirname(proj.file),"/QUES/QUES-C/QUESC_peat_database.csv", sep="")
write.table(data_merge, csv_file, quote=FALSE, row.names=FALSE, sep=",")

#=Generate area_zone lookup and calculate min area
area_zone<-melt(data = data_merge, id.vars=c('ZONE_PEAT'), measure.vars=c('COUNT'))
area_zone<-dcast(data = area_zone, formula = ZONE_PEAT ~ ., fun.aggregate = sum)
colnames(area_zone)[1]<-"ID"
colnames(area_zone)[2]<-"COUNT"
area_zone$ID<-as.numeric(as.character(area_zone$ID))
area_zone<-area_zone[with(area_zone, order(ID)),]
colnames(lookup_z_peat)[1]<-"ID"
area_zone<-merge(area_zone, lookup_z_peat, by="ID")
area<-min(sum(area_zone$COUNT), sum(data_merge$COUNT))

#=Generate administrative unit
colnames(refArea)[1]<-"ID"
colnames(refArea)[2]<-"COUNT"
colnames(lookup_ref)[1]<-"ID"
colnames(lookup_ref)[2]<-"KABKOT"
area_admin<-merge(refArea, lookup_ref, by="ID")

#=Create QUES-C database
#=Zonal statistics database
lg<-length(unique(data_merge$ZONE_PEAT))
zone_lookup<-area_zone
data_zone<-area_zone
data_zone$Z_CODE<-toupper(abbreviate(data_zone$Z_NAME_PEAT))
data_zone$Rate_seq<-data_zone$Rate_em<-data_zone$Avg_FeacEm_t2<-data_zone$Avg_FeacEm_t1<-0
for(a in 1:lg){
  i<-unique(data_merge$ZONE_PEAT)[a]
  data_z<-data_merge[which(data_merge$ZONE_PEAT == i),]
  data_zone<-within(data_zone, {Avg_FeacEm_t1<-ifelse(data_zone$ID == i, sum(data_z$FacEm_t1*data_z$COUNT)/sum(data_z$COUNT),Avg_FeacEm_t1)}) 
  data_zone<-within(data_zone, {Avg_FeacEm_t2<-ifelse(data_zone$ID == i, sum(data_z$FacEm_t2*data_z$COUNT)/sum(data_z$COUNT),Avg_FeacEm_t2)}) 
  data_zone<-within(data_zone, {Rate_em<-ifelse(data_zone$ID == i, sum(data_z$em)/(sum(data_z$COUNT)*period),Rate_em)}) 
}
data_zone$COUNT_ZONE<-NULL
data_zone[,6:8]<-round(data_zone[,6:8],digits=2)

#=Emission
# calculate largest source of emission
data_merge_sel <- data_merge[ which(data_merge$nullCek > data_merge$null),]
order_em <- as.data.frame(data_merge[order(-data_merge$em),])
# total emission
tb_em_total<-as.data.frame(cbind(order_em$LU_CHG, as.data.frame(round(order_em$em, digits=3))))
colnames(tb_em_total)<-c("LU_CHG", "em")
tb_em_total<-aggregate(em~LU_CHG,data=tb_em_total,FUN=sum)
tb_em_total$LU_CODE<-as.factor(toupper(abbreviate(tb_em_total$LU_CHG, minlength=5, strict=FALSE, method="both")))
tb_em_total<-tb_em_total[order(-tb_em_total$em),]
tb_em_total<-tb_em_total[c(3,1,2)]
tb_em_total$Percentage<-as.numeric(format(round((tb_em_total$em / sum(tb_em_total$em) * 100),2), nsmall=2))
tb_em_total_10<-head(tb_em_total,n=10)
# zonal emission
tb_em_zonal<-as.data.frame(NULL)
for (i in 1:length(zone_lookup$ID)){
  tryCatch({
    a<-(zone_lookup$ID)[i]
    tb_em<-as.data.frame(cbind(order_em$ZONE, order_em$LU_CHG, as.data.frame(round(order_em$em, digits=3))))
    colnames(tb_em)<-c("ZONE","LU_CHG", "em")
    tb_em_z<-as.data.frame(tb_em[which(tb_em$ZONE == a),])
    tb_em_z<-aggregate(em~ZONE+LU_CHG,data=tb_em_z,FUN=sum)
    tb_em_z$LU_CODE<-as.factor(toupper(abbreviate(tb_em_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
    tb_em_z<-tb_em_z[order(-tb_em_z$em),]
    tb_em_z<-tb_em_z[c(1,4,2,3)]
    tb_em_z$Percentage<-as.numeric(format(round((tb_em_z$em / sum(tb_em_z$em) * 100),2), nsmall=2))
    tb_em_z_10<-head(tb_em_z,n=10)
    tb_em_zonal<-rbind(tb_em_zonal,tb_em_z_10)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# rm(tb_em, tb_em_total, tb_em_z, tb_em_z_10)

#=Zonal additional statistics
if (((length(unique(data_merge$ID_LC1)))>(length(unique(data_merge$ID_LC2))))){
  dimention<-length(unique(data_merge$ID_LC1))
  name.matrix<-cbind(as.data.frame(data_merge$ID_LC1), as.data.frame(data_merge$LC_t1))
  name.matrix<-unique(name.matrix)
  colnames(name.matrix)<-c("ID","LC")
  name.matrix<-name.matrix[order(name.matrix$ID),]
  name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$LC, minlength=4, method="both"))
} else{
  dimention<-length(unique(data_merge$ID_LC2))
  name.matrix<-cbind(as.data.frame(data_merge$ID_LC2), as.data.frame(data_merge$LC_t2))
  name.matrix<-unique(name.matrix)
  colnames(name.matrix)<-c("ID","LC")
  name.matrix<-name.matrix[order(name.matrix$ID),]
  name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$LC, minlength=4, method="both"))
}

#=Transition matrix
# zonal emission matrix
e.m.z<-matrix(0, nrow=dimention, ncol=dimention)
em.matrix.zonal<-as.data.frame(NULL)
for (k in 1:length(zone_lookup$ID)){
  for (i in 1:nrow(e.m.z)){
    for (j in 1:ncol(e.m.z)){
      em.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j & data_merge_sel$ZONE==k),]
      e.m.z[i,j]<-as.numeric(round(sum(em.data$em), 2))
    }
  }
  e.m.z<-as.data.frame(e.m.z)
  e.m.z.c<-as.data.frame(cbind(name.matrix$LC_CODE,e.m.z))
  e.m.z.c<-cbind(rep(k,nrow(e.m.z)),e.m.z.c)
  em.matrix.zonal<-rbind(em.matrix.zonal,e.m.z.c)
}
colnames(em.matrix.zonal)<-c("ZONE","LC_CODE",as.vector(name.matrix$LC_CODE))
# rm(em.data, e.m.z, e.m.z.c)
# total emission matrix
e.m<-matrix(0, nrow=dimention, ncol=dimention)
for (i in 1:nrow(e.m)){
  for (j in 1:ncol(e.m)){
    em.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j),]
    e.m[i,j]<-round(sum(em.data$em), digits=2)
  }
}
e.m<-as.data.frame(e.m)
em.matrix.total<-as.data.frame(cbind(name.matrix$LC_CODE,e.m))
colnames(em.matrix.total)<-c("LC_CODE",as.vector(name.matrix$LC_CODE))
# rm(em.data, e.m)

#=Save database
idx_lut<-idx_lut+1
eval(parse(text=(paste("in_lut", idx_lut, " <- data_merge", sep=""))))

eval(parse(text=(paste("list_of_data_lut<-data.frame(TBL_DATA='in_lut", idx_lut,"', TBL_NAME='out_hist_quesc_", tolower(pu_name), T1, T2, "', row.names=NULL)", sep=""))))
# save to PostgreSQL
InLUT_i <- paste('in_lut', idx_lut, sep="")
dbWriteTable(DB, InLUT_i, eval(parse(text=(paste(InLUT_i, sep="" )))), append=TRUE, row.names=FALSE)
dbWriteTable(DB, "list_of_data_lut", list_of_data_lut, append=TRUE, row.names=FALSE)

# #=Rearrange zone carbon
# zone_carbon_pub<-zone_carbon
# colnames(zone_carbon_pub) <- c("ID", "Luas (Ha)", "Tutupan lahan", "Total emisi (Ton CO2-eq)", "Total sekuestrasi(Ton CO2-eq)", "Emisi bersih (Ton CO2-eq)", "Laju emisi (Ton CO2/Ha.yr)")
# admin_carbon_pub<-admin_carbon
# colnames(admin_carbon_pub) <- c("ID", "Luas (Ha)", "Wil. Administratif", "Total emisi (Ton CO2-eq)", "Total sekuestrasi(Ton CO2-eq)", "Emisi bersih (Ton CO2-eq)", "Laju emisi (Ton CO2/Ha.yr)")
# data_zone_pub<-data_zone
# data_zone_pub$Z_CODE<-NULL
# colnames(data_zone_pub) <- c("ID", "Luas (Ha)", "Unit Perencanaan", "Rerata Karbon Periode 1", "Rerata Karbon Periode 2", "Emisi bersih", "Laju emisi")

#READ PEAT DATA
check.peat<-as.integer(exists("idx_peat"))
if (check.peat==0) {
    tkmessageBox(title = "LUMENS process halted", message = "Peat area is not defines. Please add peat map into active database", icon = "error", type = "ok")
} else 
  # SELECTING AVAILABLE QUES-C ANALYSIS
  QUESC_peat_list<-as.data.frame(ls(pattern="QUESC_peat_database"))
  colnames (QUESC_peat_list) [1]<-"Data"
  QUESC_peat_list$Usage<-0
  repeat{
    QUESC_peat_list<-edit(QUESC_peat_list)
    if(sum(QUESC_peat_list$Usage)==1){
      break
    }
  }
  QUESC_peat_list <- QUESC_peat_list[which(QUESC_peat_list$Usage==1),]
  QUESC_peat_list$Usage<-NULL
  sel.QUESC<-as.character(QUESC_peat_list[1,1])
  peat_t1<-as.integer(substr(sel.QUESC, 16:19, 19))
  peat_t2<-as.integer(substr(sel.QUESC, 21:24, 25))
  text1<-paste("QUESC_Peat_database_", as.character(peat_t1),"-", as.character(peat_t2), sep="")
  eval(parse(text=(paste("QUESC_Peat_database_", as.character(peat_t1),"_", as.character(peat_t2), "<-", sel.QUESC, sep=""))))
  eval(parse(text=(paste("cross<-r.brick_", as.character(peat_t1),"_", as.character(peat_t2), sep=""))))
  cross<-stack(cross,Peat_1)
  cross<-as.data.frame(crosstab(cross))
  
  colnames(cross)[1] ="ID_LC1"
  colnames(cross)[2] = "ID_LC2"
  colnames(cross)[3] = "ZONE_PEAT"
  colnames(cross)[4] = "PEAT"
  colnames(cross)[5] = "COUNT"
  colnames(lut.c)[1]="ID_LC1"
  colnames(lut.c)[2]="LC_t1"
  colnames(lut.c)[3]="FacEm_t1"
  data_merge <- merge(cross,lut.c,by="ID_LC1")
  colnames(lut.c)[1]="ID_LC2"
  colnames(lut.c)[2]="LC_t2"
  colnames(lut.c)[3]="FacEm_t2"
  data_merge <- as.data.frame(merge(data_merge,lut.c,by="ID_LC2"))
  colnames(lut.pu)[1]="ZONE_PEAT"
  colnames(lut.pu)[2]="Z_NAME"
  data_merge <- as.data.frame(merge(data_merge,lut.pu,by="ZONE_PEAT"))
  colnames(lut.peat)[1]="ID_LC1"
  colnames(lut.peat)[2]="LC_PEAT_t1"
  colnames(lut.peat)[3]="PEAT_EM_t1"
  data_merge <- as.data.frame(merge(data_merge,lut.peat,by="ID_LC1"))
  colnames(lut.peat)[1]="ID_LC2"
  colnames(lut.peat)[2]="LC_PEAT_t2"
  colnames(lut.peat)[3]="PEAT_EM_t2"
  data_merge <- as.data.frame(merge(data_merge,lut.peat,by="ID_LC2"))
  data_merge$LC_PEAT_t1<-NULL
  data_merge$LC_PEAT_t2<-NULL
  data_merge$PEAT[is.na(data_merge$PEAT)] <- 0
  data_merge$PEAT_EM_t1[which(data_merge$PEAT!=1)]<-0
  data_merge$PEAT_EM_t2[which(data_merge$PEAT!=1)]<-0
  rm(cross)
  
  # Calculate carbon stock changes
  Spat_res<-(res(ref)^2)/10000
  data_merge$FacEm_t1<-data_merge$FacEm_t1
  data_merge$FacEm_t2<-data_merge$FacEm_t2
  data_merge$ck_em<-as.integer(data_merge$FacEm_t1>data_merge$FacEm_t2)
  data_merge$ck_sq<-as.integer(data_merge$FacEm_t1<data_merge$FacEm_t2)
  data_merge$em<-(data_merge$FacEm_t1-data_merge$FacEm_t2)*data_merge$ck_em*data_merge$COUNT*3.67*Spat_res
  data_merge$sq<-(data_merge$FacEm_t2-data_merge$FacEm_t1)*data_merge$ck_sq*data_merge$COUNT*3.67*Spat_res
  data_merge$LU_CHG <- do.call(paste, c(data_merge[c("LC_t1", "LC_t2")], sep = " to "))
  data_merge$null<-0
  data_merge$nullCek<-data_merge$em+data_merge$sq
  # Calculate below ground emission from peat
  period<-peat_t2-peat_t1
  data_merge$em_peat<-((data_merge$PEAT_EM_t1+data_merge$PEAT_EM_t2)/2)*period*data_merge$COUNT*Spat_res
  data_merge$em_tot<-data_merge$em+data_merge$em_peat
  
  #generate area_zone lookup and calculate min area
  area_zone<-as.data.frame(freq(pu_pu1))
  colnames(area_zone)[1]<-"ID"
  colnames(area_zone)[2]<-"COUNT"
  colnames(lut.pu)[1]<-"ID"
  area_zone<-merge(area_zone, lut.pu, by="ID")
  area<-min(sum(area_zone$COUNT), sum(data_merge$COUNT))
  
  
  #create QUES-C database
  
  #make zonal statistics database
  lg<-length(unique(data_merge$ZONE))
  zone_lookup<-area_zone
  data_zone<-area_zone
  data_zone$Z_CODE<-toupper(abbreviate(data_zone$Z_NAME))
  for(i in 1:lg){
    data_z<-data_merge[which(data_merge$ZONE_PEAT == i),]
    data_zone$Avg_C_t1[which(data_zone$ID == i)]<-sum(data_z$FacEm_t1*data_z$COUNT)/sum(data_z$COUNT)
    data_zone$Avg_C_t2[which(data_zone$ID == i)]<-sum(data_z$FacEm_t2*data_z$COUNT)/sum(data_z$COUNT)
    data_zone$Rate_em[which(data_zone$ID == i)]<-sum(data_z$em)/(sum(data_z$COUNT)*period)
    data_zone$Rate_seq[which(data_zone$ID == i)]<-sum(data_z$sq)/(sum(data_z$COUNT)*period)
    data_zone$Rate_em_peat[which(data_zone$ID == i)]<-sum(data_z$em_peat)/(sum(data_z$COUNT)*period)
    data_zone$Rate_em_tot[which(data_zone$ID == i)]<-sum(data_z$em_tot)/(sum(data_z$COUNT)*period)
  }
  data_zone[,5:8]<-round(data_zone[,5:8],digits=3)
  
  # Additional statistic
  data_merge2<-subset(data_merge, COUNT>0)
  
  em_above<-sum(data_merge2$em)
  em_below<-sum(data_merge2$em_peat)
  rate_above<-em_above/(sum(data_merge2$COUNT)*period)
  rate_below<-em_below/(sum(data_merge2$COUNT)*period)
  percent_below<-(em_below/(em_above+em_below))*100
  
  # Overall emission
  overal_em<-cbind(em_above, em_below, rate_above, rate_below, percent_below)
  colnames(overal_em)[1]<-"Emission (tCO2)"
  colnames(overal_em)[2]<-"Peat Emission (tCO2)"
  colnames(overal_em)[3]<-"Emission Rate (tCO2/ha.yr)"
  colnames(overal_em)[4]<-"Peat Emission Rate (tCO2/ha.yr)"
  colnames(overal_em)[5]<-"Percent Peat Emission (%)"
  
  #Largest source
  source.melt <- melt(data = data_merge2, id.vars=c('LU_CHG'), measure.vars=c('em_peat'))
  source.cast <- dcast(data = source.melt, formula = LU_CHG ~ ., fun.aggregate = sum)
  source.cast2 <- source.cast[order(-source.cast$.),]
  source.cast2sel<-head(source.cast2, n=5)
  colnames(source.cast2sel)[1]<-"LU_Change"
  colnames(source.cast2sel)[2]<-"Peat_Emission_in_tCO"
  summ<-sum(source.cast2sel[2])
  
  dirQUESC_peat<-paste(dirname(proj.file), "/QUES/QUES-C/QUESC_peat_analysis_",as.character(peat_t1),"-",as.character(peat_t2), sep="")
  dir.create(dirQUESC_peat, mode="0777")
  setwd(dirQUESC_peat)
  
  text<-paste("QUESC_peat_emission_database_", peat_t1, "_", peat_t2, sep="")
  write.dbf(data_merge2, paste(text, ".dbf", sep=""))
  eval(parse(text=(paste(text, "<-data_merge2", sep=""))))
  eval(parse(text=(paste("resave(", text, ', file="',proj.file, '")', sep=""))))
  
  #rtf report file
  title<-"\\b\\fs40 LUMENS-QUES-C Peat Project Report\\b0\\fs20"
  sub_title<-"\\b\\fs32 PERHITUNGAN EMISI PADA LAHAN GAMBUT\\b0\\fs20"
  date<-paste("Date : ", date(), sep="")
  time_start<-paste("Processing started : ", time_start, sep="")
  time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
  #area_name_rep<-paste("\\b", "\\fs20", Location, "\\b0","\\fs20")
  line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
  rtffile <- RTF("LUMENS_QUES-C_PEAT_report.lpr", font.size=9)
  addParagraph(rtffile, title)
  addParagraph(rtffile, sub_title)
  addNewLine(rtffile)
  addParagraph(rtffile, line)
  addParagraph(rtffile, date)
  addParagraph(rtffile, time_start)
  addParagraph(rtffile, time_end)
  addParagraph(rtffile, line)
  addNewLine(rtffile)
  #addParagraph(rtffile, "Rekonsiliasi unit perencanaan adalah proses untuk menyelesaikan tumpang-tindih ijin dengan merujuk pada peta acuan/referensi fungsi. Rekonsiliasi dilakukan dengan menganalisa kesesuaian fungsi antara data-data ijin dengan data referensi. Data ijin yang dimaksud datapat berupa data konsesi pengelolaan hutan, ijin perkebunan, ijin tambang dan lain sebagainya, Sedangkan data referensi yang digunakan dapat berupa data rencana tata ruang atau penunjukan kawasan. ")
  addNewLine(rtffile)
  
  addParagraph(rtffile, "\\b \\fs32 HASIL PERHITUNGAN EMISI GAMBUT\\b0 \\fs20")
  addParagraph(rtffile, line)
  #addParagraph(rtffile, "Pada bagian ini ditunjukkan hasil proses rekonsiliasi dengan menggunakan tambahan unit perencanaan ")
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addParagraph(rtffile, "\\b \\fs18 Tabel Perhitungan Emisi Gambut Keseluruhan\\b0 \\fs18")
  addNewLine(rtffile)
  addTable(rtffile, overal_em)
  addNewLine(rtffile)
  addParagraph(rtffile, "\\b \\fs18 Tabel Perhitungan Emisi Gambut Pada Unit Perencanaan\\b0 \\fs18")
  addNewLine(rtffile)
  
  addTable(rtffile, data_zone,  font.size=8)
  addNewLine(rtffile)
  addParagraph(rtffile, "\\b \\fs18 Tabel Perubahan Penggunaan Lahan Yang Menyebabkan Emisi Gambut\\b0 \\fs18")
  addNewLine(rtffile)
  
  addTable(rtffile, source.cast2sel)
  addNewLine(rtffile)
  
  done(rtffile)
  
  command<-paste("start ", "winword ", dirQUESC_peat, "/LUMENS_QUES-C_PEAT_report.lpr", sep="" )
  shell(command)
