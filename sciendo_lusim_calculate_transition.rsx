##QUES-PostgreSQL=group
##proj.file=string
##landuse_1=string
##landuse_2=string
##planning_unit=string
##raster.nodata=number 0

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
library(splitstackshape)
library(XML)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#=Load active project
load(proj.file)

# set driver connection
driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

# return the selected data from the list
list_of_data_luc<-dbReadTable(DB, c("public", "list_of_data_luc"))
list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
list_of_data_lut<-dbReadTable(DB, c("public", "list_of_data_lut"))

data_luc1<-list_of_data_luc[which(list_of_data_luc$RST_NAME==landuse_1),]
data_luc2<-list_of_data_luc[which(list_of_data_luc$RST_NAME==landuse_2),]
data_pu<-list_of_data_pu[which(list_of_data_pu$RST_NAME==planning_unit),]
data_lut<-list_of_data_lut[which(list_of_data_lut$TBL_NAME==lookup_lc),]

#=Set initial variables
# time period
T1<-data_luc1$PERIOD
T2<-data_luc2$PERIOD

#=Set working directory
pu_name<-data_pu$RST_NAME
idx_SCIENDO_lucm<-idx_SCIENDO_lucm+1
SCIENDO_folder<-paste(idx_SCIENDO_lucm, "_SCIENDO_lucm_",T1,"_",T2,"_",pu_name,sep="")
result_dir<-paste(dirname(proj.file),"/SCIENDO/", SCIENDO_folder, sep="")
dir.create(result_dir)

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
  count_ref<-as.data.frame(freq(ref))
  count_ref<-na.omit(count_ref)
  colnames(count_ref)<-c("IDADM", "COUNT")
  ref_table<-dbReadTable(DB, c("public", data_pu$LUT_NAME)) 
  lookup_z<-merge(count_ref, ref_table, by="IDADM")
} else {
  zone<-getRasterFromPG(pgconf, project, data_pu$RST_DATA, paste(data_pu$RST_DATA, '.tif', sep=''))
  lookup_z<-dbReadTable(DB, c("public", data_pu$LUT_NAME)) 
}
# landuse first time period
landuse1<-getRasterFromPG(pgconf, project, data_luc1$RST_DATA, paste(data_luc1$RST_DATA, '.tif', sep=''))
# landuse second time period
landuse2<-getRasterFromPG(pgconf, project, data_luc2$RST_DATA, paste(data_luc2$RST_DATA, '.tif', sep=''))

#=Projection handling
if (grepl("+units=m", as.character(ref@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref)[1]*res(ref)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(ref@crs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else{
  statuscode<-0
  statusmessage<-"Raster map projection is unknown"
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  quit()
}

########################################################################################################################
# LIBRARY                                                                                                              #
########################################################################################################################
transition_dir <- paste(result_dir,"/transition", sep="")
dir.create(transition_dir)


# set working directory, create new if it doesn't exist 
setwd(result_dir)
writeRaster(landuse1, filename="landuse_1.tif", format="GTiff", datatype='INT1U', overwrite=TRUE)
writeRaster(landuse2, filename="landuse_2.tif", format="GTiff", datatype='INT1U', overwrite=TRUE)
writeRaster(zone, filename="zone.tif", format="GTiff", datatype='INT1U', overwrite=TRUE)

urlAddressRaster <- result_dir
urlEgoml <- result_dir
urlDINAMICAConsole='C:/Program Files/Dinamica EGO/DinamicaConsole.exe'
timestep <- period2-period1

########################################################################################################################
# DETERMINE TRANSITION MATRIX                                                                                          #
########################################################################################################################

setwd(urlEgoml)
# begin writing tag
con <- xmlOutputDOM(tag="script")
# add property
con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-17 12:02:15"))
con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))

# begin.
# add functor = LoadCategoricalMap-PU
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Regions"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Municipalities"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', result_dir, '/zone.tif"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), 0)
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id=paste("v",1,sep="")))
con$closeTag("functor")
# end.

# begin.
# add functor = LoadCategoricalMap-LANDUSE_1
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Initial landscape map"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', result_dir, '/landuse_1.tif"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), 0)
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id=paste("v",2,sep="")))
con$closeTag("functor")
# end.

# begin.
# add functor = LoadCategoricalMap-LANDUSE_2
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Final landscape map"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', result_dir, '/landuse_2.tif"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), 0)
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id=paste("v",3,sep="")))
con$closeTag("functor")
# end.

# begin.
# add containerfunctor = ForEachRegion
con$addTag("containerfunctor", attrs=c(name="ForEachRegion"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="forEachRegion"))
con$addTag("inputport", attrs=c(name="regions", peerid="v3"))
con$addTag("inputport", attrs=c(name="borderCells"), 0)
con$addTag("internaloutputport", attrs=c(name="regionManager", id="v4"))
con$addTag("internaloutputport", attrs=c(name="step", id="v5"))

# add subtag functor for Landuse1
con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v1"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v5"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v6"))
con$closeTag("functor")

# add subtag functor for Landuse2
con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v2"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v5"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v7"))
con$closeTag("functor")

# add subtag functor for DetermineTransitionMatrix
con$addTag("functor", attrs=c(name="DetermineTransitionMatrix"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Transition Rates"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Calculate the transition rates"))
con$addTag("inputport", attrs=c(name="initialLandscape", peerid="v6"))
con$addTag("inputport", attrs=c(name="finalLandscape", peerid="v7"))
con$addTag("inputport", attrs=c(name="timeSteps"), timestep)
con$addTag("outputport", attrs=c(name="singleStepMatrix", id="v8"))
con$addTag("outputport", attrs=c(name="multiStepMatrix", id="v9"))
con$closeTag("functor")

# add subtag functor for SaveTable
con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable567"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Single-step transition matrix."))
con$addTag("inputport", attrs=c(name="table", peerid="v8"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', transition_dir, '/Single_step.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add subtag functor for SaveTable
con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable566"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Multi-step transition matrix."))
con$addTag("inputport", attrs=c(name="table", peerid="v9"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', transition_dir, '/Multi_step.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

con$closeTag("containerfunctor")  

saveXML(con$value(), file=paste(urlEgoml, "/1_Transition_Matrix_per_Region.egoml", sep=''))
command<-paste('"', urlDINAMICAConsole, '" -processors 0 -log-level 4 "', urlEgoml, '/1_Transition_Matrix_per_Region.egoml"', sep="")
system(command)

resave(idx_SCIENDO_lucm, file=proj.file)
