##proj.file=string
##pd_1 = string "2000"
##pd_2 = string "2005"
##pristine_pd = string "1990"
##planning_unit = string "pu_IDH_48s_100m"
##raster.nodata = number 0
##focal_coverage = string "asdf.csv"
##edgecon = string "contrast_table_lut"
##window.shape = selection Bujur sangkar; Lingkaran
##windowsize = number 1000
##gridres = number 10000

# AD
# QUES-B final version Bahasa Indonesia
# 30/10/2017-17/11/2017

# loading required packages====
library(vegan)
library(DBI)
library(raster)
library(RSQLite)
library(SDMTools)
library(sp)
library(rtf)
library(rgdal)
library(spatial.tools)
library(ggplot2)
library(plyr)
library(grid)
library(tiff)
library(RColorBrewer)
library(rasterVis)
library(reshape2)
library(foreign)
library(dplyr)
library(tcltk)
library(gridExtra)
library(pracma)
library(rgeos)
library(zoo)
library(DBI)
library(RPostgreSQL)
library(rpostgis)

# INPUTS testonly====
# proj.file= "D:/LUMENS/trial/trial/trial.lpj" # automatically defined in LUMENS; referring the loaded project
# planning_unit="pu_IDH_48s_100m" # Dropdown from the input planning unit data
# gridres= 10000 # filled input
# windowsize= 1000 # filled input
# window.shape= 1 # choose square (0) or circle (1)
# raster.nodata= 0 # ? read from the metadata
# pd_1 = 2000 # dropdown
# pd_2 = 2005 # dropdown
# pristine_pd = 1990 # dropdown which is the period in which the total focal area is regarded as pristine
# edgecon = "contrast_table_lut" # dropdown based on the contents of the lut data list in the database
# focal_coverage = c(1, 4, 6) # the ids of undisturbed terr forest, swamp, and mangrove
user_doc <- Sys.getenv("USERPROFILE") # in case the create_db script has not been modified to save user_doc variable

# INPUT reading process
focal_coverage <- read.csv(focal_coverage, stringsAsFactors = FALSE)
focal_coverage <- as.numeric(focal_coverage[,1])


# Parameterization and pre-processings====
load(proj.file) # loading the project file
# Static parameters and variables
time_start <- format(Sys.time())
fca_dir <- user_doc # directory in which the .fca file is located during the installation user_doc = Sys.getenv("USERPROFILE")
# setting up the connection with the PostGre database system
driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host='localhost', port=5433,
  user='postgres', password='root'
)

# Setting up the working directory
setwd(paste0(dirname(proj.file), "/QUES"))
# derive the list of available data----
list_of_data_luc<-dbReadTable(DB, c("public", "list_of_data_luc"))
list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
list_of_data_lut<-dbReadTable(DB, c("public", "list_of_data_lut"))
list_of_data_f<-dbReadTable(DB, c("public", "list_of_data_f"))
# load the chosen planning unit raster
zone <- getRasterFromPG(pgconf, project, list_of_data_pu[list_of_data_pu$RST_NAME == planning_unit, "RST_DATA"], paste0(list_of_data_pu[list_of_data_pu$RST_NAME == planning_unit, "RST_NAME"], ".tif"))
lookup_z <- dbReadTable(DB, c("public", list_of_data_pu[list_of_data_pu$RST_NAME == planning_unit,"LUT_NAME"]))
# load the pristine state landcover map and lookup table
prist_lcmap <- getRasterFromPG(pgconf, project, list_of_data_luc[list_of_data_luc$PERIOD == pristine_pd, "RST_DATA"], paste0(list_of_data_luc[list_of_data_luc$PERIOD == pristine_pd, "RST_NAME"], ".tif"))
prist_lut <- dbReadTable(DB, c("public", list_of_data_luc[list_of_data_luc$PERIOD == pristine_pd, "LUT_NAME"]))
# definition of the landcover_lookup_table
# 1. assess the consistency of the landcover classes between the pd_1 and pd_2 and the edgecon
# load tables to be assessed
contab <- dbReadTable(DB, c("public", list_of_data_lut[which(list_of_data_lut$TBL_NAME == edgecon),"TBL_DATA"]))
lut_lc1 <- dbReadTable(DB, c("public", list_of_data_luc[list_of_data_luc$PERIOD == pd_1,"LUT_NAME"]))
lut_lc2 <- dbReadTable(DB, c("public", list_of_data_luc[list_of_data_luc$PERIOD == pd_2,"LUT_NAME"]))
# compare the columns to check the consistency. the lut_lc1 and lut_lc2 are not compared because there are occasions where some classes are not present in a certain period
column_legend1 <- contab[,1]
id_legend1 <- row.names(contab)
for(i in 1:2){
  if (i == 1){ # compare between the contrast table and the first period land cover map
    column_legend2 <- lut_lc1[lut_lc1$ID != raster.nodata, "Legend"]
    id_legend2 <- as.character(lut_lc1[lut_lc1$ID != raster.nodata, "ID"])
  } else if(i == 2){ # compare between the contrast table and the second period land cover map
    column_legend2 <- lut_lc2[lut_lc2$ID != raster.nodata, "Legend"]
    id_legend2 <- as.character(lut_lc2[lut_lc2$ID != raster.nodata, "ID"])
  }
  column_legend1 <- sort(column_legend1)
  column_legend2 <- sort(column_legend2)
  if(FALSE %in% unique(column_legend2 %in% column_legend1)) stop("Inconsistency of land cover classes detected. Please make sure the inputs have the exact same land cover classes")
  # checking the consistency of the IDs given as well
  contab_ids_exist <- as.character(which(contab[,1] %in% column_legend2))
  if(!identical(contab_ids_exist, id_legend2)) stop("Inconsistency of land cover classes detected. Please make sure the inputs have the exact same land cover classes")
}
# 2. generate the 'lulc_lut' based on the contab. Assuming the contab has the complete list of land cover classes in the studied landscape
lulc_lut <- data.frame(ID = row.names(contab), Legend = contab[, 1], stringsAsFactors = FALSE) # Value 0 ~ No Data has been omitted

#saving the contrast table to be read by fragstats====
cont_csv <- paste0(LUMENS_path_user,"/", edgecon,".csv")
cont_csv <- gsub("\\\\", "/", cont_csv)
#buat dua baris sisipan di atas data index
LU_names <-contab[,1]
#baris 0
b0_ctr_tab <- vector(mode = "character", length = nrow(contab))
for(i in 1:length(b0_ctr_tab)){
  if(i == 1) b0_ctr_tab[[i]] <- paste("FSQ_TABLE")
  else b0_ctr_tab[[i]] <- NA_character_
}
#baris 1
b1_ctr_tab <- vector(mode = "character", length = nrow(contab))
for(i in 1:length(b1_ctr_tab)){
  if(i == 1) b1_ctr_tab[[i]] <- paste("#CLASS_LIST_LITERAL(",LU_names[i],sep="")
  else if(i==length(b1_ctr_tab)) b1_ctr_tab[[i]] <- paste0(LU_names[i],")") else b1_ctr_tab[[i]] <- LU_names[i]
}
#baris 2
b2_ctr_tab <- vector(mode = "character", length = nrow(contab))
for(i in 1:length(b2_ctr_tab)){
  if(i == 1) b2_ctr_tab[[i]] <- paste("CLASS_LIST_NUMERIC(1")
  else if (i==length(b2_ctr_tab)) b2_ctr_tab [[i]] <- paste0(i,")") else b2_ctr_tab[[i]] <- i
}
# rbind b0+b1+b2+dis.ind
dis.indc.df <- contab[,2:ncol(contab)]
gabungan <- as.data.frame(rbind(b0_ctr_tab, b1_ctr_tab, b2_ctr_tab, dis.indc.df))
colnames(gabungan) <- NA
write.table(gabungan, cont_csv, sep =",", row.names = FALSE, col.names = FALSE, na = "", quote = FALSE)

# define 'fa_class', a variable with chosen focal area land cover class====
fa_class <- character()
for(f in 1: length(focal_coverage)){
  fa_class <- c(fa_class, lulc_lut[lulc_lut$ID == focal_coverage[f], "Legend"])
}
# abbreviate to avoid spacing
fa_class <- abbreviate(fa_class)
# class descriptor table creation====
# class descriptor table is stored as a .csv file located in the LUMENS_path_user (temporary file)
# it describes the settings regarding how each ID should be treated
# contains the columns: ID, Name, Enabled, IsBackground
cl_desc <- data.frame(ID = c(raster.nodata, as.numeric(lulc_lut$ID)), Name = c("No data", lulc_lut$Legend), Enabled = FALSE, IsBackground= FALSE)
classdesc <- paste0(gsub("\\\\", "/", LUMENS_path_user), "/desc.csv") # the address location of the class descriptor table in the temporary folder
write.csv(cl_desc, classdesc, row.names = FALSE, quote = FALSE) # not using quotation to prevent error during fragstats execution

# habitat table creation====
lookupTableHabitat <- cl_desc[,1:3]
names(lookupTableHabitat) <- c("ID", "CLASS", "BIODIV")
lookupTableHabitat$BIODIV <- 0
habitat_table <- paste0(gsub("\\\\", "/", LUMENS_path_user), "/habitat.csv")
write.csv(lookupTableHabitat, habitat_table, row.names = FALSE, quote = FALSE)
# General details====
Spat_res <- res(ref)[1] ^ 2 / 10000 # spatial resolution in hectare unit
location <- as.character(proj_descr[proj_descr$Type == "location", "Description"])
quesb_dir <- paste0(getwd(), "/QUES-B")

#====G Raster Nodata Handling====
#Raster nodata check
nodata.cek<-0 # assign initial value of 0 to raster files checked in this routine
bperiod <- as.character(c(pd_1, pd_2)) # number of periods under consideration
for (b in 1: length(bperiod)){ # checking the existance of normalized landcover maps
  raster.cek<-paste(quesb_dir,"/landuse_tNA_", bperiod[b],".tif", sep='')
  eval(parse(text=(paste("if( file.exists('",raster.cek ,"')){print('landuse_tNA_", bperiod[b],".tif is available'); nodata.cek<-nodata.cek+1}", sep=""))))
}

#RUN NODATA SYNC FOR ALL RASTER FILES IF NOT AVAILABLE YET IN QUES-B DIRECTORY====
# load the raster from the postgre database into landCover variables
for ( b in 1: length(bperiod)){
  eval(parse(text= paste0("landCover", bperiod[b], " <- getRasterFromPG(pgconf,'", project, "', '", list_of_data_luc[list_of_data_luc$PERIOD == as.numeric(bperiod[b]), "RST_DATA"],
                          "', '", list_of_data_luc[list_of_data_luc$PERIOD == as.numeric(bperiod[b]), "RST_NAME"], ".tif')" )))
}
# Synchronization processes
if (nodata.cek<length(bperiod)){
  #Create temporary boolean raster value
  for(i in 1: length(bperiod)){
    eval(parse(text=(paste("landuse_t",i,"<- reclassify(landCover",bperiod[i],",cbind(",raster.nodata,",NA))", sep=''))))
    eval(parse(text=(paste("Landuse_t",i,"_temp<-!is.na(landuse_t",i,")", sep=''))))
  }
  #No data check raster
  lu.nodata.check<-Landuse_t1_temp
  for(i in 2:length(bperiod)){
    eval(parse(text=(paste("lu.nodata.check<-lu.nodata.check*Landuse_t",i,"_temp", sep=''))))
  }
  #syncronized nodata raster map
  for(i in 1:length(bperiod)){
    eval(parse(text=(paste("landuse_t",i,"<-lu.nodata.check*landuse_t",i, sep=''))))
    eval(parse(text=(paste("landuse_t",i,"<- reclassify(landuse_t",i,", cbind(",raster.nodata,",NA))", sep=''))))
    lu_path<-paste(quesb_dir,"/landuse_tNA_", bperiod[i],".tif", sep="")
    eval(parse(text=(paste("writeRaster(landuse_t",i,",  filename='",quesb_dir,"/",basename(lu_path),"', format='GTiff', overwrite=TRUE, NAflag=255)", sep=''))))
  }
} else { # if already exist, there is no need to write the raster into a hard file; these steps are required because later on, the lu.nodata.check will be used during the mapping process
  print ("landuse_tNA files are ready")
  for(i in 1: length(bperiod)){
    eval(parse(text=(paste("landuse_t",i,"<- reclassify(landCover",bperiod[i],",cbind(",raster.nodata,",NA))", sep=''))))
    eval(parse(text=(paste("Landuse_t",i,"_temp<-!is.na(landuse_t",i,")", sep=''))))
  }
  #No data check raster
  lu.nodata.check<-Landuse_t1_temp
  for(i in 2:length(bperiod)){
    eval(parse(text=(paste("lu.nodata.check<-lu.nodata.check*Landuse_t",i,"_temp", sep=''))))
  }
}

#Lu_path definition----
# the file locations of the synchronized LU raster data
for(i in 1: length(bperiod)){
  eval(parse(text=paste0("lu", i, "_path <-'", quesb_dir, "/landuse_tNA_", bperiod[i],".tif'")))
}


# DEFINE FUNCTIONS====
# 0. polygonize
# Define the function as written in https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath= paste0(LUMENS_path,"/bin/gdal_polygonize.py"), readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}
# 1. Calculation of pristine state focal area based on the prist_lcmap
pr_farea <- function(fclass = "Undisturbed forest"){
  # check the compatibility of the id-legend of the pristine_pd with the used map
  if(fclass %in% prist_lut$Legend){
    faarea <- ncell(prist_lcmap[prist_lcmap == prist_lut[prist_lut$Legend == fclass, "ID"]])
  } else{
    stop("Unable to proceed. Please check the consistency of the chosen pristine state land cover map")
  }
  # if proven unmatched and there is no such fclass in the prist_lut, stop. 
  # calculate the number of cells which belong to the fclass
  return(faarea)
}
# function to generate sampling grid (polygon)
generate_sampling_grid <- function(g_res = gridres, n = 1000){
  ref_poly <- rasterToPolygons(ref,na.rm = TRUE)
  if(g_res > 0){
    sampling.grid <- spsample(x = ref_poly, cellsize = g_res, type = "regular", offset = c(0.5, 0.5))
  } else{
    sampling.grid <- spsample(x = ref_poly, n = 1000, type = "regular", offset = c(0.5, 0.5))
  }
  ngrid <- length(sampling.grid)
  list_gr <- list()
  for(s in 1:ngrid){
    list_gr[s] <-  gBuffer(sampling.grid[s], width = g_res/2 , capStyle = "SQUARE")
  }
  sampling.grid <- do.call(bind, list_gr)
  return(sampling.grid)
}
#Focal Area Function
focal.area<-function(landuse.character){
  eval(parse(text=paste("colnames(freq",landuse.character,")[1]<-'ID'", sep='')))
  eval(parse(text=paste("foc.area.reclass<-merge(freq",landuse.character,",cl_desc,by='ID')", sep='')))
  foc.area.reclass <- foc.area.reclass[, c("ID", "Enabled")]
  eval(parse(text=paste("foc.area<-reclassify(",landuse.character, ",foc.area.reclass, right=NA)", sep='')))
  return (foc.area)
}
# function to extract the area of farea within each grid cell
foc.area.grid.sampled<-function(foc.area, sampling.grid){
  tothab<-extract(foc.area, sampling.grid, fun = sum, df= TRUE)
  # tothab<-data.frame(ID = sampling.grid$ID, tothab) # AD chg
  colnames(tothab) <- c("ID", "sum")
  tothab$sum<-((tothab$sum/totarea)*100)
  return(tothab)
}

teci.analysis<-function(landuse, lu_path){
  modid=1
  internal<-paste('')
  cpf<-paste('')
  io<-paste('[BAND:1]')
  desc<-paste('')
  drlib<-paste('GDAL')
  drname<-paste('GeoTIFF grid (.tif)')
  drid<-paste('63B45E15-C8E5-44f6-A9AB-60E1852CDB5D')
  
  #extent input of file 1
  xl1<-xmin(landuse)
  yl1<-ymin(landuse)
  xu1<-xmax(landuse)
  yu1<-ymax(landuse)
  
  #cell size input of file 1
  csize1<-xres(landuse)
  #row and column size input of file 1
  rowc1<-nrow(landuse)
  colc1<-ncol(landuse)
  
  
  aczero="1"
  #no data value input
  nodata=255
  bvalue=999
  #   nodata=0#ML
  #   bvalue=128#ML
  #dirname_raster<-dirname(paste(getwd(),"/landuse_tNA_", data[1,2],".tif", sep=''))
  #setwd(dirname_raster)
  
  #Clean previous teci process
  for (i in 1:3){
    mwout<-paste(lu_path,'_mw',i, sep='')
    teci.dir<-paste(mwout,"/",list.files(mwout), sep='')
    if (file.exists(teci.dir)){
      mwout2<-paste(lu_path,'_mw',i, sep='')
      unlink(mwout2, recursive=TRUE)
      print(paste(i,"deleting previous raster file found, algorithm continue..."))
    }else{
      print(paste(i,"no previous raster file found, algorithm continue..."))
    }
  }
  
  #Check .FCA model
  
  if (file.exists(paste(fca_dir,'/teciuf.fca',sep=''))){
    fca<-paste(fca_dir,'/teciuf.fca',sep='')
    print("Fragstats' model found!")
  } else { stop("Fragstats model file is not found, please make sure the file is located in your LUMENS folder in Program files")
  }
  
  #=Connect to fragstats' .fca file=
  SQLite()
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname=as.character(fca), max.con = 200, fetch.default.rec = 500, force.reload = FALSE, shared.cache=FALSE, staged.queries = TRUE)
  
  #delete all record from frg_landscape layer
  del<-paste("DELETE FROM frg_landscape_layers")
  ll <- dbSendQuery(con, del)
  
  input_desc<-paste("UPDATE frg_table_strings SET value='",classdesc,"' WHERE rec_id=5;",sep="")
  input_edge<-paste("UPDATE frg_table_strings SET value='",cont_csv,"' WHERE rec_id=2;",sep="")
  input_out<-paste("UPDATE frg_table_strings SET value='",quesb_dir,"' WHERE rec_id=6;",sep="")
  input_window_size_sqr<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=18;"); #change square window radius
  input_window_size_circ<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=19;"); #change circle window radius
  input_window_type<-paste("UPDATE frg_table_numerics SET value=",window.shape,"WHERE rec_id=13;")
  ll <- dbSendQuery(con, input_desc)
  ll <- dbSendQuery(con, input_edge)
  ll <- dbSendQuery(con, input_out)
  ll <- dbSendQuery(con, input_window_size_sqr)
  ll <- dbSendQuery(con, input_window_size_circ)
  ll <- dbSendQuery(con, input_window_type)
  
  landlayer1<-paste("INSERT INTO frg_landscape_layers(model_id, name_internal, name_external, io_info, driver_lib, driver_name, driver_id, xll, yll, xur, yur, cell_size, row_count, col_count, allow_class_zero, no_data_value, background_value) VALUES ('",modid,"','",internal,"','",lu_path,"','",io,"','",drlib,"','",drname,"','",drid,"','",xl1,"','",yl1,"','",xu1,"','",yu1,"','",csize1,"','",rowc1,"','",colc1,"','",aczero,"','",nodata,"','",bvalue,"');",sep="")
  
  ll <- dbSendQuery(con, landlayer1)
  
  #Fragstats directory
  if (file.exists("C:/Program Files (x86)/Fragstats 4")){
    setwd("C:/Program Files (x86)/Fragstats 4/")
  } else{
    setwd("C:/Program Files/Fragstats 4/")
  }
  
  #= Execute fragstats, generate TECI=
  sysout<-paste(quesb_dir, "/fragout", sep="")
  f <- paste('frg -m',shQuote(fca),' -o',sysout)
  system(f)
  
  #delete all record from frg_landscape layer
  del<-paste("DELETE FROM frg_landscape_layers")
  ll <- dbSendQuery(con, del)
  
  setwd(quesb_dir)
  mwout<-paste(lu_path,'_mw1', sep='')
  teci.dir<-paste(mwout,"/",list.files(mwout)[1], sep='')
  
  #convert -999 value to NA
  mwfile<-raster(teci.dir)
  NAvalue(mwfile)<-(999*-1)
  
  return(mwfile)
}

saveTECI<-function(mwfile, location, period){
  file.teci<-paste('TECI_',location,'_',period,'_NA',sep='')
  writeRaster(mwfile, filename=file.teci, format="GTiff", overwrite=TRUE)
  return(print(paste(file.teci, "has been written")))
}

saveFocal<-function(foc.area, location, period){
  file.habitat.name<-paste('focal_area_',location,'_',period, sep='')
  writeRaster(foc.area, filename=file.habitat.name, format="GTiff", overwrite=TRUE)
  return(print(paste(file.habitat.name, "has been written")))
}

generateDIFAtable<-function(mwfile, tothab, location, period, sampling.grid){
  tecival<-extract(mwfile, sample_grid, fun = mean, method='simple', na.rm=T, df=T) # extracting the values of the teci raster
  # tecival<-extract(mwfile, sample_grid, fun = mean, method='simple', na.rm=T, df=T) # extracting the values of the teci raster
  poly.data<-data.frame(cbind(coordinates(sample_grid), sapply(slot(sample_grid, "polygons"), function(x) slot(x, "ID"))), stringsAsFactors = FALSE)
  colnames(poly.data)<-c("x","y","ID.grid") #ID = id grid
  poly.data$ID.grid <- as.numeric(poly.data$ID.grid)
  # apply indexing scheme
  # y as character
  y_index <- data.frame(y_val = unique(poly.data$y), y_idx = 0)
  y_index <- y_index[order(y_index$y_val, decreasing = TRUE),]
  y_index$y_idx <- 1:nrow(y_index)
  # x as numeric
  x_index <- data.frame(x_val = unique(poly.data$x), x_idx = "")
  x_index <- x_index[order(x_index$x_val), ]
  # applying the function to generate the indices
  # x
  # generate nested looping mechanism to fill down the x_idx vector
  x_idx <- character()
  n = 0
  repeat{
    # add corresponding prefix according to the n value
    n <- n + 1
    if(n == 1) pre <- "" else pre <- letters[n-1]
    # count the remaining index to be assigned
    n_idx <- nrow(x_index) - (n-1)*26
    if(n_idx > 26) loopno = 26 else loopno = n_idx
    if(loopno <= 0) break
    for(l in 1: loopno){
      x_idx <- c(x_idx, toupper(paste0(pre, letters[l])))
    }
  }
  # assign the x_idx into x_idx column
  x_index$x_idx <- x_idx
  # merge the x_ and y_ index table into poly.data
  poly.data <- merge(poly.data, x_index, by.x = "x", by.y = "x_val", all.x =TRUE)
  poly.data <- merge(poly.data, y_index, by.x = "y", by.y = "y_val", all.x = TRUE)
  poly.data$IDx.grid <- paste0(poly.data$x_idx, poly.data$y_idx)
  #combine dataframe of teci and habitat
  colnames(tecival)<-c("ID.grid","teci")
  colnames(tothab)<-c("ID.grid","sum")
  ctab<-merge(tothab,poly.data,by="ID.grid")
  ctab<-merge(ctab,tecival,by="ID.grid")
  sort.ctab <- ctab[order(ctab$teci, decreasing=F, na.last=TRUE), ]
  sort.ctab <- sort.ctab[!is.na(sort.ctab$teci),] #omission of the records with teci value == NaN. Such records have the value of sum = 0
  #removing the NA value in the 'sum' column
  sort.ctab[is.na(sort.ctab$sum), "sum"] <- 0
  #calculation of the cumulative summary
  sumtab1<-cbind(sort.ctab, Cum.Sum=cumsum(sort.ctab$sum))
  cumax<-max(sumtab1$Cum.Sum, na.rm=TRUE)
  row.names(sumtab1)<-1:nrow(sumtab1)
  # adjust position of the columns
  sumtab1 <- sumtab1[,c("ID.grid", "sum", "x", "y", "x_idx", "y_idx", "IDx.grid", "teci", "Cum.Sum")]
  # add a closure record at the bottom part of the sumtab1 data.frame
  sumtab1 <- rbind(sumtab1, data.frame(ID.grid = sumtab1$ID.grid[nrow(sumtab1)], sum = 100, x = sumtab1$x[nrow(sumtab1)], y = sumtab1$y[nrow(sumtab1)], x_idx = sumtab1$x_idx[nrow(sumtab1)], y_idx = sumtab1$y_idx[nrow(sumtab1)], IDx.grid = sumtab1$IDx.grid[nrow(sumtab1)], teci = 100,Cum.Sum = cumax))
  melted_sumtab1 <- melt(sumtab1,id.vars = 'teci', measure.vars = 'sum')
  sumtab1_fin <- dcast(data = melted_sumtab1, formula = teci ~ variable, fun.aggregate = sum, subset = .(melted_sumtab1$teci!=100))
  sumtab1_fin$Cum.Sum <-cumsum(sumtab1_fin$sum)
  sumtab1_fin[nrow(sumtab1_fin)+1,] <- c(100, 100, sumtab1_fin[nrow(sumtab1_fin),'Cum.Sum'])
  # sumtab1<-round(sumtab1,digits=2)
  colnames(sumtab1)<-c("ID.grid","Habitat Area(%)","X.cor","Y.cor","ID.x", "ID.y", "IDx.grid", "TECI(%)", "Cumulative Habitat(%)")
  nama.tabel.teci<-paste("QUES-B_DIFA_Table_", location,"_", period, ".csv", sep='')
  write.table(sumtab1, nama.tabel.teci, row.names = FALSE, sep=",")
  return (sumtab1_fin)
}

#habitat changes function
subsequent.changes<-function(habitat.analysis,lu_db, analysis, location, T1, T2){
  habitat.analysis.bol<-reclassify(habitat.analysis/habitat.analysis, cbind(0,NA))
  luchg.analysis<-lu_chg*habitat.analysis.bol; #focal area analysis LUC
  luchg.analysis.att<-na.omit(as.data.frame(freq(luchg.analysis)))
  
  colnames(luchg.analysis.att)<-c("ID","COUNT")
  luchg.analysis.att<-merge(luchg.analysis.att,lu_db,by='ID')
  luchg.analysis.att <- luchg.analysis.att[,c("Z_NAME", "LC_t1", "LC_t2", "COUNT.x")]
  # luchg.analysis.att<-as.data.frame(cbind(luchg.analysis.att[1],luchg.analysis.att[2],luchg.analysis.att[8],luchg.analysis.att[9]), stringsAsFactors=T)
  luchg.analysis.att<-unique(luchg.analysis.att[,1:4])
  names(luchg.analysis.att)[4] <- "COUNT"
  luchg.analysis.att<-luchg.analysis.att[ order(luchg.analysis.att$COUNT, decreasing = TRUE), ]
  luchg.analysis.att<-transform(luchg.analysis.att, LUCHG=paste0(LC_t1," to " ,LC_t2))
  luchg.analysis.att$LUCHG<-as.character(luchg.analysis.att$LUCHG)
  
  for (i in 1:nrow(luchg.analysis.att)){
    if(as.character(luchg.analysis.att[i,"LC_t1"])==as.character(luchg.analysis.att[i,"LC_t2"])){
      luchg.analysis.att[i,"LUCHG"]<-paste("Persistent",luchg.analysis.att[i,"LC_t1"])
    }}
  luchg.analysis.att<-luchg.analysis.att[,c("Z_NAME", "COUNT", "LUCHG")]
  luchg.analysis.att$AREA_HA <- luchg.analysis.att$COUNT * Spat_res
  luchg.analysis.att <- luchg.analysis.att[,c("Z_NAME", "LUCHG", "AREA_HA")]
  
  setwd(quesb_folder)
  tryCatch({
    luchg.db.filename<-paste("LUCHG_",analysis,"_database",location,'_', T1,'_',T2,'.dbf', sep='')
    write.dbf(luchg.analysis.att, luchg.db.filename)
  },error=function(e){cat("Skipping database export process :",conditionMessage(e), "\n")})
  return(luchg.analysis.att)
}
#zonal stat for habitat loss
zonal_stat<-function(habitat.change, zone, lookup_z){
  habitat.change.0<-reclassify(habitat.change>0, cbind(NA, 0))
  zstat.habitat.change<-ZonalStat(habitat.change.0, zone, FUN = "sum")
  colnames(zstat.habitat.change)[1] ="ZONE"
  zstat.habitat.change<-merge(lookup_z[,c("ID", "Legend")], zstat.habitat.change, by.x = "ID", by.y="ZONE")
  zstat.habitat.change$Proportion<-((zstat.habitat.change$sum)/(sum(zstat.habitat.change$sum)))*100
  zstat.habitat.change.prop<-as.data.frame(cbind(' ','TOTAL',(sum(zstat.habitat.change$sum)), (sum(zstat.habitat.change$Proportion))))
  zstat.habitat.change$Proportion<-round(zstat.habitat.change$Proportion,digits=2)
  colnames(zstat.habitat.change.prop)<-c('ZONE','Z_NAME','sum','Proportion')
  zstat.habitat.change.prop$sum <- as.numeric(as.character(zstat.habitat.change.prop$sum)) * Spat_res 
  zstat.habitat.change<-zstat.habitat.change[ order(as.numeric(zstat.habitat.change$sum), decreasing=TRUE), ]
  #zstat.habitat.change<-rbind(zstat.habitat.change,zstat.habitat.change.prop)
  colnames(zstat.habitat.change)<-c('ID','ZONE','total.area(ha)','Proportion')
  return(zstat.habitat.change)
}
# plotting draft for the "semi-interactive" plot
TECI_chg <- function(T1,T2){
  # 1. load the csv of the QUES-B_DIFA_Table_SouthSumatra_2000.csv and the time period following
  raw_difa_tab.init <- read.csv(paste0(quesb_folder, "/QUES-B_DIFA_Table_", location, "_", T1, ".csv"), stringsAsFactors = FALSE)
  raw_difa_tab.final <- read.csv(paste0(quesb_folder, "/QUES-B_DIFA_Table_", location, "_", T2, ".csv"), stringsAsFactors = FALSE)
  # 2. list the IDx.grid s of the from the two tables
  compiled_difa <- unique(c(raw_difa_tab.init$IDx.grid, raw_difa_tab.final$IDx.grid))
  compiled_difa <- compiled_difa[order(compiled_difa)]
  compiled_difa <- data.frame(IDx.grid = compiled_difa, stringsAsFactors = FALSE)
  # 3. merging with the raw_difa_tab.init to  obtain the focal area and the TECI value for each grid
  compiled_difa <- merge(compiled_difa, raw_difa_tab.init[seq(nrow(raw_difa_tab.init) -1), ], by = "IDx.grid", all.x = TRUE)
  names(compiled_difa) <- c("IDx.grid", "ID.grid", "foc_pct.t1", "X.cor", "Y.cor", "ID.x", "ID.y", "TECI.t1", "cum_foc")
  compiled_difa <- compiled_difa[,c("IDx.grid", "foc_pct.t1", "TECI.t1")]
  
  compiled_difa <- merge(compiled_difa, raw_difa_tab.final[seq(nrow(raw_difa_tab.final) -1), ], by = "IDx.grid", all.x = TRUE)
  names(compiled_difa) <- c("IDx.grid", "foc_pct.t1", "TECI.t1", "ID.grid", "foc_pct.t2", "X.cor", "Y.cor", "ID.x", "ID.y", "TECI.t2", "cum_foc")
  compiled_difa <- compiled_difa[, c("IDx.grid", "foc_pct.t1", "foc_pct.t2", "TECI.t1", "TECI.t2")]
  
  # filling NA values: focal area (replaced by 0) and TECI (replaced by 110)
  for(c in 2:5){ # focal area columns
    if(c < 4){
      compiled_difa[eval(parse(text = paste0("is.na(compiled_difa$", names(compiled_difa)[c],")"))),c] <- 0
    } else{ # TECI columns
      compiled_difa[eval(parse(text = paste0("is.na(compiled_difa$", names(compiled_difa)[c],")"))),c] <- 110
    }
  }
  # generate jittered value of TECI.t1 and TECI.t2 for better aesthetic
  compiled_difa$jit.TECI.t1 <- jitter(compiled_difa$TECI.t1)
  compiled_difa$jit.TECI.t2 <- jitter(compiled_difa$TECI.t2)
  # plotting using ggplolt as bubble plot with alpha = 0.5
  # defining breaks a priori
  bx <- c(0,round(max(c(compiled_difa$foc_pct.t1, compiled_difa$foc_pct.t2))/2, digits = 1), floor(max(c(compiled_difa$foc_pct.t1, compiled_difa$foc_pct.t2))*10)/100*10)
  lbl <- as.character(bx)
  lbl[3] <- paste0(lbl[3], "\n(", round(max(c(compiled_difa$foc_pct.t1, compiled_difa$foc_pct.t2)))/100* totarea, " ha)")
  plot.TECI_chg <- ggplot(compiled_difa, aes(x = TECI.t1, y = TECI.t2)) + geom_abline(slope = 1, intercept = 0, color = grey(0.4)) +
    geom_point(data = compiled_difa, aes(x = jit.TECI.t1, y = jit.TECI.t2, size = foc_pct.t1, color = paste0("Area(%) ", T1)), alpha = 0.3) +
    geom_point(data = compiled_difa, aes(x = jit.TECI.t1, y = jit.TECI.t2, size = foc_pct.t2, color = paste0("Area(%) ", T2)), alpha = 0.3) +
    geom_text(data= compiled_difa,aes(x=jit.TECI.t1, y=jit.TECI.t2,label=IDx.grid), size = 3) + 
    scale_size(range = c(1,30), breaks = bx, labels = lbl) +
    # scale_size(range = c(1,30), breaks = bx) +
    geom_hline(yintercept = 100) +
    geom_vline(xintercept = 100) + labs(colour = "Legenda", size = "Luas (%) focal area", x = "TECI t1", y = "TECI t2") +
    theme_bw() + scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 111)) + scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 111))
  plot.TECI_chg <- plot.TECI_chg + coord_equal() + theme(axis.title =element_text(colour = grey(0.1)),
                                                         axis.text= element_text(colour = grey(0.2)),
                                                         plot.margin=unit(c(0,0,0,0), "cm"),
                                                         panel.grid.minor=element_blank(),
                                                         legend.title = element_text(size=12, face = "bold"),
                                                         legend.text = element_text(size = 10)
  )
  # legend size adjustment
  plot.TECI_chg <- plot.TECI_chg + guides(colour = guide_legend(override.aes = list(size = c(3,3))))
  # legend item order adjustment
  plot.TECI_chg <- plot.TECI_chg + guides(colour = guide_legend(order =1), size = guide_legend(order =2))
  return(plot.TECI_chg)
}
# mapping the site overlaid with sample_grid and indexing scheme
grid_map <- function(T1, T2){
  poly.data<-data.frame(cbind(coordinates(sample_grid), sapply(slot(sample_grid, "polygons"), function(x) slot(x, "ID"))), stringsAsFactors = FALSE)
  colnames(poly.data)<-c("x","y","ID.grid") #ID = id grid
  poly.data$ID.grid <- as.numeric(poly.data$ID.grid)
  # apply indexing scheme
  # y as character
  y_index <- data.frame(y_val = unique(poly.data$y), y_idx = 0)
  y_index <- y_index[order(y_index$y_val, decreasing = TRUE),]
  y_index$y_idx <- 1:nrow(y_index)
  # x as numeric
  x_index <- data.frame(x_val = unique(poly.data$x), x_idx = "")
  x_index <- x_index[order(x_index$x_val), ]
  # applying the function to generate the indices
  # x
  # generate nested looping mechanism to fill down the x_idx vector
  x_idx <- character()
  n = 0
  repeat{
    # add corresponding prefix according to the n value
    n <- n + 1
    if(n == 1) pre <- "" else pre <- letters[n-1]
    # count the remaining index to be assigned
    n_idx <- nrow(x_index) - (n-1)*26
    if(n_idx > 26) loopno = 26 else loopno = n_idx
    if(loopno <= 0) break
    for(l in 1: loopno){
      x_idx <- c(x_idx, toupper(paste0(pre, letters[l])))
    }
  }
  # assign the x_idx into x_idx column
  x_index$x_idx <- x_idx
  # merge the x_ and y_ index table into poly.data
  poly.data <- merge(poly.data, x_index, by.x = "x", by.y = "x_val", all.x =TRUE)
  poly.data <- merge(poly.data, y_index, by.x = "y", by.y = "y_val", all.x = TRUE) 
  poly.data$IDx.grid <- paste0(poly.data$x_idx, poly.data$y_idx)
  names(poly.data) <- c("Y.cor", "X.cor", "ID.grid", "ID.x", "ID.y", "IDx.grid")
  poly.data$Y.cor <- as.numeric(poly.data$Y.cor)
  poly.data$X.cor <- as.numeric(poly.data$X.cor)
  # c("ID.grid","Habitat Area(%)","X.cor","Y.cor","ID.X", "ID.y", "IDx.grid", "TECI(%)", "Cumulative Habitat(%)")
  # DATA: bacground, sample_grid
  # minor.label
  aggregation_map <- gplot(background) + geom_raster(aes(fill=as.character(value)), show.legend = FALSE) #+ scale_fill_manual(values = st_cols, breaks = 0)
  # adding the sample_grid
  aggregation_map <- aggregation_map + geom_polygon(data = sample_grid, aes(x = long, y = lat, group = group, fill = NA, colour = "Aggregation Grid"), show.legend = TRUE)
  aggregation_map <- aggregation_map + scale_fill_manual(values = c("1" = "#FFCC66"), breaks = "") +
    theme(panel.background = element_rect(fill="lightgrey"),
          legend.key =  element_rect(colour = NA, fill = NA))
  # adding customized x and y axis label
  # defining breaks
  brk_x <- unique(poly.data$X.cor)
  brk_x <- brk_x[order(brk_x)]
  brk_y <- unique(poly.data$Y.cor)
  brk_y <- brk_y[order(brk_y, decreasing = TRUE)]
  
  # input table
  raw_difa_tab.init <- read.csv(paste0(quesb_folder, "/QUES-B_DIFA_Table_", location, "_", T1, ".csv"), stringsAsFactors = FALSE)
  raw_difa_tab.init <- raw_difa_tab.init[,c("ID.x", "ID.y")]
  raw_difa_tab.final <- read.csv(paste0(quesb_folder, "/QUES-B_DIFA_Table_", location, "_", T2, ".csv"), stringsAsFactors = FALSE)
  raw_difa_tab.final <- raw_difa_tab.final[,c("ID.x", "ID.y")]
  # 
  c_grid <- data.frame(unique(rbind(raw_difa_tab.init, raw_difa_tab.final)), stringsAsFactors = FALSE)
  # application of different text color for column and row ids which is not used
  # x
  x_tab <- data.frame(ID.x = unique(poly.data[order(poly.data$X.cor), "ID.x"]), stringsAsFactors = FALSE)
  x_tab$ID <- seq(nrow(x_tab))
  # x_tab <- data.frame(ID.x = unique(poly.data$ID.x), stringsAsFactors = FALSE)
  # attempt to write the alternative
  x_tab$colour <- grey(0.7)
  x_tab[x_tab$ID.x %in% c_grid$ID.x, "colour"] <- grey(0.2)
  # y
  y_tab <- data.frame(ID.y = unique(poly.data[order(poly.data$Y.cor, decreasing = TRUE), "ID.y"]), stringsAsFactors = FALSE)
  y_tab$ID <- seq(nrow(y_tab))
  # attempt to write the alternative
  y_tab$colour <- grey(0.7)
  y_tab[y_tab$ID.y %in% c_grid$ID.y, "colour"] <- grey(0.2)
  # apply the customized breaks and labels
  aggregation_map <- aggregation_map + scale_x_continuous(breaks = brk_x, labels = x_tab$ID.x) + scale_y_continuous(breaks = brk_y, labels = y_tab$ID.y)
  # apply text colour
  aggregation_map <- aggregation_map + theme(axis.text.x = element_text(color = x_tab$colour, size = 5), axis.text.y = element_text(color = y_tab$colour, size = 6.5),
                                             axis.ticks.x = element_line(color = x_tab$colour), axis.ticks.y = element_line(color = y_tab$colour))
  # aggregation_map <- aggregation_map + geom_text(data = c_grid, aes(x = X.cor, y = Y.cor, label = ID.grid), size = 2)
  aggregation_map <- aggregation_map + guides(colour = guide_legend(override.aes = list(fill = NA)))
  aggregation_map <- aggregation_map + labs(colour = "Legenda") + theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
                                                                         legend.title = element_text(face = "bold"),
                                                                         legend.key.size = unit(0.5, "cm"),
                                                                         plot.margin=unit(c(0,0,0,0), "cm"),
                                                                         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                                                                         legend.key.width = unit(0.6, "cm")) + coord_equal()
  return(aggregation_map)
}

#Looping for each ecosystem====
for(p in 1:length(fa_class)){
  result_dir=paste0(quesb_dir, '/', fa_class[p])
  lookupTableHabitat<-read.table(habitat_table, header=T, sep=",", stringsAsFactors=FALSE)
  cl_desc<-read.table(classdesc, header=TRUE, sep=",", stringsAsFactors=FALSE)
  # descriptor table adjustment
  cl_desc$Enabled <- FALSE
  cl_desc[cl_desc$Name == names(fa_class[p]),"Enabled"] <- TRUE
  # habitat table adjustment
  lookupTableHabitat$BIODIV <- 0 
  lookupTableHabitat[lookupTableHabitat$CLASS == names(fa_class[p]),"BIODIV"] <- 1
  # saving all of the adjustments made
  write.csv(cl_desc, classdesc, row.names = FALSE, quote = FALSE) # not using quotation to prevent error during fragstats execution
  write.csv(lookupTableHabitat, habitat_table, row.names = FALSE, quote = FALSE)
  #Looping for each period===
  dir.create(result_dir)
  setwd(result_dir)
  for(k in 1:(length(bperiod)-1)){
    quesb_folder<-paste(result_dir,"/QuES-B_analysis_", fa_class[p], "_" , as.character(pd_1), "_", as.character(pd_2),sep="")
    dir.create(quesb_folder)
    #====I Sampling grid raster preparation====
    #Initial raster sampling grid
    sample_grid <- generate_sampling_grid()
    #====J DEFINING FOCAL AREA====
    #Initial focal area
    eval(parse(text=paste0("freqlanduse_t",k," <-lut_lc",k,"[lut_lc", k, "$ID != raster.nodata, c('ID', 'COUNT')]")))
    eval(parse(text=paste0("names(freqlanduse_t",k,") <- c('value', 'count')")))
    eval(parse(text=paste0("freqlanduse_t",k+1," <-lut_lc",k+1,"[lut_lc", k+1, "$ID != raster.nodata, c('ID', 'COUNT')]")))
    eval(parse(text=paste0("names(freqlanduse_t",k+1,") <- c('value', 'count')")))
    
    foc.area.init<-focal.area(paste0("landuse_t",k))
    #Final focal area
    foc.area.final<-focal.area(paste0("landuse_t",k+1))
    
    # define the pristine state focal area
    totarea <- pr_farea(names(fa_class[p]))
    
    #Initial focal area fraction inside sampling grid
    tothab.init<-foc.area.grid.sampled(foc.area.init, sample_grid)
    
    #Final focal area fraction inside sampling grid
    tothab.final<-foc.area.grid.sampled(foc.area.final, sample_grid)
    
    #====K Fragstats .fca Preparation and Execution====
    # Disconnect db connection  
    dbDisconnect(DB)
    #Execute TECI T1
    mwfile.init<-teci.analysis(landuse_t1, eval(parse(text=paste0("lu",k,"_path"))))
    # [1] "1 no previous raster file found, algorithm continue..."
    # [1] "2 no previous raster file found, algorithm continue..."
    # [1] "3 no previous raster file found, algorithm continue..."
    # [1] "Fragstats' model found!"
    # Fragstats v4.2.1.603 [built on 2015-01-23]
    # Copyright 2015 Kevin Mcgarigal & Eduard Ene
    # Loading model:D:/IDH//teciuf.fca
    # Checking model consistency
    # Model consistency check: OK
    # Starting run 1.
    # Analyzing file: C:/IDH/new/Raster/lc1990_sumsel_v2.tif
    # Invalid class descriptors header
    # Table files require signature.
    # The metric selection require the Edge Constrast table but the table is not loaded.
    # Analysis failed for file: C:/IDH/new/Raster/lc1990_sumsel_v2.tif
    # Run 1 ended.
    # Run completed in 0h : 0m : 0.796000s, please review the results.
    #Execute TECI T2
    mwfile.final<-teci.analysis(landuse_t2, eval(parse(text=paste0("lu",k+1,"_path"))))
    # reconnect with the postgresql database
    DB <- dbConnect(
      driver, dbname=project, host='localhost', port=5433,
      user='postgres', password='root'
    )
    #dbGetStatement(ll)
    
    #dbHasCompleted(ll)
    
    #End of Fragstats TECI moving window analysis
    
    #====L Write Raster TECI & Focal area File==== 
    setwd(quesb_folder)
    #save TECI file function
    
    #save TECI file
    saveTECI(mwfile.init, location, as.character(pd_1))
    saveTECI(mwfile.final, location, as.character(pd_2))
    
    saveFocal(foc.area.init, location, as.character(pd_1))
    saveFocal(foc.area.final, location, as.character(pd_2))
    
    difa.table.init<-generateDIFAtable(mwfile.init, tothab.init, location, as.character(pd_1), sampling.grid = sample_grid)
    difa.table.final<-generateDIFAtable(mwfile.final, tothab.final, location, as.character(pd_2), sampling.grid = sample_grid)
    
    #DIFA Chart Initial
    difa.init<-ggplot(difa.table.init, aes(x =difa.table.init$teci, y =difa.table.init$Cum.Sum, xend=100, yend=100)) +
      geom_area() + ggtitle(paste(location, as.character(pd_1))) +
      labs(x = "Sorted TECI value (%)", y='Cumulative Proportion of Focal Areas (%)')
    
    #DIFA Chart Final
    difa.final<-ggplot(difa.table.final, aes(x =difa.table.final$teci, y =difa.table.final$Cum.Sum, xend=100, yend=100)) +
      geom_area() + ggtitle(paste(location, as.character(pd_2))) +
      labs(x = "Sorted TECI value (%)", y='Cumulative Proportion of Focal Areas (%)')
    
    #Calculate area under the curve
    AUC.init = round((trapz(na.omit(difa.table.init$teci),difa.table.init$Cum.Sum))/100,digits=2)
    AUC.final = round((trapz(na.omit(difa.table.final$teci),difa.table.final$Cum.Sum))/100,digits=2)
    
    #====N Zonal statistics on QUES-B====
    zstat.init<-ZonalStat(mwfile.init, zone, FUN = "all")
    zstat.init[3]<-NULL
    zstat.init[3]<-NULL
    zstat.init[3]<-NULL
    
    zstat.final<-ZonalStat(mwfile.final, zone, FUN = "all")
    zstat.final[3]<-NULL
    zstat.final[3]<-NULL
    zstat.final[3]<-NULL
    
    #====O SDM Tools Landscape metrics =====
    #SDM Tools fragstats; mean patch area calculation; patch number calculation
    foc.area.stats.init<- ClassStat(foc.area.init,bkgd=0, cellsize=(res(foc.area.init)[1]/100))
    foc.area.stats.init<-t(as.data.frame(foc.area.stats.init))
    foc.area.stats.init<-round(foc.area.stats.init[,1], digits=2)
    
    foc.area.stats.final<- ClassStat(foc.area.final,bkgd=0, cellsize=(res(foc.area.final)[1]/100))
    foc.area.stats.final<-t(as.data.frame(foc.area.stats.final))
    foc.area.stats.final<-round(foc.area.stats.final[,1], digits=2)
    
    #Combine class STATS
    foc.area.stats<-cbind(foc.area.stats.init,foc.area.stats.final)
    foc.area.stats.temp1<-foc.area.stats[2:4,c('foc.area.stats.init','foc.area.stats.final')]
    total.edge<-(foc.area.stats[6:6,c('foc.area.stats.init','foc.area.stats.final')]*100)
    foc.area.stats.temp3<-(foc.area.stats[10:13,c('foc.area.stats.init','foc.area.stats.final')])
    foc.area.stats<-rbind(foc.area.stats.temp1,total.edge,foc.area.stats.temp3)
    rm(foc.area.stats.temp1,total.edge,foc.area.stats.temp3)
    
    col.init<-paste('class.stats.', as.character(pd_1), sep='')
    colnames(foc.area.stats)<-c(paste('class.stats.', as.character(pd_1), sep=''),paste('class.stats.', as.character(pd_2), sep=''))
    foc.area.stats.filename<-paste("Focal_area_class_metrics",location,'_', as.character(pd_1),'_', as.character(pd_2),'.csv', sep='')
    write.csv(foc.area.stats, foc.area.stats.filename, row.names=TRUE)
    
    #Focal area decrement and increment
    chk_loss<-foc.area.init>foc.area.final
    chk_gain<-foc.area.init<foc.area.final
    foc.area.loss<-(foc.area.init-foc.area.final)*chk_loss # integration increase
    foc.area.gain<-(foc.area.final-foc.area.init)*chk_gain # integration decrease
    
    if (maxValue(foc.area.gain)==0 & minValue(foc.area.gain)==0){ 
      print(paste("NO FOCAL AREA RECOVERED"))
    } else {
      foc.area.gain
    }
    
    #Habitat loss (TECI increment) and Habitat recovery (decrement) except nodata
    mwfile.init.chk<-mwfile.init
    mwfile.init.chk0<-reclassify(mwfile.init.chk, cbind(NA,0))
    mwfile.final.chk<-mwfile.final
    mwfile.final.chk0<-reclassify(mwfile.final.chk, cbind(NA,0))
    
    chk_teci_decrement<-mwfile.init.chk>mwfile.final.chk
    chk_teci_decrement <- reclassify(chk_teci_decrement, cbind(NA,0))
    chk_teci_increment<-mwfile.final.chk>mwfile.init.chk
    chk_teci_increment <- reclassify(chk_teci_increment, cbind(NA,0))
    habitat.recovery<-(mwfile.init.chk0-mwfile.final.chk0)*chk_teci_decrement*(foc.area.init==0) # TECI value decrement
    #habitat.recovery1<-(mwfile.init.chk0-mwfile.final.chk0)*chk_teci_decrement*(foc.area.init) # TECI value decrement
    habitat.degradation<-(mwfile.final.chk0-mwfile.init.chk0)*chk_teci_increment*(foc.area.final>=0) # TECI value increment
    #TECI loss and gain in NA data
    mwfile.init.NA <- is.na(mwfile.init.chk)
    mwfile.init.NA[mwfile.init.NA == FALSE] <- NA
    
    mwfile.final.NA <- is.na(mwfile.final.chk)
    mwfile.final.NA[mwfile.final.NA == FALSE] <- NA
    
    #Habitat gain and recovery
    habitat.gain.NA<-mwfile.final*mwfile.init.NA;#TECI gain in NA area
    #habitat.gain.NA<- reclassify(habitat.gain.NA, cbind(0, NA))
    habitat.gain.NA<-habitat.gain.NA*(foc.area.final>=0)
    #habitat.gain.recovery<-mosaic(habitat.recovery, habitat.gain.NA, fun="max")
    
    #Habitat loss and degradation
    habitat.loss.NA<-mwfile.init*mwfile.final.NA;#TECI loss in NA area
    #habitat.loss.NA<- reclassify(habitat.loss.NA, cbind(0, NA))
    habitat.loss.NA<- habitat.loss.NA*(foc.area.final>=0)
    #habitat.loss.degradation<-mosaic(habitat.degradation, habitat.loss.NA,
    # mwfile.init.NA <- reclassify(mwfile.init.chk, cbind(NA, 999))
    # mwfile.init.NA<-((mwfile.init.NA/999)==1)
    # 
    # mwfile.final.NA <- reclassify(mwfile.final.chk, cbind(NA, 999))
    # mwfile.final.NA<-((mwfile.final.NA/999)==1)
    # 
    #Habitat gain and recovery
    # habitat.gain.NA<-mwfile.final.chk0*mwfile.init.NA;#TECI gain in NA area
    #habitat.gain.NA<- reclassify(habitat.gain.NA, cbind(0, NA))
    # habitat.gain.NA<-habitat.gain.NA*(foc.area.final>=0)
    #habitat.gain.recovery<-mosaic(habitat.recovery, habitat.gain.NA, fun="max")
    # 
    #Habitat loss and degradation
    # habitat.loss.NA<-mwfile.init.chk0*mwfile.final.NA;#TECI loss in NA area
    #habitat.loss.NA<- reclassify(habitat.loss.NA, cbind(0, NA))
    # habitat.loss.NA<- habitat.loss.NA*(foc.area.final>=0)
    #habitat.loss.degradation<-mosaic(habitat.degradation, habitat.loss.NA, fun="max") ADverified
    
    if (maxValue(chk_loss)>0){
      foc.area.loss<-chk_loss*landuse_t2
      foc.area.loss <- reclassify(foc.area.loss, cbind(0, NA))
      foc.area.loss.att<-na.omit(as.data.frame(freq(foc.area.loss)))
      foc.area.loss.att$prop<-(foc.area.loss.att$count/sum(foc.area.loss.att$count))*100
      
      colnames(foc.area.loss.att)[1]<-c("ID")
      foc.area.loss.att<-merge(lookupTableHabitat, foc.area.loss.att, by="ID")
      foc.area.loss.att$BIODIV<-NULL
      colnames(foc.area.loss.att)<-c("ID", "LULC", "Area", "Proportion_of_loss")
      foc.area.loss.att$Area<-foc.area.loss.att$Area*Spat_res
      foc.area.loss.att<-arrange(foc.area.loss.att, -Proportion_of_loss)
      foc.area.loss.att$Proportion_of_loss<-round(foc.area.loss.att$Proportion_of_loss, digits=2)
      foc.area.loss.att.filename<-paste("Focal_area_loss_source_",location,'_', as.character(pd_1),'_', as.character(pd_2),'.dbf', sep='')
      write.dbf(foc.area.loss.att, foc.area.loss.att.filename)
    } else { 
      print("No focal area loss found")
    }
    
    if (maxValue(chk_gain)>0) {
      foc.area.gain<-chk_gain*landuse_t1
      foc.area.gain <- reclassify(foc.area.gain, cbind(0, NA))
      foc.area.gain.att<-na.omit(as.data.frame(freq(foc.area.gain)))
      foc.area.gain.att$prop<-(foc.area.gain.att$count/sum(foc.area.gain.att$count))*100
      
      colnames(foc.area.gain.att)[1]<-c("ID")
      foc.area.gain.att<-merge(lookupTableHabitat, foc.area.gain.att, by="ID")
      foc.area.gain.att$BIODIV<-NULL
      colnames(foc.area.gain.att)<-c("ID", "LULC", "Area", "Proportion_of_gain")
      foc.area.gain.att$Area<-foc.area.gain.att$Area*Spat_res
      foc.area.gain.att<-arrange(foc.area.gain.att, -Proportion_of_gain)
      foc.area.gain.att$Proportion_of_gain<-round(foc.area.gain.att$Proportion_of_gain, digits=2)
      foc.area.gain.att.filename<-paste("Focal_area_gain_source",location,'_', as.character(pd_1),'_', as.character(pd_2),'.dbf', sep='')
      write.dbf(foc.area.gain.att, foc.area.gain.att.filename)
    } else { 
      print("No focal area gain found")
    }
    
    #zonal stat for focal area gain/loss
    lookup_z.area<-as.data.frame(na.omit(freq(zone)))
    lookup_z.area[2]<-lookup_z.area[2]*Spat_res
    colnames(lookup_z.area)<-c('ZONE','zone.area')
    #lookup_z.area$zone.area<-lookup_z.area$zone.area*Spat_res
    #foc.area.change.map<-reclassify((foc.area.final-foc.area.init),cbind(0,NA))
    zstat.foc.area.basic<-as.data.frame(zonal((foc.area.final-foc.area.init), zone, fun='sum'))
    colnames(zstat.foc.area.basic) =c("ZONE","foc.area.change")
    zstat.foc.area.basic$foc.area.change <- zstat.foc.area.basic$foc.area.change*Spat_res
    zstat.foc.area<-zstat.foc.area.basic
    
    
    zstat.foc.area<-merge(lookup_z[,c(1,3)],zstat.foc.area,by.x='ID', by.y = "ZONE")
    names(zstat.foc.area)[1] <- "ZONE"
    zstat.foc.area<-merge(zstat.foc.area,lookup_z.area,by='ZONE')
    zstat.foc.area$change.proportion<-round((zstat.foc.area$foc.area.change/zstat.foc.area$zone.area)*100, digits=2) # relative to the total zone area
    zstat.foc.area<-arrange(zstat.foc.area, foc.area.change)
    
    #====R zonal stat for habitat recovery and degradation ====
    #important variables below:
    # foc.area.gain.att
    # foc.area.loss.att
    # zstat.foc.area
    # zstat.habitat.degradation
    # zstat.habitat.recovery
    tryCatch({
      habitat.recovery.0<-reclassify(habitat.recovery, cbind(NA, 0))
      zstat.habitat.recovery<-ZonalStat(habitat.recovery.0, zone, FUN = "all")
      colnames(zstat.habitat.recovery)[1] ="ZONE"
      zstat.habitat.recovery<-merge(lookup_z[,c(1,3)], zstat.habitat.recovery, by.x = "ID", by.y = "ZONE")
      names(zstat.habitat.recovery)[1] = "ZONE"
      zstat.habitat.recovery[4]<-NULL
      zstat.habitat.recovery[4]<-NULL
      zstat.habitat.recovery[4]<-NULL
      zstat.habitat.recovery[7]<-NULL
      zstat.habitat.recovery$max<-round(zstat.habitat.recovery$max, digits=2)
      zstat.habitat.recovery$min<-round(zstat.habitat.recovery$min, digits=2)
      zstat.habitat.recovery$mean<-round(zstat.habitat.recovery$mean, digits=2)
      zstat.habitat.recovery$sd<-round(zstat.habitat.recovery$sd, digits=2)
      zstat.habitat.recovery<-merge(zstat.habitat.recovery, zstat.foc.area.basic, by="ZONE")
      zstat.habitat.recovery$norm.mean<-zstat.habitat.recovery$mean/abs(zstat.habitat.recovery$foc.area)
      zstat.habitat.recovery$norm.mean<-round(zstat.habitat.recovery$norm.mean, digits=3)
      zstat.habitat.recovery<-arrange(zstat.habitat.recovery, -norm.mean)
    }, error=function(e){cat("Skipping zonal stats on habitat recovery:",conditionMessage(e), "\n")})
    
    tryCatch({
      habitat.degradation.0<-reclassify(habitat.degradation, cbind(NA, 0))
      zstat.habitat.degradation<-ZonalStat(habitat.degradation.0, zone, FUN = "all")
      colnames(zstat.habitat.degradation)[1] ="ZONE"
      zstat.habitat.degradation<-merge(lookup_z[,c(1,3)], zstat.habitat.degradation, by.x = "ID", by.y = "ZONE")
      names(zstat.habitat.degradation)[1] = "ZONE"
      zstat.habitat.degradation[4]<-NULL
      zstat.habitat.degradation[4]<-NULL
      zstat.habitat.degradation[4]<-NULL
      zstat.habitat.degradation[7]<-NULL
      zstat.habitat.degradation$max<-round(zstat.habitat.degradation$max, digits=2)
      zstat.habitat.degradation$min<-round(zstat.habitat.degradation$min, digits=2)
      zstat.habitat.degradation$mean<-round(zstat.habitat.degradation$mean, digits=2)
      zstat.habitat.degradation$sd<-round(zstat.habitat.degradation$sd, digits=2)
      zstat.habitat.degradation<-merge(zstat.habitat.degradation, zstat.foc.area.basic, by="ZONE")
      zstat.habitat.degradation$norm.mean<-zstat.habitat.degradation$mean/abs(zstat.habitat.degradation$foc.area)
      zstat.habitat.degradation$norm.mean<-round(zstat.habitat.degradation$norm.mean, digits=3)
      zstat.habitat.degradation<-arrange(zstat.habitat.degradation, -norm.mean)
    }, error=function(e){cat("Skipping zonal stats on habitat gain:",conditionMessage(e), "\n")})
    
    #write zonal stats table
    tryCatch({
      zstat.gain.recover.filename<-paste("Habitat_recovery_zonal_stat_",location,'_', as.character(pd_1),'_', as.character(pd_2),'.dbf', sep='')
      write.dbf(zstat.habitat.recovery, zstat.gain.recover.filename)
    }, error=function(e){cat("Skipping zonal stats table export process:",conditionMessage(e), "\n")})
    
    tryCatch({
      zstat.loss.degradation.filename<-paste("Habitat_degradation_zonal_stat_",location,'_', as.character(pd_1),'_', as.character(pd_2),'.dbf', sep='')
      write.dbf(zstat.habitat.degradation, zstat.loss.degradation.filename)
    }, error=function(e){cat("Skipping zonal stats table export process:",conditionMessage(e), "\n")})
    
    #ADHERE====
    #backround map
    background<-landuse_t1/landuse_t1
    #Create Map for report
    myColors1 <- brewer.pal(9,"Set1")
    myColors2 <- brewer.pal(8,"Accent")
    myColors3 <- brewer.pal(12,"Paired")
    myColors4 <- brewer.pal(9, "Pastel1")
    myColors5 <- brewer.pal(8, "Set2")
    myColors6 <- brewer.pal(8, "Dark2")
    myColors7 <- rev(brewer.pal(11, "RdYlGn"))
    myColors8 <- "#000000"
    myColors9 <- brewer.pal(12, "Set3")
    
    myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
    
    # Generate the plot to show the TECI and focal area dynamics====
    teci_fa_dyn.plot <- TECI_chg(pd_1, pd_2)
    ag_map <- grid_map(pd_1, pd_2)
    
    #Landuse 1 map
    area_lc1<-as.data.frame(freq(landuse_t1))
    colnames(area_lc1)[1]<-'ID'
    area_lc1<-merge(area_lc1, cl_desc, by='ID')
    colnames(area_lc1)[3]<-'CLASS_LC1'
    area_lc1[4]<-NULL
    # id_length<-max(length(unique(area_lc1$ID)), length(unique(area_lc2$ID)))
    # myColors.lu <- myColors[1:id_length]
    # ColScale.lu<-scale_fill_manual(name="Land Use Class", breaks=area_lc1$ID, labels=area_lc1$CLASS_LC1, values=myColors.lu)
    # plot.LU1<-gplot(landuse_t1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
    #   coord_equal() + ColScale.lu +
    #   theme(plot.title = element_text(lineheight= 5, face="bold")) +
    #   theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
    #          panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    #          legend.title = element_text(size=8),
    #          legend.text = element_text(size = 6),
    #          legend.key.height = unit(0.25, "cm"),
    #          legend.key.width = unit(0.25, "cm"))
    
    #Landuse 2 map
    area_lc2<-as.data.frame(freq(landuse_t2))
    colnames(area_lc2)[1]<-'ID'
    area_lc2<-merge(area_lc2, cl_desc, by='ID')
    colnames(area_lc2)[3]<-'CLASS_LC2'
    area_lc2[4]<-NULL
    id_length<-nrow(lulc_lut)
    myColors.lu <- myColors[1:id_length]
    ColScale.lu<-scale_fill_manual(name="Land Use Class", breaks=unique(lulc_lut$ID), labels=lulc_lut$Legend, values=myColors.lu)
    plot.LU1<-gplot(landuse_t1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
      coord_equal() + ColScale.lu +
      theme(plot.title = element_text(lineheight= 5, face="bold")) +
      theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title = element_text(size=8),
             legend.text = element_text(size = 6),
             legend.key.height = unit(0.25, "cm"),
             legend.key.width = unit(0.25, "cm"))
    
    #ColScale.lu<-scale_fill_manual(name="Land Use Class", breaks=area_lc2$ID, labels=area_lc2$CLASS_LC2, values=myColors.lu)
    plot.LU2<-gplot(landuse_t2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
      coord_equal() + ColScale.lu +
      theme(plot.title = element_text(lineheight= 5, face="bold")) +
      theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title = element_text(size=8),
             legend.text = element_text(size = 6),
             legend.key.height = unit(0.25, "cm"),
             legend.key.width = unit(0.25, "cm"))
    
    myColors  <-c(myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors6)
    
    #zone map
    area_zone<-lookup_z
    colnames(area_zone)[1]<-'ID'
    colnames(area_zone)[3]<-'ZONE'
    myColors.Z <- myColors[1:length(unique(area_zone$ID))]
    ColScale.Z<-scale_fill_manual(name="Zone Class", breaks=area_zone$ID, labels=area_zone$ZONE, values=myColors.Z)
    plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
      coord_equal() + ColScale.Z +
      theme(plot.title = element_text(lineheight= 5, face="bold")) +
      theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title = element_text(size=8),
             legend.text = element_text(size = 6),
             legend.key.height = unit(0.25, "cm"),
             legend.key.width = unit(0.25, "cm"))
    
    #Focal Area Change: plot.FAC.loss and plot.FAC.gain
    lookup_change<-as.data.frame(cbind(0,NA))
    lookup_change<-rbind(lookup_change, cbind(1,2))
    
    if(maxValue(chk_loss)>0) {
      foc.area.loss.reclass<- reclassify(chk_loss, lookup_change)
      foc.area.loss<-mosaic(foc.area.init, foc.area.loss.reclass, fun="max")
      ID<-as.data.frame(levels(ratify(foc.area.loss)));# or as.data.frame(na.omit(freq(foc.area.loss)))
      Label<-c("Non Focal Area", "Stable Focal Area", "Focal Area loss")
      FAC<-as.data.frame(cbind(ID, Label))
      ColScale.FAC1 <-scale_fill_manual(name="Area Class", breaks=FAC$ID, labels=FAC$Label, values= c("#FFCC66", "#597814","#862D63"))
      plot.FAC.loss<-gplot(foc.area.loss, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
        coord_equal() + ColScale.FAC1 +
        theme(plot.title = element_text(lineheight= 5, face="bold")) +
        theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
               panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
               legend.title = element_text(size=8),
               legend.text = element_text(size = 6),
               legend.key.height = unit(0.25, "cm"),
               legend.key.width = unit(0.25, "cm"))
    } else {
      print("No Habitat loss found")
    }
    
    if(maxValue(chk_gain)>0) {
      foc.area.gain.reclass<- reclassify(chk_gain, lookup_change)
      foc.area.gain<-mosaic(foc.area.init, foc.area.gain.reclass, fun="max")
      ID<-as.data.frame(levels(ratify(foc.area.gain)));# or as.data.frame(na.omit(freq(foc.area.gain)))
      Label<-c("Non Focal Area", "Stable Focal Area", "Focal Area gain")
      FAC<-as.data.frame(cbind(ID, Label))
      ColScale.FAC2<-scale_fill_manual(name="Area Class", breaks=FAC$ID, labels=FAC$Label, values= c("#FFCC66", "#597814","#2C4770"))
      plot.FAC.gain<-gplot(foc.area.gain, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
        coord_equal() + ColScale.FAC2 +
        theme(plot.title = element_text(lineheight= 5, face="bold")) +
        theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
               panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
               legend.title = element_text(size=8),
               legend.text = element_text(size = 6),
               legend.key.height = unit(0.25, "cm"),
               legend.key.width = unit(0.25, "cm"))
    } else {
      print("No Habitat gain found")
    }
    # ADHERE
    #====S Subsequent Land Use: Habitat Degradation, Recovey, Loss, Gain=====
    # conditional testing for assessing the impact of the land use change in habitat conditions
    # checking the availability of the change analysis in the database
    chg_db <- tolower(paste0("xtab_", planning_unit, pd_1, pd_2))
    if(chg_db  %in% list_of_data_lut$TBL_NAME){
      # loading the lulc change cross table from postgre database as 'lu_db' dataframe
      chg_db_data <- list_of_data_lut[list_of_data_lut$TBL_NAME == chg_db, "TBL_DATA"]
      lu_db <- dbReadTable(DB, c("public", chg_db_data))
      # loading the associated raster file as 'lu_chg' raster
      lu_chg <- getRasterFromPG(pgconf, project, list_of_data_f[list_of_data_f$RST_NAME == gsub("xtab", "chgmap", chg_db), "RST_DATA"], paste0(gsub("xtab", "chgmap", chg_db), ".tif"))
      lu_db <- merge(lu_db,lookup_z[,c(1,3)], by.x= 'ZONE', by.y ='ID', all.x = TRUE)
      # lu_db <- lu_db[,c('ID_Z', 'ID_LC_T2', 'ID_LC_T1', 'COUNT', 'LC_T1', 'LC_T2','Z_NAME')]
      # merging the change data.frame with the class descriptors of land cover ID
      lu_db <- merge(lu_db, cl_desc[,c("ID", "Name")], by.x = "ID_LC1", by.y = "ID", all.x = TRUE)
      lu_db <- merge(lu_db, cl_desc[,c("ID", "Name")], by.x = "ID_LC2", by.y = "ID", all.x = TRUE)
      names(lu_db)[c(ncol(lu_db)-1, ncol(lu_db))] <- c("LC_t1", "LC_t2")
      lu_db <- lu_db[,c("ZONE", "ID_LC1", "ID_LC2", "ID_CHG", "COUNT", "LC_t1", "LC_t2", "Legend")]
      names(lu_db) <- c("ID_Z", 'ID_LC1','ID_LC2', 'ID', 'COUNT', 'LC_t1', 'LC_t2', 'Z_NAME') # ID_CHG into ID to match the routine of the process subsequent.changes
      # lu_db$ID <- as.numeric(as.character(lu_db$ID_LC2))*10^6 + as.numeric(as.character(lu_db$ID_LC1))*10^3 + as.numeric(as.character(lu_db$ID_Z))
      #Landscape level Habitat Degradation
      luchg.db.degrad<-subsequent.changes(habitat.degradation,lu_db, "Degradation", location, as.character(pd_1), as.character(pd_2))
      #Landscape level Habitat Recovery
      luchg.db.recovery<-subsequent.changes(habitat.recovery,lu_db, "Recovery", location, as.character(pd_1), as.character(pd_2))
      
      #top 10 habitat degradation
      luchg.degradation.10<-luchg.db.degrad[1:10,]
      
      #Landscape level Habitat Loss
      luchg.db.loss<-subsequent.changes(habitat.loss.NA,lu_db, "Loss", location, as.character(pd_1), as.character(pd_2))
      
      #Zonal Stat Habitat LOSS
      luchg.db.loss.zstat<-zonal_stat(habitat.loss.NA, zone, lookup_z)
      
      tryCatch({
        luchg.db.gain<-subsequent.changes(habitat.gain.NA,lu_db, "Gain", location, as.character(pd_1), as.character(pd_2))
      }, error=function(e){cat("Skipping habitat gain analysis:",conditionMessage(e), "\n")})
    }
    # preprocessing the mapping process
    # 1. polygonize the background raster
    bg_poly <- gdal_polygonizeR(background)
    # Defining the maximum colour scale----
    # the scale would be used across the plots of TECI
    maxval <- ceiling(max(c(values(mwfile.init), values(mwfile.final)), na.rm = TRUE))
    #====Plot Habitat extent Map t1====
    plot.mw.init <- gplot(mwfile.init)
    plot.mw.init <- plot.mw.init + geom_polygon(data = bg_poly, aes(x = long, y = lat, group = group), fill="#FFCC66", show.legend = FALSE) +
      geom_raster(aes(fill=value)) + scale_fill_gradient(low = "#62D849", high="#6B54D3", guide="colourbar", limits=c(0,maxval), na.value = NA)
    plot.mw.init<- plot.mw.init + coord_equal() + labs(fill = "TECI") +theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
                                                                              panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                                                                              legend.title = element_text(size=8, face = "bold"),
                                                                              legend.text = element_text(size = 8),
                                                                              legend.key.height = unit(0.375, "cm"),
                                                                              legend.key.width = unit(0.375, "cm")
    )
    
    #====Plot Habitat extent Map t2====
    plot.mw.fin <- gplot(mwfile.final)
    plot.mw.fin <- plot.mw.fin + geom_polygon(data = bg_poly, aes(x = long, y = lat, group = group), fill="#FFCC66", show.legend = FALSE) +
      geom_raster(aes(fill=value)) + scale_fill_gradient(low = "#62D849", high="#6B54D3", guide="colourbar", limits=c(0,maxval), na.value = NA)
    plot.mw.fin<- plot.mw.fin + coord_equal() + labs(fill = "TECI") +theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
                                                                            panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                                                                            legend.title = element_text(size=8, face = "bold"),
                                                                            legend.text = element_text(size = 8),
                                                                            legend.key.height = unit(0.375, "cm"),
                                                                            legend.key.width = unit(0.375, "cm")
    )
    #====Plot Habitat loss and degradation====
    tryCatch({
      # maxval<-ceiling(maxValue(habitat.degradation)/10)
      # maxval<-maxval*10
      habitat.degradation[habitat.degradation==0] <- NA
      plot.HD <- gplot(habitat.degradation)
      plot.HD <- plot.HD + geom_polygon(data = bg_poly, aes(x = long, y = lat, group = group), fill="#FFCC66", show.legend = FALSE) +
        geom_raster(aes(fill=value)) + scale_fill_gradient(low = "#62D849", high="#6B54D3", limits=c(0,maxval), guide="colourbar", na.value = NA)
      plot.HD <- plot.HD + labs(fill = "TECI") + coord_equal() + theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
                                                                        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                                                                        legend.title = element_text(size=8, face = "bold"),
                                                                        legend.text = element_text(size = 8),
                                                                        legend.key.height = unit(0.375, "cm"),
                                                                        legend.key.width = unit(0.375, "cm"))
    }, error=function(e){cat("skipping habitat degradation plot :",conditionMessage(e), "\n")})
    
    # Habitat loss lateron
    tryCatch({
      # maxval<-ceiling(maxValue(habitat.loss.NA)/10)
      # maxval<-maxval*10
      plot.HL <- gplot(habitat.loss.NA)
      plot.HL <- plot.HL + geom_polygon(data = bg_poly, aes(x = long, y = lat, group = group), fill="#FFCC66", show.legend = FALSE) +
        geom_raster(aes(fill=value)) + scale_fill_gradient(low = "#62D849", high="#6B54D3", limits=c(0,maxval), guide="colourbar", na.value = NA)
      plot.HL <- plot.HL + labs(fill = "TECI") + coord_equal() + theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
                                                                        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                                                                        legend.title = element_text(size=8, face = "bold"),
                                                                        legend.text = element_text(size = 8),
                                                                        legend.key.height = unit(0.375, "cm"),
                                                                        legend.key.width = unit(0.375, "cm"))
    }, error=function(e){cat("skipping habitat loss plot :",conditionMessage(e), "\n")})
    
    #====Plot Habitat gain and recovery ====
    tryCatch({
      # maxval<-ceiling(maxValue(habitat.recovery)/10)
      # maxval<-maxval*10
      habitat.recovery[habitat.recovery==0] <- NA
      plot.HR <- gplot(habitat.recovery)
      plot.HR <- plot.HR + geom_polygon(data = bg_poly, aes(x = long, y = lat, group = group), fill="#FFCC66", show.legend = FALSE) +
        geom_raster(aes(fill=value)) + scale_fill_gradient(low = "#62D849", high="#6B54D3", limits=c(0,maxval), guide="colourbar", na.value = NA)
      plot.HR <- plot.HR + labs(fill = "TECI") + coord_equal() + theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
                                                                        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                                                                        legend.title = element_text(size=8, face = "bold"),
                                                                        legend.text = element_text(size = 8),
                                                                        legend.key.height = unit(0.375, "cm"),
                                                                        legend.key.width = unit(0.375, "cm"))
    }, error=function(e){cat("skipping habitat recovery plot :",conditionMessage(e), "\n")})
    
    tryCatch({
      # maxval<-ceiling(maxValue(habitat.gain.NA)/10)
      # maxval<-maxval*10
      plot.HG <- gplot(habitat.gain.NA)
      plot.HG <- plot.HG + geom_polygon(data = bg_poly, aes(x = long, y = lat, group = group), fill="#FFCC66", show.legend = FALSE) +
        geom_raster(aes(fill=value)) + scale_fill_gradient(low = "#62D849", high="#6B54D3", limits=c(0,maxval), guide="colourbar", na.value = NA)
      plot.HG <- plot.HG + labs(fill = "TECI") + coord_equal() + theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
                                                                        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                                                                        legend.title = element_text(size=8, face = "bold"),
                                                                        legend.text = element_text(size = 8),
                                                                        legend.key.height = unit(0.375, "cm"),
                                                                        legend.key.width = unit(0.375, "cm"))
    }, error=function(e){cat("skipping habitat gain plot :",conditionMessage(e), "\n")})
    #====Plot Habitat loss and degradation====
    #plot.background<-gplot(background, maxpixels=100000) + geom_raster(aes(fill=as.factor(value)))
    # tryCatch({
    #   maxval<-ceiling(maxValue(habitat.degradation)/10)
    #   maxval<-maxval*10
    #   background[background==1]<-(-maxval)
    #   plot.hbt.loss<-merge(habitat.degradation, background, overlap=TRUE)
    #   plot.HD<-gplot(plot.hbt.loss, maxpixels=100000) + geom_raster(aes(fill=value)) +
    #     coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
    #     theme(plot.title = element_text(lineheight= 5, face="bold")) +
    #     theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
    #            panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    #            legend.title = element_text(size=8),
    #            legend.text = element_text(size = 8),
    #            legend.key.height = unit(0.375, "cm"),
    #            legend.key.width = unit(0.375, "cm"))
    #   background[background==-maxval]<-1
    # }, error=function(e){cat("skipping habitat degradation plot :",conditionMessage(e), "\n")})
    # 
    # tryCatch({
    #   maxval<-ceiling(maxValue(habitat.loss.NA>0)/10)
    #   #maxval<-maxval*10
    #   background[background==1]<-(-maxval)
    #   plot.hbt.loss<-merge(habitat.loss.NA>0, background, overlap=TRUE)
    #   plot.HL<-gplot(habitat.loss.NA>0, maxpixels=100000) + geom_raster(aes(fill=value)) +
    #     coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
    #     theme(plot.title = element_text(lineheight= 5, face="bold")) +
    #     theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
    #            panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    #            legend.title = element_text(size=8),
    #            legend.text = element_text(size = 8),
    #            legend.key.height = unit(0.375, "cm"),
    #            legend.key.width = unit(0.375, "cm"))
    #   background[background==-maxval]<-1
    # }, error=function(e){cat("skipping habitat loss plot :",conditionMessage(e), "\n")})
    # 
    #====Plot Habitat gain and recovery ====
    #plot.background<-gplot(background, maxpixels=100000) + geom_raster(aes(fill=as.factor(value)))
    # tryCatch({
    #   maxval<-ceiling(maxValue(habitat.recovery)/10)
    #   maxval<-maxval*10
    #   background[background==1]<-(-maxval)
    #   plot.hbt.gain<-merge(habitat.recovery>0, background, overlap=TRUE)
    #   plot.HR<-gplot(plot.hbt.gain, maxpixels=100000) + geom_raster(aes(fill=value)) +
    #     coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
    #     theme(plot.title = element_text(lineheight= 5, face="bold")) +
    #     theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
    #            panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    #            legend.title = element_text(size=8),
    #            legend.text = element_text(size = 8),
    #            legend.key.height = unit(0.375, "cm"),
    #            legend.key.width = unit(0.375, "cm"))
    #   background[background==-maxval]<-1
    # }, error=function(e){cat("skipping habitat recovery plot :",conditionMessage(e), "\n")})
    # 
    # tryCatch({
    #   #plot.background<-gplot(background, maxpixels=100000) + geom_raster(aes(fill=as.factor(value)))
    #   maxval<-ceiling(maxValue(habitat.gain.NA)/10)
    #   #maxval<-maxval*10
    #   background[background==1]<-(-maxval)
    #   plot.hbt.gain<-merge(habitat.gain.NA, background, overlap=TRUE)
    #   plot.HG<-gplot(plot.hbt.gain, maxpixels=100000) + geom_raster(aes(fill=value)) +
    #     coord_equal() + scale_fill_gradient2(low="#999999",mid = "#FFCC66", high="#003300",limits=c(0,maxval), guide="colourbar") +
    #     theme(plot.title = element_text(lineheight= 5, face="bold")) +
    #     theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
    #            panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    #            legend.title = element_text(size=8),
    #            legend.text = element_text(size = 8),
    #            legend.key.height = unit(0.375, "cm"),
    #            legend.key.width = unit(0.375, "cm"))
    #   background[background==-maxval]<-1
    # }, error=function(e){cat("skipping habitat gain plot :",conditionMessage(e), "\n")})
    #====Create RTF Report File====
    # title<-"\\b\\fs32 LUMENS-QUES Project Report\\b0\\fs20"
    title<-"\\b\\fs32 Laporan Proyek LUMENS-\\i QUES\\b0\\fs32"
    # sub_title<-"\\b\\fs28 Sub-modules: Biodiversity Analysis\\b0\\fs20"
    sub_title<-"\\b\\fs28 Sub-modul: Analisis Biodiversitas\\b0\\fs28"
    test<-as.character(Sys.Date())
    date<-paste("Tanggal : ", test, sep="")
    t_start<-paste("Proses dimulai : ", time_start, sep="")
    time_end<-paste("Proses selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
    line<-paste("------------------------------------------------------------------------------------------------------------------------------------------")
    area_name_rep<-paste0("\\b", "\\fs24 ", location, "\\b0","\\fs24")
    I_O_period_1_rep<-paste0("\\b","\\fs24 ", as.character(pd_1), "\\b0","\\fs24")
    I_O_period_2_rep<-paste0("\\b","\\fs24 ", as.character(pd_2), "\\b0","\\fs24")
    chapter1<-"\\b\\fs28 I. DATA INPUT\\b0\\fs28"
    chapter2<-"\\b\\fs28 II. PERUBAHAN AREA FOKAL\\b0\\fs28"
    chapter3<-"\\b\\fs28 III. DISTRIBUSI NILAI IKTT\\b0\\fs28"
    chapter4<-"\\b\\fs28 IV. DEGREE OF INTEGRATION OF FOCAL AREA (DIFA)\\b0\\fs28"
    chapter5<-"\\b\\fs28 V. DINAMIKA IKTT DAN AREA FOKAL\\b0\\fs28"
    chapter6<-"\\b\\fs28 VI. DESKRIPSI IKTT UNIT PERENCANAAN\\b0\\fs28"
    #Report 0. Cover=====
    rtffile <- RTF("LUMENS_QUES-B_report.lpr", font.size=10, width = 8.267, height = 11.692, omi = c(0,0,0,0))
    addPng(rtffile, "D:/LUMENS/finalization/report_material/Slide2.PNG", width = 8.267, height = 11.692)
    addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
    
    
    #==== Report I. DATA INPUT ====
    # rtffile <- RTF("LUMENS_QUES-B_report.lpr", font.size=10, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
    addParagraph(rtffile, title)
    addParagraph(rtffile, sub_title)
    addNewLine(rtffile)
    addParagraph(rtffile, line)
    addParagraph(rtffile, date)
    addParagraph(rtffile, t_start)
    addParagraph(rtffile, time_end)
    # addParagraph(rtffile, time_end)
    addParagraph(rtffile, line)
    # addNewLine(rtffile)
    # addParagraph(rtffile, chapter1)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Dokumen ini dihasilkan dari penjalanan modul \\i Quantification of Ecosystem Services - Biodiversity (QUES-B) \\i0 dalam perangkat lunak LUMENS. Sistematika laporan ini adalah: pengantar yang memuat prinsip dasar analisis terdapat di bagian ini, informasi mengenai data-data yang digunakan pada bagian I, sedangkan bagian II - VII memuat intisari luaran analisis modul  \\i QUES-B\\i0 . Pada setiap bagian terdapat petunjuk singkat yang disediakan demi kemudahan pengguna dalam menginterpretasi informasi dan/atau data yang ditampilkan.\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile, n = 1)
    text <- paste0("\\qj\\fs24 Analisis dalam modul \\i QUES-B \\i0 bertujuan untuk memberi gambaran mengenai aspek biodiversitas suatu daerah di tingkat lanskap. Kerangka kerja yang diadopsi mengacu pada metode penelitian yang telah dipublikasikan oleh Dewi, dkk. pada tahun 2013. Sebelum proses dijalankan, pengguna menentukan kelas-kelas tutupan lahan alami yang paling merepresentasikan habitat bagi keanekaragaman hayati asli suatu lanskap. Berdasarkan peta tutupan lahan multi waktu, dinamika kondisi tutupan alami dianalisis dalam modul \\i QUES-B \\i0 sehingga dihasilkan beberapa besaran yang mengindikasikan perubahan luas dan konfigurasi tutupan alami di suatu daerah studi. Peralihan tutupan lahan yang berkontribusi terhadap dinamika kondisi tutupan alami dan luasannya dapat diketahui berdasarkan luaran modul ini. Selain itu, informasi mengenai perubahan luas dan konfigurasi tutupan alami suatu daerah disajikan secara kuantitatif dalam indeks tunggal yang disebut \\i Degree of Integration of Focal Area (DIFA)\\i0 . Indeks yang ringkas sekaligus indikatif ini akan membantu pemangku kepentingan yang harus mempertimbangan banyak aspek dalam analisis kebijakan pengelolaan lahan, khususnya dalam memperkirakan implikasi suatu keputusan terhadap kelestarian biodiversitas suatu daerah. Informasi kewilayahan juga turut melengkapi luaran modul ini untuk membantu proses dialog dan kolaborasi antar pemangku kepentingan dalam perencanaan dan pengelolaan daerah yang memperhatikan kelestarian keanekaragaman hayati.\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile, n = 1)
    addParagraph(rtffile, chapter1)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Analisis dalam modul \\i QUES-B \\i0 dijalankan berdasarkan masukan data berupa tabel dan peta. Tabel yang diperlukan antara lain tabel bobot kontras tepi (\\i edge contrast weight\\i0) yang memuat angka derajat perbedaan antara suatu kelas tutupan lahan terhadap kelas lainnya dalam memfasilitasi keberadaan biodiversitas asli dari area fokal. Peta tutupan lahan serta peta unit perencanaan adalah data-data spasial yang menjadi masukan utama dalam analisis. Peta tutupan lahan digunakan sebagai dasar dari penghitungan luas dan konfigurasi spasial area fokal dalam daerah studi. Penggunaan peta tutupan lahan pada dua titik waktu membuat analisis dinamika area fokal antar waktu dapat dilakukan. Pemahaman mengenai hubungan erat antara subjek pengelolaan kawasan yang beragam dengan kondisi area fokal suatu daerah melatarbelakangi pelibatan peta unit perencanaan dalam tahap penyajian hasil analisis. \\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Pada bagian ini ditampilkan data-data spasial yang digunakan sebagai masukan dalam analisis. \\fs24")
    addParagraph(rtffile, text)
    addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
    text <- paste0("\\b\\fs24 Peta Tutupan Lahan \\b0\\fs24", area_name_rep,"  \\b\\fs24 tahun \\b0\\fs24", I_O_period_1_rep)
    addParagraph(rtffile, text)
    addNewLine(rtffile, n=1)
    addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, plot.LU1 )
    #rm(plot.LU1)
    text <- paste0("\\b\\fs24 Peta Tutupan Lahan \\b0\\fs24", area_name_rep, "  \\b\\fs24 tahun \\b0\\fs24", I_O_period_2_rep)
    addParagraph(rtffile, text)
    addNewLine(rtffile, n=1)
    addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, plot.LU2 )
    addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
    #rm(plot.LU2)
    text <- paste("\\b\\fs24 Peta Unit Perencanaan\\b0\\fs24 ", area_name_rep, sep=" ")
    addParagraph(rtffile, text)
    addNewLine(rtffile, n=1)
    addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, plot.Z )
    #rm(plot.Z)
    addNewLine(rtffile, n=1)
    text <- paste0("\\b\\fs24 Kelas tutupan lahan yang dijadikan sebagai area fokal: ", names(fa_class[p]), "\\b0\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile, n=1)
    #==== Report II. Focal Area Changes ====
    addParagraph(rtffile, chapter2)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Dalam \\i QUES-B \\i0 digunakan istilah area fokal yang didefinisikan sebagai kelas tutupan lahan yang mewakili habitat bagi keanekaragaman hayati asli suatu daerah dalam kondisi primer. Suatu daerah studi dapat saja memiliki lebih dari satu jenis area fokal namun proses analisis dilangsungkan secara unik per area fokal. Dalam analisis dinamika area fokal, modul \\i QUES-B \\i0 memperhitungkan tak hanya luas namun juga konfigurasi spasialnya. Dengan kata lain, konektifitas dan diskonektifitas antara petak-petak area fokal mempengaruhi luaran analisis \\i QUES-B\\i0. \\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Bagian ini memuat informasi mengenai perubahan luas area fokal dan perubahan tutupan lahan yang berkaitan. Luas perubahan tutupan lahan dan area fokal terdampak serta statistik lanskap dasar yang mendeskripsikan kondisi umum area fokal secara kuantitatif tersaji dalam bentuk tabel. Selain itu, peta area fokal pada dua titik waktu yang berbeda juga ditampilkan untuk memberikan gambaran visual mengenai dimensi spasial dari dinamika yang terjadi. Pada tabel kedua sebelum terakhir ditunjukkan dinamika luas area fokal pada setiap unit perencanaan serta proporsi relatif luas area fokal terdampak terhadap luas total unit perencanaan (dalam %). \\fs24")
    addParagraph(rtffile, text)
    
    if(maxValue(chk_loss)>0) {
      addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
      text <- paste("\\b\\fs24 Peta Kelenyapan Area Fokal \\b0\\fs24", area_name_rep, "  \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24-\\b0\\fs24 ", I_O_period_2_rep,  sep="")
      addParagraph(rtffile, text)
      addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, plot.FAC.loss )
      addNewLine(rtffile, n=1)
      text <- paste("\\b\\fs24 Tutupan Lahan Terasosiasi dengan Kelenyapan Area Fokal \\b0\\fs24",area_name_rep, " \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24 -\\b0\\fs24", I_O_period_2_rep,  sep=" ")
      addParagraph(rtffile, text)
      addTable(rtffile, foc.area.loss.att)
      addParagraph(rtffile, "\\b\\fs20 *Besaran area dalam hektar; Proporsi dalam persen(%)\\b0\\fs20 ")
      addNewLine(rtffile, n=1)
    } else {
      print("Tidak ditemukan kelenyapan area fokal")
    }
    
    if(maxValue(chk_gain)>0) {
      addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
      text <- paste("\\b\\fs24 Peta Kemunculan Area Fokal \\b0\\fs24", area_name_rep, "  \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24-\\b0\\fs24 ", I_O_period_2_rep,  sep="")
      addParagraph(rtffile, text)
      addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, plot.FAC.gain )
      addNewLine(rtffile, n=1)
      text <- paste("\\b\\fs24 Tutupan Lahan Sebelum Kemunculan Area Fokal\\b0\\fs24 ",area_name_rep,  " \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep,'\\b\\fs24-\\b0\\fs24',I_O_period_2_rep,  sep=" ")
      addParagraph(rtffile, text)
      addTable(rtffile, foc.area.gain.att)
      addParagraph(rtffile, "\\b\\fs20 *Besaran area dalam hektar; Proporsi dalam persen(%)\\b0\\fs20 ")
      addNewLine(rtffile, n=1)
    } else {
      print("Tidak ditemukan kemunculan area fokal")
    }
    
    text <- paste("\\b\\fs24 Perubahan Area Fokal pada Unit Perencanaan\\b0\\fs24 ",area_name_rep, " \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep,'\\b\\fs24 -\\b0\\fs24',I_O_period_2_rep,  sep=" ")
    addParagraph(rtffile, text)
    addTable(rtffile, zstat.foc.area)
    addParagraph(rtffile, "\\b\\fs20 *Besaran area dalam hektar; Proporsi dalam persen(%)\\b0\\fs20 ")
    addNewLine(rtffile, n=1)
    text <- paste("\\b\\fs24 Statistik Lanskap Dasar\\b0\\fs24 ")
    addParagraph(rtffile, text)
    addTable(rtffile, foc.area.stats, row.names=TRUE)
    addParagraph(rtffile, "\\b\\fs20 *Besaran area dalam hektar;Panjang dalam meter; Proporsi dalam persen(%)\\b0\\fs20 ")
    addNewLine(rtffile, n=1)
    
    #==== Report III.  Map of dissimilarities from focal area====
    addParagraph(rtffile, chapter3)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Derajat perbedaan antara sebidang area fokal dengan kelas tutupan non fokal yang bersebelahan mempengaruhi sifat tepi, terutama permeabilitasnya (kemampuan tepi untuk meloloskan perpindahan individu spesies, materi, dll. dari area fokal). Bidang area fokal dengan ukuran, bentuk, dan konfigurasi yang sama dikatakan lebih rentan terhadap ancaman terhadap keanekaragaman hayatinya apabila tepinya bersebelahan langsung degan kelas-kelas pentutupan lahan yang derajat perbedaannya tinggi dibandingkan dengan kelas yang derajat perbedaannya rendah. Hal ini dibangun berdasarkan anggapan bahwa tepi area fokal yang berbatasan secara langsung dengan kelas yang memiliki kontras tinggi akan memiliki permeabilitas yang lebih rendah dan berlaku pula sebaliknya.\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Modul \\i QUES-B \\i0 menghitung estimasi permeabilitas suatu petak area fokal berdasarkan indeks kontras tepi total (IKTT atau \\i Total Edge Contrast Index\\i0, disingkat \\i TECI\\i0) pada tingkat sub-lanskap. Sub-lanskap ditentukan berdasarkan 'jendela bergerak' (\\i moving window\\i0) yang mengikhtisarkan indeks kontras tepi total pada area sub-lanskap yang berpusat pada setiap piksel area fokal. Pendekatan matematis yang diterapkan dalam peringkasan nilai indeks kontras tepi total dalam 'jendela bergerak' adalah perata-rataan. Adapun dimensi 'jendela bergerak', yakni ukuran dan bentuknya, diatur oleh pengguna pada jendela input. \\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Rentang nilai IKTT adalah nol hingga 100. Nilai IKTT nol menunjukkan kenihilan kontras antara suatu sel area fokal dengan petak-petak di sekitarnya, sedangkan nilai seratus terasosiasi dengan kondisi di mana area fokal sangat berbeda dengan piksel yang mengelilinginya (contoh: area fokal berupa kelas tutupan hutan primer yang 'dikepung' oleh area terbangun). Perlu diperhatikan bahwa tinggi rendahnya tingkat perbedaan suatu kelas tutupan lahan terhadap tutupan area fokal ditentukan berdasarkan nilai bobot kontras tepi yang dijadikan sebagai salah satu masukan modul ini (lihat bagian I). \\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Proses penghitungan IKTT dilakukan secara spasial sehingga dihasilkan peta yang menunjukkan distribusi nilai pada daerah studi. Satu titik waktu diwakili oleh satu peta IKTT seperti yang ditampilkan di bawah ini. Perubahan nilai IKTT yang umum teramati dalam analisis antarwaktu antara lain: penambahan, pengurangan, kemunculan, dan kelenyapan. Perubahan-perubahan tersebut merupakan dampak dari perubahan tutupan lahan suatu daerah pada suatu titik waktu ke titik waktu setelahnya terutama yang mempengaruhi luas dan distribusi area fokal dan/atau area di sekitarnya. Pemahaman lebih mendalam mengenai perubahan-perubahan IKTT serta interpretasinya terdapat pada bagian V. \\fs24")
    addParagraph(rtffile, text)
    addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
    text <- paste("\\b\\fs24 Peta Distribusi Nilai IKTT \\b0\\fs24 ",area_name_rep, " \\b\\fs24  tahun \\b0\\fs24", I_O_period_1_rep, sep="")
    addParagraph(rtffile, text)
    addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, plot.mw.init )
    addNewLine(rtffile, n=1)
    text <- paste("\\b\\fs24 Peta Distribusi Nilai IKTT \\b0\\fs24 ",area_name_rep, " \\b\\fs24  tahun \\b0\\fs24", I_O_period_2_rep, sep="")
    addParagraph(rtffile, text)
    addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, plot.mw.fin )
    addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
    
    #==== Report IV.  DIFA Chart ====
    addParagraph(rtffile, chapter4)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Informasi mengenai luas dan distribusi area fokal serta nilai IKTT suatu daerah memiliki tingkat spesifisitas dan kedetilan yang tinggi. Kenyataan bahwa terdapat banyak sekali faktor lain yang perlu diperhatikan (misalnya: legalitas, ekonomi, dan sosial) dalam proses pengelolaan dan perencanaan daerah serta keberagaman tingkat pemahaman para pemangku kepentingan terkait isu habitat dan lingkungan hidup, informasi-informasi terkait kondisi area fokal perlu dikemas secara ringkas dan padat. Untuk menjawab kebutuhan tersebut, telah dikembangkan suatu indeks numerik yang disebut sebagai \\i DIFA \\i0 (singkatan dari \\i Degree of Integration of Focal Area\\i0).\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Penghitungan indeks \\i DIFA \\i0 dilakukan setelah peta nilai IKTT suatu daerah dihasilkan. Suatu mekanisme peringkasan nilai IKTT dan luasan area fokal diterapkan dengan bantuan unit pengagregasi berupa poligon-poligon persegi yang mencakup seluruh area studi. Luas area fokal dan rataan seluruh nilai IKTT dari area yang tercakup setiap persegi dihitung dan diasosiasikan dengan ID unik dari masing-masing unit pengagregasi tersebut. Kemudian, dilakukan penyusunan catatan teragregasi berdasarkan nilai IKTT dari masing-masing ID unit dari kecil ke besar. Unit-unit dengan nilai IKTT yang sama diintegrasi sehingga diperoleh luasan total area fokal yang terasosiasi dengan nilai-nilai IKTT yang ada. Angka luasan total area fokal dikonversi menjadi nilai persentase relatif terhadap luasan total area fokal pada masa ketika luasan area fokal diasumsikan utuh dan belum terpengaruhi manusia secara signifikan. Setelah itu, dibuat kurva antara nilai kumulatif persentase area fokal (sumbu y) terhadap nilai IKTT (sumbu x). Hasil pembagian luas area di bawah kurva dengan angka 100 adalah nilai \\i DIFA \\i0 daerah studi pada suatu titik waktu. \\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Berbeda dengan nilai IKTT, peningkatan nilai \\i DIFA \\i0 mengindikasikan kondisi area fokal yang semakin ideal. Besarnya nilai \\i DIFA \\i0 dipengaruhi oleh beberapa faktor seperti: luas total, konfigurasi dan distribusi spasial area fokal pada titik waktu tertentu serta kontras area fokal dengan area di sekitarnya. Persentase total area fokal menentukan batas maksimal nilai \\i DIFA \\i0 pada suatu titik waktu, sedangkan nilai IKTT yang menunjukkan kontras area fokal dengan wilayah sekitarnya mempengaruhi kelerengan kurva kumulatif yang dihasilkan. Dengan demikian, dapat disimpulkan bahwa nilai \\i DIFA \\i0 yang tinggi berasosiasi dengan daerah berarea fokal luas dan terintegrasi. \\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Kurva nilai kumulatif persentase area fokal yang diproyeksikan terhadap nilai IKTT serta hasil penghitungan nilai \\i DIFA \\i0 ditampilkan pada bagian di bawah ini. \\fs24")
    addParagraph(rtffile, text)
    addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
    text <- paste("\\b\\fs24 Kurva \\i Degree of Integration of Focal Area (DIFA)\\b0\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile, n=0.5)
    addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, grid.arrange(difa.init, difa.final, ncol=2))
    addNewLine(rtffile, n=1)
    text <- paste("\\b\\fs24 Nilai Indeks \\i DIFA\\b0\\fs24")
    addParagraph(rtffile, text)
    text <- paste(I_O_period_1_rep, " : ", AUC.init, "%", "          ;    " ,I_O_period_2_rep, " : ", AUC.final, "%", sep=" ")
    addParagraph(rtffile, text)
    addNewLine(rtffile, n=1)
    #==== Report V.  Habitat Change Analysis ====
    addParagraph(rtffile, chapter5)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Perubahan indeks \\i DIFA \\i0 dipengaruhi oleh perubahan nilai IKTT dan luas area fokal yang terjadi dalam konteks yang sangat beragam. Pemahaman mengenai konteks-konteks tersebut dapat didekati melalui pengenalan status dan letak kawasan perencanaan tempat terjadinya perubahan nilai IKTT dan/atau luas area fokal. Seperti yang telah dituliskan sebelumnya, secara umum dinamika nilai IKTT dan luas area fokal dapat digolongkan menjadi peningkatan atau penurunan nilai IKTT serta kelenyapan dan kemunculan nilai IKTT sebagai dampak dari konversi tutupan lahan dari/menjadi kelas tutupan area fokal.\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Di awal bagian ini ditampilkan sebuah grafik yang menunjukkan keseluruhan ID unit agregasi (lihat bagian IV) dalam koordinat kartesian berupa nilai IKTT pada titik waktu terdahulu sebagai posisi x dan IKTT pada titik waktu terkini sebagai posisi Y. Luasan lingkaran menunjukkan luas area fokal pada masing-masing unit agregasi di setiap titik waktu; setiap titik waktu diwakili oleh warna lingkaran yang berbeda. Perbedaan luas lingkaran yang berpusat pada titik yang sama mengindikasikan terjadinya perubahan luas area fokal dalam periode yang diamati. Lingkaran-lingkaran yang berada di atas garis acuan diagonal merupakan unit agregasi tempat teramatinya peningkatan nilai IKTT, sementara lingkaran yang berada di bawah garis tersebut menunjukkan fenomena sebaliknya. ID unit agregasi yang mengalami kelenyapan atau kemunculan nilai IKTT ditampilkan pada pojok atas atau kanan grafik (melampaui nilai 100), secara berurutan.\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Di bawah grafik tersebut, terdapat peta yang menampilkan poligon agregasi untuk menunjukkan letak unit-unit agregasi pada daerah studi. Dengan mencocokkan nama titik dalam grafik terhadap indeks peta, pengguna dapat memetakan perkiraan lokasi terjadinya fenomena-fenomena yang mempengaruhi nilai \\i DIFA \\i0 suatu daerah. Informasi lebih lengkap mengenai bentuk perubahan tutupan lahan yang menyebabkan perubahan nilai IKTT dan kawasan yang terasosiasi disampaikan pada sub-bagian-sub-bagian berikutnya dalam bentuk peta dan tabel.\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    # summary plot
    text <- paste("\\b\\fs24 Grafik Dinamika Nilai IKTT dan Area Fokal \\b0\\fs24 ",area_name_rep, "  \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24 -\\b0\\fs24 ", I_O_period_2_rep,  sep="")
    addParagraph(rtffile, text)
    addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=5, res=150, teci_fa_dyn.plot)
    addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1.5,1,1,0.8))
    text <- paste("\\b\\fs24 Peta Unit Agregasi \\b0\\fs24",  sep="")
    addParagraph(rtffile, text)
    addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=5, res=150, ag_map)
    addNewLine(rtffile, n=1)
    tryCatch({
      text <- paste("\\b\\fs24 1. Peningkatan Nilai IKTT \\b0\\fs24 ",area_name_rep, "  \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24 -\\b0\\fs24 ", I_O_period_2_rep,  sep="")
      addParagraph(rtffile, text)
      addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HD)
      addNewLine(rtffile, n=1)
      text <- paste("\\b\\fs24 10 Perubahan Terbesar Tutupan Lahan yang Berasosiasi dengan Peningkatan Nilai IKTT\\b0\\fs24 ", sep="")
      addParagraph(rtffile, text)
      addTable(rtffile, luchg.db.degrad[1:10,])
      # addNewLine(rtffile, n=1)
    }, error=function(e){cat("Melewatkan analisis peningkatan nilai IKTT:",conditionMessage(e), "\n")})
    addNewLine(rtffile, n=1)
    
    tryCatch({
      if(maxValue(chk_loss)>0) {
        text <- paste("\\b\\fs24 2. Kelenyapan Nilai IKTT \\b0\\fs24 ",area_name_rep, "  \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24 -\\b0\\fs24 ", I_O_period_2_rep,  sep="")
        addParagraph(rtffile, text)
        addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HL)
        addNewLine(rtffile, n=1)
        text <- paste("\\b\\fs24 10 Perubahan Terbesar Tutupan Lahan yang Berasosiasi dengan Kelenyapan Nilai IKTT\\b0\\fs24 ", sep="")
        addParagraph(rtffile, text)
        addTable(rtffile, luchg.db.loss[1:10,])
        # addNewLine(rtffile, n=1)
      } else { 
        print("Tidak ditemukan kelenyapan nilai IKTT")
      }
    }, error=function(e){cat("Melewatkan analisis kelenyapan nilai IKTT:",conditionMessage(e), "\n")})
    addNewLine(rtffile, n=1)
    
    tryCatch({
      text <- paste("\\b\\fs24 3. Penurunan Nilai IKTT \\b0\\fs24 ",area_name_rep, "  \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24 -\\b0\\fs24 ", I_O_period_2_rep,  sep="")
      addParagraph(rtffile, text)
      addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HR)
      addNewLine(rtffile, n=1)
      text <- paste("\\b\\fs24 10 Perubahan Terbesar Tutupan Lahan yang Berasosiasi dengan Penurunan Nilai IKTT \\b0\\fs24 ", sep="")
      addParagraph(rtffile, text)
      addTable(rtffile, luchg.db.recovery[1:10,])
      # addNewLine(rtffile, n=1)
    }, error=function(e){cat("Melewatkan analisis penurunan nilai IKTT:",conditionMessage(e), "\n")})
    addNewLine(rtffile, n=1)
    
    tryCatch({
      if(maxValue(chk_gain)>0) {
        text <- paste("\\b\\fs24 4. Kemunculan Nilai IKTT \\b0\\fs24 ",area_name_rep, "  \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24 -\\b0\\fs24 ", I_O_period_2_rep,  sep="")
        addParagraph(rtffile, text)
        if (maxValue(chk_gain)>0) {
          addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=3, res=150, plot.HG)
        } else { 
          print('Melewatkan analisis kemunculan nilai IKTT') 
        }
        addNewLine(rtffile, n=1)
        text <- paste("\\b\\fs24 10 Perubahan Terbesar Tutupan Lahan yang Berasosiasi dengan Kemunculan Nilai IKTT\\b0\\fs24 ", sep="")
        addParagraph(rtffile, text)
        addTable(rtffile, luchg.db.gain[1:10,])
      } else {
        print("Tidak ditemukan kemunculan nilai IKTT")
      }
    }, error=function(e){cat("Melewatkan analisis kemunculan nilai IKTT:",conditionMessage(e), "\n")})
    #==== XXX Report VI.  Habitat Quality Comparison XXX ====
    #addParagraph(rtffile, chapter6)
    addNewLine(rtffile)
    #tryCatch({
    #text <- paste("\\b\\fs20 Habitat Quality Comparison in\\b0\\fs20 ",area_name_rep, "  ", I_O_period_1_rep, "\\b\\fs20 -\\b0\\fs20 ", I_O_period_2_rep,  sep="")
    #addParagraph(rtffile, text)
    #addPlot.RTF(rtffile, plot.fun=print, width=6.7, height=4, res=150, grid.arrange(plot.hb.chg.init, plot.hb.chg.final, ncol=2) )
    #addNewLine(rtffile, n=1)
    #addTable(rtffile, habitat.change)
    #addNewLine(rtffile, n=1)
    #},error=function(e){cat("skipping Habitat Quality Comparison analysis:",conditionMessage(e), "\n")})
    #addNewLine(rtffile, n=1)
    #==== Report VI.  TECI Zonal Statistics ====
    addParagraph(rtffile, chapter6)
    addNewLine(rtffile)
    text <- paste0("\\qj\\fs24 Analisis dinamika kondisi area fokal seringkali memerlukan tindak lanjut berupa dialog dengan pemangku kepentingan, misalnya untuk perencanaan dan implementasi suatu aksi intervensi seperti restorasi atau konservasi. Identifikasi pemangku kepentingan yang paling sesuai dengan isu pengelolaan kondisi area fokal dapat dibantu dengan ketersediaan informasi mengenai profil kawasan dari waktu ke waktu. Sehubungan dengan hal tersebut, pada bagian ini ditampilkan hasil penghitungan statistika deskriptif perubahan nilai IKTT pada masing-masing kawasan pengelolaan/unit perencanaan. Informasi disajikan dalam dua tabel: tabel pertama memuat informasi mengenai kenaikan nilai IKTT sedangkan tabel berikutnya mengenai penurunan nilai IKTT. Informasi tambahan berupa perubahan luas area fokal di setiap kawasan pengelolaan turut ditampilkan untuk membantu interpretasi dinamika riil yang terjadi.\\fs24")
    addParagraph(rtffile, text)
    addNewLine(rtffile)
    tryCatch({
      text <- paste("\\b\\fs24 Profil Unit Perencanaan: Kenaikan nilai IKTT \\b0\\fs24 ",area_name_rep, "  \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24 -\\b0\\fs24 ", I_O_period_2_rep,  sep="")
      addParagraph(rtffile, text)
      addTable(rtffile, zstat.habitat.degradation)
      addParagraph(rtffile, "\\b\\fs20 *max, min, mean, dan sd merupakan deskripsi numerik dari peningkatan nilai IKTT\\b0\\fs20 ")
      addParagraph(rtffile, "\\b\\fs20 *foc.area atau total focal area dalam hektar\\b0\\fs20 ")
      addNewLine(rtffile, n=1)
      
      text <- paste("\\b\\fs24 Profil Unit Perencanaan: Penurunan nilai IKTT \\b0\\fs24 ",area_name_rep, "  \\b\\fs24 periode \\b0\\fs24", I_O_period_1_rep, "\\b\\fs24 -\\b0\\fs24 ", I_O_period_2_rep,  sep="")
      addParagraph(rtffile, text)
      addTable(rtffile, zstat.habitat.recovery)
      addParagraph(rtffile, "\\b\\fs20 *max, min, mean, dan sd merupakan deskripsi numerik dari penurunan nilai IKTT\\b0\\fs20 ")
      addParagraph(rtffile, "\\b\\fs20 *foc.area atau total focal area dalam hektar\\b0\\fs20 ")
      addNewLine(rtffile, n=1)
    }, error=function(e){cat("Melewatkan analisis profil unit perencanaan:",conditionMessage(e), "\n")})
    addNewLine(rtffile, n=1)
    done(rtffile)
    t1 <- as.character(pd_1)
    t2 <- as.character(pd_2)
    tryCatch({
      dbase.preques.name<-paste("QuES_B_database_", location,'_', as.character(pd_1),'_', as.character(pd_2),'.ldbase', sep='')
      save(landuse_t1,landuse_t2, zone, cl_desc, lookup_z, t1, t2, location, mwfile.init,mwfile.final,habitat.degradation,habitat.loss.NA,habitat.gain.NA, habitat.recovery,file=dbase.preques.name)
    }, error=function(e){cat("QuES-B database production is failed, re-check your data :",conditionMessage(e), "\n")})
  }
}


