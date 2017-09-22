# AD 22/9/2017
# LUMENS
# Script to import LUMENS database
# the overall framework should replicate tasks covered by the db create such as architecture checking etc. PLUS the restoration of the dumped database

# user inputs AD comm: only 'working_directory' input slot retained, add also another slot for the file of .zip====
##DB-PostgreSQL=group
##project=string (enter name of the project)
##working_directory=folder
##description=string
##location=string (enter location)
##province=string (enter province name of your location)
##country=string (enter country name)
##admin_attribute=vector
##field_attribute=field admin_attribute
##spat_res=number 50
##dissolve_table=file
##statusoutput=output table

# AD testonly INPUT====
library(svDialogs)
repeat{
working_directory = dlgDir(default= "", title= paste0("Choose the directory into which the LUMENS data will be extracted"))$res
if(length(working_directory) != 0) break
}
repeat{
file_to_extract = file.choose()
if(length(file_to_extract) !=0) break
}
#=Load library AD inc====
library(zip)
library(utils)
library(stringr)
library(RPostgreSQL)
library(DBI)
library(rpostgis)

#=Set time start
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

# unzipping the .lpj file defined in 'file_to_extract' at the destination_folder
proj_zfile <- grep(pattern = ".lpj$", zip_list(zipfile = file_to_extract)$filename, value=TRUE)
dir_name <- substr(proj_zfile, 1, (nchar(proj_zfile)-4))
if(!dir.exists(paste0(working_directory, "/", dir_name))) dir.create(paste0(working_directory, "/", dir_name))
unzip(file_to_extract, exdir = paste0(working_directory, "/", dir_name))

# loading the original .lpj file AD inc====
proj.file <- paste0(working_directory, "/", dir_name,"/", dir_name, ".lpj")
load(proj.file)

# check desktop architecture AD include====
win_arch=Sys.getenv("R_ARCH")
LUMENS_path = paste0(Sys.getenv("ProgramFiles"), "\\LUMENS")
if (file.exists(LUMENS_path)){
  processing_path = paste0(LUMENS_path, "\\apps\\qgis\\python\\plugins\\processing\\r\\scripts")
} else{
  LUMENS_path = paste0(Sys.getenv("ProgramFiles(x86)"), "\\LUMENS")
  processing_path = paste0(LUMENS_path, "\\apps\\qgis\\python\\plugins\\processing\\r\\scripts")
}
postgre_path = paste0(Sys.getenv("ProgramFiles"), "\\PostgreSQL\\9.6")
if(!file.exists(postgre_path)){
  postgre_path = paste0(Sys.getenv("ProgramFiles(x86)"), "\\PostgreSQL\\9.6")
  if(!file.exists(postgre_path)){
    statuscode<-0
    statusmessage<-"Please install PostgreSQL database.."
    statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
    quit()
  }
}

# set PostgreSQL driver and profile AD inc=====
user_appdata<-Sys.getenv("APPDATA")
pgconf_file<-paste0(user_appdata, "\\postgresql\\pgpass.conf")
if(file.exists(pgconf_file)){
  pgconf_line<-readLines(pgconf_file)
  pgconf_len<-length(pgconf_line)
  pgconf_line<-pgconf_line[pgconf_len]
  pgconf_list<-unlist(str_split(pgconf_line, ':'))
  pgconf<-data.frame(rbind(pgconf_list))
  colnames(pgconf)<-c("host", "port", "auth", "user", "pass")
} else {
  # please install PostgreSQL 
  statuscode<-0
  statusmessage<-"Please check PostgreSQL configuration.."
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  quit()
}

# check status of PostgreSQL server AD inc====
pg_isready<-paste0("pg_isready -p ", pgconf$port)
pg_response<-system(pg_isready)
if(pg_response==2){
  # please check your connection
  statuscode<-0
  statusmessage<-"Please check PostgreSQL connection.."
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  quit()
}

#This variables only to find out the identity of user who create database for the first time AD inc====
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS", sep="") 
if(!dir.exists(LUMENS_path_user)) dir.create(LUMENS_path_user, mode="0777")

# clear temp first
setwd(LUMENS_path_user)
unlink(list.files(pattern="*"))

# getting an information of windows architecture through the path of LUMENS installation AD inc====
gdaltranslate<-(paste0("\"",LUMENS_path, "\\bin\\gdal_translate.exe\""))
gdalraster<-(paste0("\"", LUMENS_path, "\\bin\\gdal_rasterize.exe\""))

# Adjustment of 'ref' variable AD inc====
ref@file@name <- paste0(working_directory, "/", dir_name, "/ref.tif")

# set batch parameter AD inc ADD also the restoration of the database====
pgEnvBatch <- paste(LUMENS_path_user, "/pg_env.bat", sep="")
pathEnv = ""
pathEnv[1] = paste0("@SET PATH=", postgre_path, "\\bin;%PATH%")
pathEnv[2] = paste0("@SET PGDATA=", postgre_path, "\\data")
pathEnv[3] = paste0("@SET PGUSER=", pgconf$user)
pathEnv[4] = paste0("@SET PGPORT=", pgconf$port)
pathEnv[5] = paste0("@SET PGLOCALEDIR=", postgre_path, "\\share\\locale\n")

createNewPGTbl = pathEnv
# dir_name as a new pg_db name
createNewPGTbl[6] = paste("createdb ", dir_name, sep="")
createNewPGTbl[7] = paste('psql -d ', dir_name, ' -c "CREATE EXTENSION postgis;"', sep="")
createNewPGTbl[8] = paste('psql -d ', dir_name, ' -c "CREATE EXTENSION postgis_topology;"\n', sep="")
createNewPGTbl[9] = paste0('psql -d ', dir_name, ' < ', working_directory, "/", dir_name, "/", dir_name, ".sql")

newBatchFile <- file(pgEnvBatch)
writeLines(createNewPGTbl, newBatchFile)
close(newBatchFile)
# execute batch file
pgEnvBatchFile<-str_replace_all(string=pgEnvBatch, pattern="/", repl='\\\\')
system(pgEnvBatchFile)

# set driver connection AD incl====
driver <- dbDriver('PostgreSQL')
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

# write project properties into table AD inc to be changed into table properties adjustment AD to edit the proj.file====
proj_descr[proj_descr$Type= "working_directory","Description"] <- working_directory

#=Save all params into .RData objects AD inc save SOME parameters====
save(LUMENS_path_user,
     LUMENS_path,
     pgEnvBatch,
     pathEnv,
     proj_descr,
     ref,
     srid,
     lut_ref,
     location,
     province,
     country,
     cov_desc,
     idx_landuse,
     idx_pu,
     idx_rec_pu,
     idx_factor,
     idx_lut,
     idx_lut_carbon,
     idx_lut_landuse,
     idx_lut_pu,
     idx_period,
     idx_PUR,
     idx_PreQUES,
     idx_QUESC,
     idx_QUESB,
     idx_QUESH,
     idx_SCIENDO_led,
     idx_SCIENDO_lucm,
     idx_TA_opcost,
     idx_TA_regeco,
     win_arch,
     processing_path,
     gdalraster,
     gdaltranslate,
     addRasterToPG,
     getRasterFromPG,
     postgre_path,
     pgconf,
     resave, 
     file=proj.file)
# write the properties of reference data to PostgreSQL AD inc fetch the list of pu data from the database and save it as csv====
# identification of data which have been input: check idx_ es: factor, landuse, lut, pu
data_types <- c("factor", "landuse", "lut", "pu")
abbrvs <- c("f", "luc", "lut", "pu")
categories <- c("factor_data", "land_use_cover","lookup_table","planning_unit")
# loop
for(d in 1:length(data_types)){
  # check whether the value of 'idx_'data_types[d] is bigger than 0
  logic <- eval(parse(text=paste0("idx_", data_types[d], " > 0")))
  if(logic){
    list_of_data_lut<-dbReadTable(DB, c("public", paste0("list_of_data_",abbrvs[d])))
    csv_file<-paste0(LUMENS_path_user,"/csv_",categories[d],".csv")
    write.table(list_of_data_lut, csv_file, quote=FALSE, row.names=FALSE, sep=",")
  }
}

#=Writing final status message (code, message) AD inc to be changed====
statuscode<-1
statusmessage<-"LUMENS database has been successfully imported!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)

