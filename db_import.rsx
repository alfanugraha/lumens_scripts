##DB-PostgreSQL=group
##zip_file=string
##new_working_directory=folder
##statusoutput=output table

#=Load library ====
library(zip)
library(utils)
library(stringr)
library(RPostgreSQL)
library(DBI)
library(rpostgis)

# unzipping the .lpj file defined in 'zip_file' at the destination_folder
proj_zfile <- grep(pattern = ".lpj$", utils::unzip(zip_file, list=TRUE)[,1], value=TRUE)
dir_name <- substr(proj_zfile, 1, (nchar(proj_zfile)-4)) # 'dir_name' is equivalent to 'project'
project_dir <- paste0(new_working_directory, "/", dir_name)
if(!dir.exists(project_dir)) dir.create(project_dir)
unzip(zip_file, exdir = project_dir)

# loading the original .lpj file ====
proj.file <- paste0(project_dir, "/", dir_name, ".lpj")
load(proj.file)

# Updating desktop architecture ====
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
    unlink(project_dir, recursive=T)
    quit()
  }
}

# set PostgreSQL driver and profile =====
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
  unlink(project_dir, recursive=T)
  quit()
}

# check status of PostgreSQL server ====
pg_isready<-paste0("pg_isready -p ", pgconf$port)
pg_response<-system(pg_isready)
if(pg_response==2){
  # please check your connection
  statuscode<-0
  statusmessage<-"Please check PostgreSQL connection.."
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  unlink(project_dir, recursive=T)
  quit()
}

# Updating the temporary folder according to the new user profile====
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS", sep="") 
if(!dir.exists(LUMENS_path_user)) dir.create(LUMENS_path_user, mode="0777")

# clear temp first
setwd(LUMENS_path_user)
unlink(list.files(pattern="*"))

# getting an information of windows architecture through the path of LUMENS installation ====
gdaltranslate<-(paste0("\"",LUMENS_path, "\\bin\\gdal_translate.exe\""))
gdalraster<-(paste0("\"", LUMENS_path, "\\bin\\gdal_rasterize.exe\""))

# Adjustment of 'ref' variable ====
ref@file@name <- paste0(project_dir, "/ref.tif")

# set batch parameter and restore the database====
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
createNewPGTbl[9] = paste0('psql -d ', dir_name, ' < ', project_dir, "/", dir_name, ".sql")

newBatchFile <- file(pgEnvBatch)
writeLines(createNewPGTbl, newBatchFile)
close(newBatchFile)
pgEnvBatchFile<-str_replace_all(string=pgEnvBatch, pattern="/", repl='\\\\')

# set driver connection====
driver <- dbDriver('PostgreSQL')
check_connection<-tryCatch({
  DB <- dbConnect(
    driver, dbname=dir_name, host=as.character(pgconf$host), port=as.character(pgconf$port),
    user=as.character(pgconf$user), password=as.character(pgconf$pass)
  )
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
if(!is.null(check_connection)){
  statuscode<-0
  statusmessage<-"Failed to import LUMENS database!"
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  unlink(project_dir, recursive=T)
  quit()
} else {
  system(pgEnvBatchFile)
}

# project properties adjustment====
proj_descr$Description <- as.character(proj_descr$Description)
proj_descr[proj_descr$Type== "working_directory","Description"] <- new_working_directory

#=Save all params into .RData objects====
resave(LUMENS_path_user,
       LUMENS_path,
       pgEnvBatch,
       pathEnv,
       proj_descr,
       win_arch,
       processing_path,
       gdalraster,
       gdaltranslate,
       postgre_path,
       pgconf,
       file=proj.file)


#=Writing final status message (code, message)  ====
statuscode<-1
statusmessage<-"LUMENS database has been successfully imported!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)

