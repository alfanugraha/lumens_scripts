##DB-PostgreSQL=group
##working_directory=folder
##proj.file=file

#=Load library
library(stringr)
library(RPostgreSQL)
library(DBI)
library(rpostgis)
library(zip)
library(utils)

#=Set time start
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#=Load active project that will be exported
load(proj.file)

#set zip name
LZIP<-dirname(proj.file)

#extract database name from proj.file
db_name <- gsub(paste0(LZIP, "/"), "", proj.file)
db_name <- gsub(".lpj", "", db_name)

createNewPGTbl = pathEnv
# db_name as a new db_name.sql
createNewPGTbl[6] = paste("pg_dump -d ", db_name, " > ", db_name, ".sql", sep="")

# replacement pgEnvBatch
newBatchFile <- file(pgEnvBatch)
writeLines(createNewPGTbl, newBatchFile)
close(newBatchFile)

# execute batch file
pgEnvBatchFile<-str_replace_all(string=pgEnvBatch, pattern="/", repl='\\\\')
system(pgEnvBatchFile)

#=Zipping process
zip(paste0(db_name, ".lzip"), dir(LZIP))
