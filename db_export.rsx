##DB-PostgreSQL=group
##proj.file=string

#=Load library
library(stringr)
library(RPostgreSQL)
library(DBI)
library(rpostgis)
library(zip)
library(utils)

#=Load active project that will be exported
load(proj.file)

#set zip name
LZIP<-dirname(proj.file)
setwd(LZIP)

#extract database name from proj.file
db_name <- as.character(proj_descr[1,2])
sql_file <- paste0(LZIP, '/', db_name, '.sql')

createNewPGTbl = pathEnv
# db_name as a new db_name.sql

createNewPGTbl[6] = paste("pg_dump -d ", db_name, " > ", sql_file, sep="")
# replacement pgEnvBatch
newBatchFile <- file(pgEnvBatch)
writeLines(createNewPGTbl, newBatchFile)
close(newBatchFile)
# execute batch file
pgEnvBatchFile<-str_replace_all(string=pgEnvBatch, pattern="/", repl='\\\\')
system(pgEnvBatchFile)

#=Zipping process
zip(paste0(db_name, ".lzip"), dir(LZIP))
unlink(sql_file)
