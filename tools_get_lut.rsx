##TOOLS-PostgreSQL=group
##proj.file=string
##ref_data=string
##main_data=output table

library(DBI)
library(RPostgreSQL)
library(rpostgis)

#=Load active project
load(proj.file)

# set driver connection
driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
data_pu<-list_of_data_pu[which(list_of_data_pu$RST_NAME==ref_data),]
ref_table<-dbReadTable(DB, c("public", data_pu$LUT_NAME)) 

col_ref_table<-ncol(ref_table)
main_data<-subset(ref_table, select=colnames(ref_table)[col_ref_table])
