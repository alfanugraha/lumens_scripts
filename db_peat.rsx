##DB-PostgreSQL=group
##data=raster
##proj.file=string
##type=selection Land Use/Cover; Planning Unit; Factor
##period=number 0
##description=string
##attribute_table=string
##statusoutput=output table
##passfilenames

#=Load library
library(spatial.tools)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(stringr)

#=Load active project 
load(proj.file)

Peat_value=1
#=Create raster_category function
# to synchronize all of the data spatial input
command="raster"
idx_peat<-1
name<-paste("Peat",sep="")
raster_category<-function(category, name, desc) {
  eval(parse(text=(paste(name,"_", idx_peat, "<<-", command,'("', data, '")', sep=""))))
  eval(parse(text=(paste(name,"_", idx_peat, "<<-spatial_sync_raster(", name, ',', 'ref, method = "ngb")', sep=""))))
  eval(parse(text=(paste(name,"_", idx_peat, "<<-", name, "*1",  sep=""))))
  eval(parse(text=(paste("names(", name,"_", idx_peat, ")<<-desc", sep=""))))
  eval(parse(text=(paste(name,"_", idx_peat, "@title<<-category", sep=""))))
}

# set driver connection
driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

category<-"peat_map"
data_name<-"peat"
#create attribute table
attribute_table<-read.table(attribute_table, sep=",", header = TRUE)
colnames(attribute_table)<-c("ID", "Legend")
eval(parse(text=(paste(data_name, idx_peat, "<-attribute_table",  sep=""))))

#write raster detail to PostgreSQL
eval(parse(text=(paste("list_of_data_peat<-data.frame(RST_DATA='in_", data_name, idx_peat,"', row.names=NULL)", sep=""))))

In_peat_i <- paste('in_', data_name, idx_peat, sep="")
#append list
dbWriteTable(DB, "list_of_data_peat", list_of_data_peat, append=TRUE, row.names=FALSE)

#write to csv
list_of_data_peat<-dbReadTable(DB, c("public", "list_of_data_peat"))
csv_file<-paste(LUMENS_path_user,"/csv_", category, ".csv", sep="")
write.table(list_of_data_peat, csv_file, quote=FALSE, row.names=FALSE, sep=",")

addRasterToPG(project, data, In_peat_i, srid)

resave(idx_peat, file=proj.file)

statuscode<-1
statusmessage<-"peat data has been added!"

dbDisconnect(DB)

#=Writing final status message (code, message)
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)