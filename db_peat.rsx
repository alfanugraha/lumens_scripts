##DB-PostgreSQL=group
##data="C:/LUMENS_new/Data/1_Raster/peat/peat_48s_30mF.tif"
##proj.file="C:/LUMENS_new/Lumens_02/Lumens_02.lpj"
##type=selection Land Use/Cover; Planning Unit; Factor
##period=number 0
##description="peat_48s_30mF"
##attribute_table="C:/LUMENS_new/Data/3_Tabular/peat_legend.csv"
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
data_name<-"in_peat"

#create peat raster temp
peat_temp<-raster(data)
peat_temp<-reclassify(peat_temp, cbind(NA, 255)) # need to set as a dynamic variable
peat_temp_name<-paste0(LUMENS_path_user, "/peat_temp.tif")
writeRaster(peat_temp, filename=peat_temp_name, format="GTiff", overwrite=TRUE)

#create attribute table
attribute_table<-read.table(attribute_table, sep=",", header = TRUE)
peat_freq<-as.data.frame(freq(peat_temp))
attribute_table<-merge(peat_freq, attribute_table, by.x='value', by.y='Id', all.x=TRUE)
colnames(attribute_table)<-c("ID", "COUNT", "Legend")
#colnames(attribute_table)<-ncol(attribute_table)
eval(parse(text=(paste(data_name, idx_peat, "<-attribute_table",  sep=""))))
eval(parse(text=(paste(data_name, "_lut", idx_peat, "<-attribute_table", sep=""))))

#write raster detail to PostgreSQL
eval(parse(text=(paste("list_of_data_peat<-data.frame(RST_DATA='", data_name, idx_peat,"', RST_NAME='", description, "', LUT_NAME='", data_name, "_lut", idx_peat, "', row.names=NULL)", sep=""))))

# list_of_data_peat <- paste(list_of_data_peat, sep="")
In_peat_i <- paste(data_name, idx_peat, sep="")
InPeatLUT_i <- paste(data_name, "_lut", idx_peat, sep="")

#append list
dbWriteTable(DB, "list_of_data_peat", list_of_data_peat, append=TRUE, row.names=FALSE)
dbWriteTable(DB, InPeatLUT_i, eval(parse(text=(paste(InPeatLUT_i, sep="" )))), append=TRUE, row.names=FALSE)

#write to csv
list_of_data_peat<-dbReadTable(DB, c("public", "list_of_data_peat")) #error when upload to postgres
csv_file<-paste(LUMENS_path_user,"/csv_", category, ".csv", sep="")
write.table(list_of_data_peat, csv_file, quote=FALSE, row.names=FALSE, sep=",")

addRasterToPG(project, data, In_peat_i, srid)

resave(idx_peat, file=proj.file)

statuscode<-1
statusmessage<-"peat data has been added!"

dbDisconnect(DB)

#=Writing final status message (code, message)
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)