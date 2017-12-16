##QUES-PostgreSQL=group
##proj.file=string
##SCIENDO_LUCM_index=string
##n_rep = number 4
##statusoutput=output table

library(spatial.tools)
library(splitstackshape)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(XML)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

load(proj.file)

# set driver connection
# driver <- dbDriver('PostgreSQL')
# project <- as.character(proj_descr[1,2])
# DB <- dbConnect(
#   driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
#   user=as.character(pgconf$user), password=as.character(pgconf$pass)
# )

#=Set working directory
SCIENDO_folder<-SCIENDO_LUCM_index
result_dir<-paste(dirname(proj.file),"/SCIENDO/", SCIENDO_folder, sep="")
setwd(result_dir)
factor_dir <- (paste(result_dir,"/factor/", sep=""))
raster_sink <- paste0(result_dir, "/result")
rate_dir <- paste0(result_dir, "/transition")
dir.create(raster_sink)
# skeleton builder====

# static_var<-data.frame(aliasFactor)
# static_var$identifier<-paste('&quot;static_var/', static_var$aliasFactor, '&quot; 10 500000 1 5,&#x0A;', sep='')
# 
# identifier<-do.call(paste, c(as.list(static_var$identifier), sep="        "))

start <- as.numeric(lusim_lc[1,1])
lenght <- as.numeric(nrow(lusim_lc))
end <- as.numeric(lusim_lc[lenght,1])

skeleton1<-data.frame(nT1=c(start:end), divider=lenght)
skeleton1<-expandRows(skeleton1, 'divider')
skeleton2<-data.frame(nT2=rep(rep(c(start:end), lenght)))

skeleton<-cbind(skeleton1, skeleton2)
skeleton <- skeleton[skeleton$nT1 != skeleton$nT2, ]
# rebuild the chunk
skeleton$char <- paste(skeleton$nT1, skeleton$nT2, sep = "-&gt;")
skeleton$char_fx <- paste0(skeleton$char, " 0.3,&#x0A;")
skeleton[nrow(skeleton), "char_fx"] <- gsub("3,&", "3&", skeleton[nrow(skeleton), "char_fx"])
txt_skl <- paste(skeleton$char_fx, collapse = "    ")
txt_skl2 <- gsub("0.3", "2 1 1", txt_skl)
txt_skl3 <- gsub("2 1 1", "1 1 1", txt_skl2)
# ADHERE
# skeleton$key<-do.call(paste, c(skeleton[c("nT1", "nT2")], sep = "-&gt;"))
# 
# skeleton$transition<-paste("&#x0A;    ", skeleton$key, " &#x0A;        ", sep='')
# 
# skeletonFinal<-do.call(paste, c(as.list(skeleton$transition), sep=","))
# skeletonFinal<-paste('[', skeletonFinal, "&#x0A;]", sep='')

# Variable list====
file_ers <- "sciendo_factor.ers"
file_init_lc <- "/landuse_2.tif"
zone <- "/zone.tif"
DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles"), "\\Dinamica EGO\\DinamicaConsole.exe")
if (file.exists(DINAMICA_exe)){
  urlDINAMICAConsole = DINAMICA_exe
} else{
  DINAMICA_exe<-paste0(Sys.getenv("ProgramFiles(x86)"), "\\Dinamica EGO\\DinamicaConsole.exe")
  urlDINAMICAConsole = DINAMICA_exe
}


# begin writing tag
con <- xmlOutputDOM(tag="script")
# add property
con$addTag("property", attrs=c(key="dff.date", value="2016-Nov-09 17:01:03"))
con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))

# begin.
# add functor = LoadMap
con$addTag("functor", attrs=c(name="LoadMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Static variable maps."))
con$addTag("inputport", attrs=c(name="filename"), paste('"', factor_dir, file_ers, '"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v1"))
con$closeTag("functor") 
# end.

# begin.
# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Initial landscape maps."))
con$addTag("inputport", attrs=c(name="filename"), paste('"', result_dir, file_init_lc, '"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v2"))
con$closeTag("functor")
# end.

# begin.
# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Municipalities"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', result_dir, zone, '"', sep=''))
con$addTag("inputport", attrs=c(name="nullValue"), ".none")
con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
con$addTag("inputport", attrs=c(name="step"), "0")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="map", id="v3"))
con$closeTag("functor")
# end.

# begin.
# add containerfunctor = ForEachRegion
con$addTag("containerfunctor", attrs=c(name="RegionManager"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="regionManager3260"))
con$addTag("inputport", attrs=c(name="regions", peerid="v3"))
con$addTag("inputport", attrs=c(name="borderCells"), 0)
con$addTag("internaloutputport", attrs=c(name="regionManager", id="v4"))

# add containerfunctor = Repeat
con$addTag("containerfunctor", attrs=c(name="Repeat"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="repeat279"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Simulation model."))
con$addTag("inputport", attrs=c(name="iterations"), n_rep)
con$addTag("internaloutputport", attrs=c(name="step", id="v5"))

# add functor = LoadCategoricalMap
con$addTag("functor", attrs=c(name="MuxCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Landscape"))
con$addTag("inputport", attrs=c(name="initial", peerid="v2"))
con$addTag("inputport", attrs=c(name="feedback", peerid="v15"))
con$addTag("outputport", attrs=c(name="map", id="v6"))
con$closeTag("functor")

# add functor = SaveMap
con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap282"))
con$addTag("inputport", attrs=c(name="map", peerid="v15"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', raster_sink, '/Landscape.tif"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 4)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add functor = SaveMap
con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap3414"))
con$addTag("inputport", attrs=c(name="map", peerid="v16"))
con$addTag("inputport", attrs=c(name="filename"), paste('"', raster_sink, '/Probabilities.tif"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 4)
con$addTag("inputport", attrs=c(name="step", peerid="v5"))
con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$closeTag("functor")

# add containerfunctor = ForEachCategory
con$addTag("containerfunctor", attrs=c(name="ForEachCategory"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="forEachCategory283"))
con$addTag("inputport", attrs=c(name="categorization", peerid="v3"))
con$addTag("internaloutputport", attrs=c(name="step", id="v7"))

con$addTag("functor", attrs=c(name="IntegerValue"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="int290"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="This operator is used here to force a dependence between two groups."))
con$addTag("inputport", attrs=c(name="constant"), 0)
con$addTag("outputport", attrs=c(name="object", id="v8"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="LoadTable"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Transition Matrix"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Load transition matrix."))
con$addTag("inputport", attrs=c(name="filename"), paste('"', rate_dir, '/Single_step.csv"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v7"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="table", id="v9"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="LoadWeights"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Weights of Evidence Coefficients"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Load Weights of Evidence coefficients."))
con$addTag("inputport", attrs=c(name="filename"), paste('"', factor_dir, '/WoE.dcf"', sep=''))
con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
con$addTag("inputport", attrs=c(name="step", peerid="v7"))
con$addTag("inputport", attrs=c(name="workdir"), ".none")
con$addTag("outputport", attrs=c(name="weights", id="v10"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="RegionalCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="regionalCategoricalMap289"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Assign a map to the region using the given identifier."))
con$addTag("inputport", attrs=c(name="globalMapName"), paste('"landscape"', sep=''))
con$addTag("inputport", attrs=c(name="regionalMap", peerid="v11"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="AllocateTransitions"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Updated Landscape (Region)"))
con$addTag("inputport", attrs=c(name="lanscape", peerid="v13"))
con$addTag("inputport", attrs=c(name="probabilities", peerid="v14"))
con$addTag("inputport", attrs=c(name="transitionMatrix", peerid="v9"))
con$addTag("inputport", attrs=c(name="percentOfTransitionsByExpansion"), paste('[&#x0A;    ', txt_skl, ']', sep=''))
con$addTag("inputport", attrs=c(name="patchExpansionParameters"), paste('[&#x0A;    ', txt_skl2, ']', sep=''))
con$addTag("inputport", attrs=c(name="patchGenerationParameters"), paste('[&#x0A;    ', txt_skl3, ']', sep=''))
con$addTag("inputport", attrs=c(name="printTransitionInfo"), ".no")
con$addTag("outputport", attrs=c(name="resultingLanscape", id="v11"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="RegionalizeMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v1"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v12"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Landscape (Region)"))
con$addTag("inputport", attrs=c(name="globalMap", peerid="v6"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="regionalMap", id="v13"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="RegionalMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="regionalMap3412"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Assign a map to the region using the given identifier."))
con$addTag("inputport", attrs=c(name="globalMapName"), paste('"probabilities"', sep=''))
con$addTag("inputport", attrs=c(name="regionalMap", peerid="v14"))
con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$closeTag("functor")

con$addTag("containerfunctor", attrs=c(name="CalcWOfEProbabilityMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="Probabilities (Region)"))
con$addTag("property", attrs=c(key="dff.functor.extendedcomment", value="Calculate probability map."))
con$addTag("inputport", attrs=c(name="landscape", peerid="v13"))
con$addTag("inputport", attrs=c(name="weights", peerid="v10"))
con$addTag("inputport", attrs=c(name="transitions"), paste('[ ', paste(skeleton$char, collapse = ", "), ']', sep=''))
con$addTag("inputport", attrs=c(name="cellType"), ".uint8")
con$addTag("inputport", attrs=c(name="nullValue"), ".default")
con$addTag("outputport", attrs=c(name="probabilities", id="v14"))

con$addTag("functor", attrs=c(name="NameMap"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="nameMap298"))
con$addTag("inputport", attrs=c(name="map", peerid="v12"))
con$addTag("inputport", attrs=c(name="mapName"), paste('"static_var"', sep=''))
con$closeTag("functor")

con$closeTag("containerfunctor") #    CalcWOfEProbabilityMap

con$closeTag("containerfunctor") # ForEachCategory

# add containerfunctor = Group
con$addTag("containerfunctor", attrs=c(name="Group"), close=FALSE)
con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
con$addTag("property", attrs=c(key="dff.functor.alias", value="group300"))

con$addTag("functor", attrs=c(name="IntegerValue"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="int302"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="This operator is used here to force a dependence between two groups."))
con$addTag("inputport", attrs=c(name="constant", peerid="v8"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="MergeRegionalCategoricalMaps"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="Updated Landscape"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Merge all maps assigned to the regions using the given identifier."))
con$addTag("inputport", attrs=c(name="globalMapName"), paste('"landscape"', sep=''))
con$addTag("inputport", attrs=c(name="mergeNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="globalMap", id="v15"))
con$closeTag("functor")

con$addTag("functor", attrs=c(name="MergeRegionalMaps"), close=FALSE)
con$addTag("property", attrs=c(key="dff.functor.alias", value="mergeRegionalMaps3413"))
con$addTag("property", attrs=c(key="dff.functor.comment", value="Merge all maps assigned to the regions using the given identifier."))
con$addTag("inputport", attrs=c(name="globalMapName"), paste('"probabilities"', sep=''))
con$addTag("inputport", attrs=c(name="mergeNonRegionCells"), ".no")
con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
con$addTag("outputport", attrs=c(name="globalMap", id="v16"))
con$closeTag("functor")

con$closeTag("containerfunctor") # Group

con$closeTag("containerfunctor")  # Repeat
con$closeTag("containerfunctor") # RegionManager
# end.

# write egoml
egoml_file=paste(result_dir, "/4_Simulation_per_Regions.egoml", sep='')
saveXML(con$value(), file=egoml_file)

# replace ampersand code character
egoml_text  <- readLines(egoml_file)
egoml_text_new  <- gsub(pattern="amp;", replace="", x=egoml_text)
writeLines(egoml_text_new, con=egoml_file)

command<-paste('"', urlDINAMICAConsole, '" -processors 0 -log-level 4 "', result_dir, '/4_Simulation_per_Regions.egoml"', sep="")

system(command)

# dbDisconnect(DB)

statuscode<-1
statusmessage<-"SCIENDO has completed successfully"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
