library(dplyr)
library(ggplot2)
library(rtf)
library(data.table)
library(rgdal)
library(ReporteRs)
library(tcltk)

start_time<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#wd data_input
wdlc<-"C:/LUprof/landcover"
wdt<-"C:/LUprof/tabular"
dir.create("C:/LUprof/sProfa_result")
setwd("C:/LUprof/sProfa_result")
prodraster<-"YES" #AE this is user option to produce result in tiff format
