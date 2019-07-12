##[SCIENDO]=group
##Directory_of_factors=folder
##Look_up_table_land_use=file
##Year_of_t1=number 2010
##Iteration=number 5
##Location=string
##passfilenames

library(raster)
library(rgdal)
library(SDMTools)
library(tiff)
library(foreign)
library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(spatial.tools)
library(pander)
library(knitr)
library(markdown)
library(rtf)

command<-paste('"C:/Program Files/Dinamica EGO/DinamicaConsole.exe" -processors 0 -log-level 4 "', Directory_of_factors, '/Factors/run_land_use_change_model.egoml"', sep="")
system(command)

command2<-paste(Directory_of_factors, "/Result/", sep="")
command3.1<-paste(command2,"Hasil_simulasi_01.tif", sep="")
landuse1<-raster(command3.1)

command2<-paste(Directory_of_factors, "/Result/", sep="")
command3.1<-paste(command2,"Hasil_simulasi_02.tif", sep="")
landuse2<-raster(command3.1)

command2<-paste(Directory_of_factors, "/Result/", sep="")
command3.1<-paste(command2,"Hasil_simulasi_03.tif", sep="")
landuse3<-raster(command3.1)

command2<-paste(Directory_of_factors, "/Result/", sep="")
command3.1<-paste(command2,"Hasil_simulasi_04.tif", sep="")
landuse4<-raster(command3.1)

command2<-paste(Directory_of_factors, "/Result/", sep="")
command3.1<-paste(command2,"Hasil_simulasi_05.tif", sep="")
landuse5<-raster(command3.1)

setwd(Directory_of_factors)
lookup_lc<- read.table(Look_up_table_land_use, header=TRUE, sep=",",)
colnames(lookup_lc)<-c("ID", "CLASS")
area_lc1<-as.data.frame(freq(landuse1))
area_lc2<-as.data.frame(freq(landuse2))
area_lc3<-as.data.frame(freq(landuse3))
area_lc4<-as.data.frame(freq(landuse4))
area_lc5<-as.data.frame(freq(landuse4))
colnames(area_lc1)[1] = "ID"
colnames(area_lc1)[2] = "COUNT_SIM1"
colnames(area_lc2)[2] = "COUNT_SIM2"
colnames(area_lc3)[2] = "COUNT_SIM3"
colnames(area_lc4)[2] = "COUNT_SIM4"
colnames(area_lc5)[2] = "COUNT_SIM5"

test1<-stack(landuse1,landuse2,landuse3,landuse4,landuse5)
png(file = "myplot.png", bg = "transparent", width = 1024, height = 768, res=200)
gambar1<-plot(test1)
dev.off()

area_lc<-cbind(area_lc1, area_lc2[2],area_lc3[2],area_lc4[2],area_lc5[2])
area_lc<-merge(lookup_lc, area_lc,by="ID")


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

if (0 %in% area_lc1$ID){
  myColors  <-c(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
} else {
  myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
}


#Landuse 1 map
myColors.lu <- myColors[1:length(unique(area_lc1$ID))]
ColScale.lu<-scale_fill_manual(name="Tipe tutupan lahan", breaks=area_lc1$ID, labels=area_lc$CLASS, values=myColors.lu)
plot.LU1<-gplot(landuse1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))
plot.LU2<-gplot(landuse2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))
plot.LU3<-gplot(landuse3, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))
plot.LU4<-gplot(landuse4, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))
plot.LU5<-gplot(landuse5, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#WRITE REPORT
title<-"\\b\\fs40 LUMENS-SCIENDO Project Report\\b0\\fs20"
sub_title<-"\\b\\fs32 SIMULASI PERUBAHAN PENGGUNAAN LAHAN\\b0\\fs20"
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1<-"\\b\\fs28 RINGKASAN HASIl SIMULASI \\b0\\fs20"
chapter2<-"\\b\\fs28 PETA SIMULASI PERUBAHAN PENGGUNAAN LAHAN \\b0\\fs20"
rtffile <- RTF("LUMENS_SCIENDO_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, date)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Simulasi perubahan tutupan lahan dilakukan untuk memperkirakan perubahan tutupan lahan di suatu daerah di masa yang akan datang. Analisa ini dilakukan dengan menggunakan data peta tutupan lahan pada dua periode waktu yang berbeda dan data-data faktor pemicu perubahan penggunaan lahan di suatu daerah. Informasi yang dihasilkan melalui analisa ini dapat digunakan dalam proses perencanaan untuk berbagai hal. Diantaranya adalah: memperkirakan prioritas pembangunan, memperkirakan dampak ex-ante perubahan penggunaan lahan, merencanakan skenario pembangunan di masa yang akan datang, dan lain sebagainya.")
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Data yang dihasilkan dalam analisa ini adalah data peta prediksi penggunaan lahan di masa yang akan datang berdasarkan faktor pemicu yang telah diperkirakan pada analisa perubahan penggunaan lahan menggunakan modul Pre-QUES")
addNewLine(rtffile)
addPng(rtffile,"myplot.png", width=5, height=5)
addNewLine(rtffile)
addTable(rtffile,area_lc,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, chapter2)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Pada bagian ini disajikan peta-peta hasil prediksi perubahan penggunaan lahan untuk keseluruhan bentang lahan yang dianalisa")
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,plot.LU1)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,plot.LU2)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,plot.LU3)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,plot.LU4)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,plot.LU5)
addNewLine(rtffile)
done(rtffile)

command<-paste("start ", "winword ", Directory_of_factors, "/LUMENS_SCIENDO_report.lpr", sep="" )
shell(command)

