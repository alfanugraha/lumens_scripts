##DB-PostgreSQL=group
##proj.file=string
##database_status=output table

library(tcltk)

load(proj.file)

numberOfObject<-length(ls(all.names=T))

#=Create function for list all objects in project file
.ls.objects <- function (pos = 1, pattern, order.by,
  decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
  fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
  as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
  out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
  out <- head(out, n)
  out
}
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
listOfData<-lsos(pos = environment(), n=numberOfObject)
listOfData$var_name<-row.names(listOfData)
row.names(listOfData)<-NULL
setwd(dirname(proj.file))

#description
status_country<-c("country", eval(parse(text=(paste("country")))) )
status_province<-c("province", eval(parse(text=(paste("province")))) )
status_location<-c("location", eval(parse(text=(paste("location")))) )
database_status<-as.data.frame(rbind(status_country, status_location, status_province))
database_status$V1<-as.character(factor(database_status$V1))
database_status$V2<-as.character(factor(database_status$V2))

#all maps
#check Landuse_ var
row.names(database_status)<-NULL
status_landuse<-as.data.frame(ls(pattern="freq"))
n<-nrow(status_landuse)
if(n!=0){
data.y<-NULL
for (q in 1:n) {
data.x<-substr(as.character(factor(status_landuse[q,1])), 5, 14)
data.z<-names(eval(parse(text=(paste(data.x, sep="")))))
if(q==1){
data.y<-data.frame(data.x,data.z)
} else {
data_temp<-data.frame(data.x,data.z)
data.y<-rbind(data.y,data_temp)
}
}
colnames(data.y)[1]="V1"
colnames(data.y)[2]="V2"
status_landuse<-as.data.frame(data.y)
database_status<-rbind(database_status, status_landuse)
}

#check Peat_ var
status_peat<-as.data.frame(ls(pattern="Peat"))
n<-nrow(status_peat)
if(n!=0){
  data.y<-NULL
  for (q in 1:n) {
    data.x<-as.character(factor(status_peat[q,1]))
    data.z<-names(eval(parse(text=(paste(data.x, sep="")))))
    if(q==1){
      data.y<-data.frame(data.x,data.z)
    } else {
      data_temp<-data.frame(data.x,data.z)
      data.y<-rbind(data.y,data_temp)
    }
  }
  colnames(data.y)[1]="V1"
  colnames(data.y)[2]="V2"
  status_peat<-as.data.frame(data.y)
  database_status<-rbind(database_status, status_peat)
}

#check pu_ var
status_pu<-as.data.frame(as.character(ls(pattern="pu_pu")))
n<-nrow(status_pu)
if (n!=0) {
data.y<-NULL
for (q in 1:n) {
data.x<-as.character(factor(status_pu[q,]))
data.z<-names(eval(parse(text=(paste(data.x, sep="")))))
if(q==1){
data.y<-data.frame(data.x,data.z)
} else {
data_temp<-data.frame(data.x,data.z)
data.y<-rbind(data.y,data_temp)
}
}
colnames(data.y)[1]="V1"
colnames(data.y)[2]="V2"
status_pu<-as.data.frame(data.y)
database_status<-rbind(database_status, status_pu)
} else {
status_pu<-c("ref", eval(parse(text=(paste("names(ref)")))) )
database_status<-rbind(database_status, status_pu)
}

#index
status_PUR.index<-c("idx_PUR", eval(parse(text=(paste("idx_PUR")))) )
status_PreQUES.index<-c("idx_PreQUES", eval(parse(text=(paste("idx_PreQUES")))) )
status_QUESB.index<-c("idx_QUESB", eval(parse(text=(paste("idx_QUESB")))) )
status_QUESC.index<-c("idx_QUESC", eval(parse(text=(paste("idx_QUESC")))) )
status_QUESH.index<-c("idx_QUESH", eval(parse(text=(paste("idx_QUESH")))) )
status_SCIENDO1.index<-c("idx_SCIENDO_led", eval(parse(text=(paste("idx_SCIENDO_led")))) )
status_SCIENDO2.index<-c("idx_SCIENDO_lucm", eval(parse(text=(paste("idx_SCIENDO_lucm")))) )
status_TA1.index<-c("idx_TA_opcost", eval(parse(text=(paste("idx_TA_opcost")))) )
status_TA2.index<-c("idx_TA_regeco", eval(parse(text=(paste("idx_TA_regeco")))) )
status_landuse.index<-c("idx_landuse", eval(parse(text=(paste("idx_landuse")))) )
status_factor.index<-c("idx_factor", eval(parse(text=(paste("idx_factor")))) )
status_lut.index<-c("idx_lut", eval(parse(text=(paste("idx_lut")))) )
status_lut_carbon.index<-c("idx_lut_carbon", eval(parse(text=(paste("idx_lut_carbon")))) )
status_lut_landuse.index<-c("idx_lut_landuse", eval(parse(text=(paste("idx_lut_landuse")))) )
status_lut_zone.index<-c("idx_lut_pu", eval(parse(text=(paste("idx_lut_pu")))) )
database_status<-rbind(database_status, status_PUR.index, status_PreQUES.index,
status_QUESB.index, status_QUESC.index, status_QUESH.index,
status_SCIENDO1.index, status_SCIENDO2.index, status_TA1.index,
status_TA2.index, status_landuse.index, status_factor.index,
status_lut.index, status_lut_carbon.index, status_lut_landuse.index, status_lut_zone.index)

#check period
status_period<-as.data.frame(as.character(ls(pattern="period\\d")))
n<-nrow(status_period)
if (n!=0) {
data.y<-NULL
for (q in 2:n) {
data.x<-as.character(factor(status_period[q,]))
data.z<-eval(parse(text=(paste(data.x, sep=""))))
if(q==2){
data.y<-data.frame(data.x,data.z)
} else {
data_temp<-data.frame(data.x,data.z)
data.y<-rbind(data.y,data_temp)
}
}
colnames(data.y)[1]="V1"
colnames(data.y)[2]="V2"
status_period<-as.data.frame(data.y)
database_status<-rbind(database_status, status_period)
}
row.names(database_status)<-NULL
colnames(database_status)[1]="Data"
colnames(database_status)[2]="Value"

#=Create HTML file (.html)
htmlproject<-paste("status_LUMENS_database.html", sep="")
sink(htmlproject)
cat("<!DOCTYPE html>")
cat("<html><head><meta name='qrichtext' content='1' />")
cat("<style>
table a:link {color: #666;font-weight: bold;text-decoration:none;}
table a:visited {color: #999999;font-weight:bold;text-decoration:none;}
table a:active, table a:hover { color: #bd5a35;text-decoration:underline;}
table {font-family:Arial, Helvetica, sans-serif;color:#666;font-size:12px;text-shadow: 1px 1px 0px #fff;background:#eaebec;margin:20px;border:#ccc 1px solid;-moz-border-radius:3px;-webkit-border-radius:3px;border-radius:3px;-moz-box-shadow: 0 1px 2px #d1d1d1;-webkit-box-shadow: 0 1px 2px #d1d1d1;box-shadow: 0 1px 2px #d1d1d1;}
table th {
    padding:10px 25px 11px 25px;
    border-top:1px solid #fafafa;
    border-bottom:1px solid #e0e0e0;

    background: #ededed;
    background: -webkit-gradient(linear, left top, left bottom, from(#ededed), to(#ebebeb));
    background: -moz-linear-gradient(top,  #ededed,  #ebebeb);
}
table th:first-child { text-align: left; padding-left:20px; }
table tr:first-child th:first-child {-moz-border-radius-topleft:3px;-webkit-border-top-left-radius:3px;border-top-left-radius:3px;}
table tr:first-child th:last-child {-moz-border-radius-topright:3px;-webkit-border-top-right-radius:3px;border-top-right-radius:3px;}
table tr {text-align: center;padding-left:20px;}
table td:first-child {text-align: left;padding-left:20px;border-left: 0;}
table td {padding:5px;border-top: 1px solid #ffffff;border-bottom:1px solid #e0e0e0;border-left: 1px solid #e0e0e0;

    background: #fafafa;
    background: -webkit-gradient(linear, left top, left bottom, from(#fbfbfb), to(#fafafa));
    background: -moz-linear-gradient(top,  #fbfbfb,  #fafafa);
}
table tr.even td {background: #f6f6f6;background: -webkit-gradient(linear, left top, left bottom, from(#f8f8f8), to(#f6f6f6));background: -moz-linear-gradient(top,  #f8f8f8,  #f6f6f6);}
table tr:last-child td {border-bottom:0;}
table tr:last-child td:first-child {-moz-border-radius-bottomleft:3px;-webkit-border-bottom-left-radius:3px;border-bottom-left-radius:3px;}
table tr:last-child td:last-child {-moz-border-radius-bottomright:3px;-webkit-border-bottom-right-radius:3px;border-bottom-right-radius:3px;}
table tr:hover td {background: #f2f2f2;background: -webkit-gradient(linear, left top, left bottom, from(#f2f2f2), to(#f0f0f0));background: -moz-linear-gradient(top,  #f2f2f2,  #f0f0f0);}")
cat("</style></head><body><table>")
n_status<-nrow(database_status)
for(row in 0:n_status){
  if (row == 0) { 
    cat(paste("<tr><th>",colnames(database_status)[1],"</th> <th>",colnames(database_status)[2],"</th></tr>"))
  } else {
    cat(paste("<tr><td>",database_status$Data[row],"</td> <td>",database_status$Value[row],"</td></tr>"))
  }
}
cat("</table></body></html>")
sink()

gc()