##[LUMENS]=group
##project_file=file
##overview=output raster
##passfilenames

load(project_file)

setwd(LUMENS_path_user)
unlink(list.files(pattern="*"))

overview<-ref