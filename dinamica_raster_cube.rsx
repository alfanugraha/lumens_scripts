##[SCIENDO]=group
##Directory_of_factors=folder
##Look_up_table_land_use=file
##Iteration=number 5
##Location=string
##passfilenames

command<-paste('"C:/Program Files/Dinamica EGO/DinamicaConsole.exe" -processors 0 -log-level 4 "', Directory_of_factors, '/Factors/create_raster_cube_Merangin.egoml"', sep="")
system(command)