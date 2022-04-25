
library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maptools)
library(RColorBrewer)



hab<- raster("spatialdata/habitat.tif")
st<- raster("spatialdata/st.tif")
R<-1000
powerthresh<-0.25
disp<-1

test1<- Condatis(hab=hab, st=st, R=R, powerthresh=powerthresh, disp=disp)