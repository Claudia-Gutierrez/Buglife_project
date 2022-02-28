library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maptools)

hab<- raster("data/raster/test.tif")
st<- raster("data/raster/st.tif")

test<-Condatis(hab=hab, st=st, R=100, powerthresh=0.25, disp=1)
 