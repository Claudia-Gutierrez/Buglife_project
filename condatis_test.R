library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maptools)
library(RColorBrewer)

memory.limit(size=20000)
hab<- raster("spatialdata/habitat.tif")
habbl<-raster("spatialdata/habitatBL.tif")
st<- raster("spatialdata/st.tif")

test<-Condatis(hab=hab, st=st, R=100, powerthresh=0.25, disp=0.1)
test$flow
plot(test$flow_raster, col=grey(1:100/100))

test2<-Condatis(hab=hab2, st=st, R=100, powerthresh=0.25, disp=1)
plot(test2$flow_raster, col=grey(1:100/100))
