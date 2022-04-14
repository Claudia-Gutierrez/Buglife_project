################################################
#                                              #
#   Condatis Function  (Buglife's adaption)    #
#                                              #
################################################

#What
#how
#Iterations
#result

#INPUTS:
# Hab - raster of the habitat you wish to measure connectivity over (*.tif obtained with the layer preparation script)
# st - raster of location of sources and targets (*.tif obtained with the layer preparation script)
# R - R value of the species moving (number of movers produced per km^2 of habitat), fixed to 1000
# disp - range of dispersal distance per group

library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maptools)
library(RColorBrewer)

memory.limit(size=20000) 

#Raster of AOI
hab<- raster("spatialdata/habitat.tif")

#Raster of AOI including B-line projects
#hab<-raster("spatialdata/habitatBL.tif")

st<- raster("spatialdata/st.tif")
R<-1000
disper <-data.frame(disper=10^seq(-1.8,1,0)) #Dispersal distance for bees and hoverflies (0.015-10.4km)

test_result<-data.frame()
for(i in 1:nrow(disper)) {
  dis<-disper$disper
  test<-CondatisNR(hab=hab, st=st, R=1000, disp=dis)
  test_result<- rbind(test_result, test$flow)
  }
  
colnames(test_result)<-c("xm", "ym", "cover", "x", "y", "disp", "progress", "flow", "std_flow", "conductance")



