################################################
#                                              #
#   Condatis Function  (Buglife's adaption)    #
#                                              #
################################################

#This script runs the Condatis function to calculate the speed metric in the landscape selected in step 2

#The script iterates the calculation of speed using different dispersal distances within the range estimated in step 1

#The result is a table with the speed corresponding to a particulars dispersal distance

#INPUTS:
# Hab - raster of the habitat you wish to measure connectivity over (habitat.tif obtained with the layer preparation script)
# st - raster of location of sources and targets (st.tif obtained with the layer preparation script)
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
disper <-data.frame(disper=10^seq(-1,1,1)) 

#Dispersal distance for bees and hoverflies (0.015-10.4km)
#disper <-data.frame(disper=10^seq(-1.8,1,0.1))

#Dispersal distance for moths (0.00043-81.1km)
#disper <-data.frame(disper=10^seq(-3.367,1.91,0.5))

test_result<-data.frame()
for(i in 1:nrow(disper)) {
  dis<-disper$disper
  test<-CondatisNR(hab=hab, st=st, R=1000, disp=dis)
  test_result<- rbind(test_result, test$flow)
  }
  
colnames(test_result)<-c("xm", "ym", "cover", "x", "y", "disp", "progress", "flow", "std_flow", "conductance")

#Calculate speed per dispersal distance
speed<-test_result %>% 
  group_by(disp) %>%
  summarise(Speed = sum(flow))



