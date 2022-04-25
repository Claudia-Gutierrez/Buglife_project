################################################
#                                              #
#   Condatis Function  (Buglife's adaption)    #
#                                              #
################################################

#This script runs the Condatis function to calculate the speed metric in the landscape selected in step 2

#The script iterates the calculation of speed using different dispersal distances within the range estimated in step 1

#The result is a table with the speed corresponding to a particulars dispersal distance

#INPUTS:
# Hab - raster of the habitat you wish to measure connectivity over (habitat.tif/ habitat_bl.tif obtained with the layer preparation script)
# st - raster of location of sources and targets (st.tif obtained with the layer preparation script)
# R - R value of the species moving (number of movers produced per km^2 of habitat), fixed to 1000
# disp - range of dispersal distance per group

library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maptools)
library(RColorBrewer)

memory.limit(size=15000) 

#Raster of AOI without B-line project
hab<- raster("spatialdata/habitat.tif")
st<- raster("spatialdata/st.tif")
R<-1000

#Dispersal distance for bees and hoverflies (0.015-10.4km)
disper <-c(10^seq(-1.8,1,0.1))

#Dispersal distance for moths (0.00043-81.1km)
#disper <-10^seq(-3.367,1.91,0.2)

test_result<-data.frame()
for(i in disper) {
  test<-CondatisNR(hab=hab, st=st, R=R, disp=i)
  test_result<- rbind(test_result, test$conductance)
  }
  
colnames(test_result)<-c("disp" , "conductance")

con<- data.frame(test_result %>%
                   group_by(disp)%>%
                   summarise(Conduct = mean(conductance)))

write.csv(con, "conductance/test.csv")




#Raster of AOI including B-line projects
hab<-raster("spatialdata/habitatBL.tif")
st<- raster("spatialdata/st.tif")
R<-1000

#Dispersal distance for bees and hoverflies (0.015-10.4km)
disper <-c(10^seq(-1.8,1,0.1))

#Dispersal distance for moths (0.00043-81.1km)
#disper <-10^seq(-3.367,1.91,0.2)

test_result<-data.frame()
for(i in disper) {
  test<-CondatisNR(hab=hab, st=st, R=R, disp=i)
  test_result<- rbind(test_result, test$conductance)
}

colnames(test_result)<-c("disp" , "conductance")

con<- data.frame(test_result %>%
                   group_by(disp)%>%
                   summarise(Conduct = mean(conductance)))

write.csv(con, "conductance/test_bl.csv")


cond<- data.frame(read.csv("conductance/test.csv"))

cond_bl<- data.frame(read.csv("conductance/test_bl.csv"))

ggplot() +
  geom_point(data = cond, aes(x = log(disp), y = log(Conduct)), color = "red") + 
  geom_point(data = cond_bl, aes(x = log(disp), y = log(Conduct)))


