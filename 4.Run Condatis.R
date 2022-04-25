################################################
#                                              #
#   Condatis Function  (Buglife's adaption)    #
#                                              #
################################################

#This script runs the Condatis function to calculate the speed metric in the landscape selected in step 2

#The script iterates the calculation of speed using different dispersal distances within the range estimated in step 1

#The result is a table with the speed corresponding to a particular dispersal distance

#INPUTS:
# Hab - raster of the habitat you wish to measure connectivity over (habitat.tif/ habitat_bl.tif obtained with the layer preparation script)
# st - raster of location of sources and targets (st.tif obtained with the layer preparation script)
# R - R value of the species moving (number of movers produced per km^2 of habitat), fixed to 1000
# disp - range of dispersal distance per group

library(raster)
library(sf)
library(tidyverse)
library(rgdal)
library(dplyr)
library(ggplot2)
library(maptools)
library(scales)

# Run Condatis with dispersal distance iteration --------------------------


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


# Plot results ------------------------------------------------------------

#Joining results of the conductance of landscapes with ('B-line') and without ('No B-line') B-line project intervention
cond<- data.frame(read.csv("conductance/test.csv"))
cond_bl<- data.frame(read.csv("conductance/test_bl.csv"))
conductance<-data.frame(cond$disp, cond$Conduct, cond_bl$Conduct)
colnames(conductance)<-c('disp_dist', 'No B-line','B-line')

#Rearranging the conductance data frame to plot both landscapes
conductance.long <- conductance %>% 
  select('disp_dist', 'No B-line','B-line') %>% 
  pivot_longer(-disp_dist, names_to = "Variable", values_to = "speed")


#plot absolute dispersal distance vs log speed
ggplot(conductance.long, aes(disp_dist, log10(speed), colour = Variable)) + 
  geom_point()+
  labs(x = 'Dispersal distance [km]', y='log(Speed)')

#plot log dispersal distance vs log speed
ggplot(conductance.long, aes(log10(disp_dist), log10(speed), colour = Variable)) + 
  geom_point()+
  labs(x = 'log_Dispersal distance (km)', y='log(Speed)' )+
  scale_x_continuous(breaks=c(-1,0,1), labels=c("-1 (0.1)","0 (1)", "1 (10)"))



library(DescTools)
nobl_area<-AUC(conductance$disp_dist, conductance$`No B-line`)
nobl_area
bl_area<-AUC(conductance$disp_dist, conductance$`B-line`)
bl_area
change<-bl_area-nobl_area
perc_change<-(change/nobl_area)*100
perc_change



