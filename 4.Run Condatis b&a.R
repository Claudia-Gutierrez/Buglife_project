################################################
#                                              #
#   Speed change before and after intervention #
#                                              #
################################################

#This script runs the Condatis function to calculate the speed metric in the landscapes selected in step 2

#The script iterates the calculation of speed using different dispersal distances within the range estimated in step 1 and 2

#The result is a table with the speed corresponding to a particular dispersal distance

#The area under the curve (AUC) of dispersal distance vs speed is calculated for both landscapes, i.e. habitat without B-line project and habitat with B-line project intervention. Then, the percentage of change of speed between both landscape curves is calculated.

#INPUTS:
# Hab - raster of the habitat you wish to measure connectivity over (habitat.tif/ habitat_bl.tif obtained with the layer preparation script step 2)
# st - raster of location of sources and targets (st.tif obtained with the layer preparation script)
# R - R value of the species moving (number of movers produced per km^2 of habitat), fixed to 1000 for all species
# disper - range of dispersal distance 

library(raster)
library(sf)
library(tidyverse)
library(rgdal)
library(dplyr)
library(ggplot2)
library(maptools)
library(scales)
library(DescTools)

# Run Condatis with dispersal distance iteration --------------------------


#Raster of AOI without B-line project
hab<- raster("spatialdata/habitat3k.tif") #3k grid @ 10m resolution
st<- raster("spatialdata/st3k.tif")
R<-1000

#Range defined between the minimum distance for bees and maximum distance between the source and target[Dispersal distance for bees and hoverflies: 0.015-10.4km; and for moths 0.00043-81.1km]
disper <-c(10^seq(-1.8,0.5,0.1))


test_result<-data.frame()
for(i in disper) {
  test<-CondatisNR(hab=hab, st=st, R=R, disp=i)
  test_result<- rbind(test_result, test$conductance)
  }
  
colnames(test_result)<-c("disp" , "conductance")

con<- data.frame(test_result %>%
                   group_by(disp)%>%
                   summarise(Conduct = mean(conductance)))

write.csv(con, "conductance/test3k.csv")



#Raster of AOI including B-line projects
hab<-raster("spatialdata/habitatBL3k.tif") #3k grid @ 10m resolution
st<- raster("spatialdata/st3k.tif")
R<-1000

#Range defined between the minimum distance for bees and maximum distance between the source and target[Dispersal distance for bees and hoverflies: 0.015-10.4km; and for moths 0.00043-81.1km]
disper <-c(10^seq(-1.8,0.5,0.1))

test_result<-data.frame()
for(i in disper) {
  test<-CondatisNR(hab=hab, st=st, R=R, disp=i)
  test_result<- rbind(test_result, test$conductance)
}

colnames(test_result)<-c("disp" , "conductance")

con<- data.frame(test_result %>%
                   group_by(disp)%>%
                   summarise(Conduct = mean(conductance)))

write.csv(con, "conductance/test_bl3k.csv")

# Plot results ------------------------------------------------------------

#Joining results of the conductance of landscapes with ('B-line') and without ('No B-line') B-line project intervention
cond<- data.frame(read.csv("conductance/test3k.csv"))
cond_bl<- data.frame(read.csv("conductance/test_bl3k.csv"))
conductance<-data.frame(cond$disp, cond$Conduct, cond_bl$Conduct)
colnames(conductance)<-c('disp_dist', 'before GCB','after GCB')
write.csv(conductance, "conductance/test_3kdiff.csv")

#Rearranging the conductance data frame to plot both landscapes
conductance.long <- conductance %>% 
  select('disp_dist', 'before GCB','after GCB') %>% 
  pivot_longer(-disp_dist, names_to = "Variable", values_to = "speed")


#plot absolute dispersal distance vs speed
ggplot(conductance.long, aes(disp_dist, speed, colour = Variable)) + 
  geom_point(size = 5)+
  labs(x = 'Dispersal distance [km]', y='Speed')+
  theme(text = element_text(size = 30), legend.position="right",
        legend.title=element_blank())

#plot absolute dispersal distance vs log speed
ggplot(conductance.long, aes(disp_dist, log10(speed), colour = Variable)) + 
  geom_point(size = 5)+
  labs(x = 'Dispersal distance [km]', y='log(Speed)')+
  theme(text = element_text(size = 30))


#plot log dispersal distance vs log speed
ggplot(conductance.long, aes(log10(disp_dist), log10(speed), colour = Variable))+ 
  geom_point(size = 5)+
  labs(x = 'log (Dispersal distance) [km]', y='log(Speed)' )+
  scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5))+
  theme(text = element_text(size = 30), legend.position="right",
        legend.title=element_blank())

# Estimate change of speed due to intervention ----------------------------

nobl_area<-AUC(conductance$disp_dist, conductance$`before GCB`)
nobl_area
bl_area<-AUC(conductance$disp_dist, conductance$`after GCB`)
bl_area
change<-bl_area-nobl_area
perc_change<-(change/nobl_area)*100
perc_change

write.csv(perc_change,"conductance/test_3kchange.csv")




# plot --------------------------------------------------------------------

befaf<-ggplot(conductance.long, aes(disp_dist, speed, colour=Variable))+
  geom_point(size = 3)+
  scale_x_log10("Dispersal distance (km)",
                labels = scales::number_format(accuracy = 0.01))+
  scale_y_log10("Speed",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
befaf
ggsave("figs/befaf.jpeg", befaf, width = 4250, height = 2500,
       units = "px", dpi = 500)

