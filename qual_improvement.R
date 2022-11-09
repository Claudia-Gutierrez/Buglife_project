################################################
#                                              #
#    Quality improvement and area increase     #
#                                              #
################################################

#This script calculates the proportional change of quality and proportional area increase before and after intervention
#Plots the improvement/area increase vs the sum of weighted change

#INPUTS:
# habitatX.tif - tif file created in step 2 of site X before intervention
#habitatBL.tif- tif file created in step 2 of site X after intervention
#weighted_change- data frame with sum of weighted change per site calculated with the 'sum_cond_change' script


# Improvement function -------------------------------------------------------------
#This function calculates the proportional increase of habitat quality and area before and after intervention

#INPUTS:
# before - *.tif of habitat before intervention
# after- *.tif of habitat before intervention
# NOTE:Area given in ha assuming a 10m^2 resolution, adjust accordingly

Improvement_func<-function(before, after, site){
  library(raster)
  library(dplyr)
  
  qbef<-cellStats(before, stat='sum', na.rm=TRUE)
  qaft<-cellStats(after, stat='sum', na.rm=TRUE)
  
  prop_impro<- (qaft-qbef)/qbef
  
  bef1_<- raster::reclassify(before, c(0.01, 1, 1))
  befcount<-cellStats(bef1_, 'sum')
  
  aft1_<- raster::reclassify(after, c(0.01, 1, 1))
  aftcount<-cellStats(aft1_, 'sum')
  area_incr<-(aftcount-befcount)/befcount
  
  impro<-as.data.frame(prop_impro)
  impro$area_inc<-area_incr
  impro$site<- site
    impro$areabef<-befcount/10000
  impro$areaaft<-aftcount/10000
  colnames(impro)<- c("qual_improve","area_incr","site","area_before_ha","area_after_ha")
  return(impro)
}



# 180 ---------------------------------------------------------------------
site<-180
before<- raster("spatialdata/habitat180.tif")
after<- raster("spatialdata/habitatBL180.tif")

impro180<- Improvement_func(before,after,site)

# 189 ---------------------------------------------------------------------
site<-189
before<- raster("spatialdata/habitat189.tif")
after<- raster("spatialdata/habitatBL189.tif")

impro189<- Improvement_func(before,after,site)

# 190 ---------------------------------------------------------------------

site<-190
before<- raster("spatialdata/habitat190.tif")
after<- raster("spatialdata/habitatBL190.tif")

impro190<- Improvement_func(before,after,site)

# 205 ---------------------------------------------------------------------
site<-205
before<- raster("spatialdata/habitat205.tif")
after<- raster("spatialdata/habitatBL205.tif")

impro205<- Improvement_func(before,after,site)


# 206 ---------------------------------------------------------------------
site<-206
before<- raster("spatialdata/habitat206.tif")
after<- raster("spatialdata/habitatBL206.tif")

impro206<- Improvement_func(before,after,site)

# 207 ---------------------------------------------------------------------
site<-207
before<- raster("spatialdata/habitat207.tif")
after<- raster("spatialdata/habitatBL207.tif")

impro207<- Improvement_func(before,after,site)

# 209 ---------------------------------------------------------------------
site<-209
before<- raster("spatialdata/habitat209.tif")
after<- raster("spatialdata/habitatBL209.tif")

impro209<- Improvement_func(before,after,site)

# 215 ---------------------------------------------------------------------
site<-215
before<- raster("spatialdata/habitat215.tif")
after<- raster("spatialdata/habitatBL215.tif")

impro215<- Improvement_func(before,after,site)


# 216 ---------------------------------------------------------------------
site<-216
before<- raster("spatialdata/habitat216.tif")
after<- raster("spatialdata/habitatBL216.tif")

impro216<- Improvement_func(before,after,site)

# 218 ---------------------------------------------------------------------
site<-218
before<- raster("spatialdata/habitat218.tif")
after<- raster("spatialdata/habitatBL218.tif")

impro218<- Improvement_func(before,after,site)


# 220 ---------------------------------------------------------------------
site<-220
before<- raster("spatialdata/habitat220.tif")
after<- raster("spatialdata/habitatBL220.tif")

impro220<- Improvement_func(before,after,site)


# 221 ---------------------------------------------------------------------
site<-221
before<- raster("spatialdata/habitat221.tif")
after<- raster("spatialdata/habitatBL221.tif")

impro221<- Improvement_func(before,after,site)


# 231 ---------------------------------------------------------------------
site<-231
before<- raster("spatialdata/habitat231.tif")
after<- raster("spatialdata/habitatBL231.tif")

impro231<- Improvement_func(before,after,site)



# Improvement all sites -------------------------------------------------------------

improvement<-rbind(impro180,impro189,impro190,impro205,impro206, impro207,impro209,impro215, impro216,impro218,impro220,impro221,impro231)

write.csv(improvement, "spatialdata/improvement.csv")


# Plot improvement vs weighted change EW ----------------------------------------

impro_weightEW<-left_join(weighted_changeEW, improvement, by='site')

#quality vs log (weighted change)

ggplot(impro_weightEW, aes(qual_improve, log10(sum_weight_change), size = area_after_ha))+
  ggtitle("East to West movement") +
  geom_point(color='#FF3366')+
  scale_size_area("Total \nhabitat area",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=3)+
  labs(x = 'Proportion of quality \nimprovement', y="log10(Sum of wheighted \nchange in speed)")+
  stat_smooth(method="lm",se=FALSE, color="#FF6699", show.legend = FALSE)+
  theme(text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())

corrEWq <- cor.test(x=impro_weightEW$qual_improve, y=log10(impro_weightEW$sum_weight_change), method = 'spearman')
corrEWq

#area vs log (weighted change)
ggplot(impro_weightEW, aes(area_incr, log10(sum_weight_change), size = area_after_ha))+
  ggtitle("East to West movement") +
  geom_point(color='#FF3366')+
  scale_size_area("Total \nhabitat area",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=3)+
  labs(x = 'Proportion of area increase', y="log10(Sum of wheighted \nchange in speed)")+
  stat_smooth(method="lm",se=FALSE, color="#FF6699", show.legend = FALSE)+
  theme(text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())

corrEWa <- cor.test(x=impro_weightEW$area_incr, y=log10(impro_weightEW$sum_weight_change), method = 'spearman')
corrEWa
#rho=0.7333333 p=0.02117 S = 44 95%ci= 0.5741896 0.9667247


# Plot improvement vs weighted change NS ----------------------------------------

impro_weightNS<-left_join(weighted_changeNS, improvement, by='site')

#quality
ggplot(impro_weightNS, aes(qual_improve, sum_weight_change, size = area_after_ha))+
  geom_point(color='cyan4')+
  scale_size_area("Total \nhabitat area",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=3)+
  labs(x = 'Proportion of quality \nimprovement', y="Sum of wheighted \nchange in speed)")+
  stat_smooth(method="lm",se=FALSE, color="#669999", show.legend = FALSE)+
  theme(text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())

#quality vs log (weighted change)

ggplot(impro_weightNS, aes(qual_improve, log10(sum_weight_change), size = area_after_ha))+
  ggtitle("South to North movement") +
  geom_point(color='cyan4')+
  scale_size_area("Total \nhabitat area",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=3)+
  labs(x = 'Proportion of quality \nimprovement', y="log10(Sum of wheighted \nchange in speed)")+
  stat_smooth(method="lm",se=FALSE, color="#669999", show.legend = FALSE)+
  theme(text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())

corrNSq <- cor.test(x=impro_weightNS$qual_improve, y=log10(impro_weightNS$sum_weight_change), method = 'pearson')
corrNSq
#R=0.8732728 p=0.0004463 t=5.3769 95%ci= 0.5741896 0.9667247


#area vs log (weighted change)
ggplot(impro_weightNS, aes(area_incr, log10(sum_weight_change), size = area_after_ha))+
  ggtitle("South to North movement") +
  geom_point(color='cyan4')+
  scale_size_area("Total \nhabitat area",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=3)+
  labs(x = 'Proportion of area increase', y="log10(Sum of wheighted \nchange in speed)")+
  stat_smooth(method="lm",se=FALSE, color="#669999", show.legend = FALSE)+
  theme(text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())

corrNSa <- cor.test(x=impro_weightNS$area_incr, y=log10(impro_weightNS$sum_weight_change), method = 'pearson')
corrNSa
#R=0.7514287  p=0.00767 t=3.4165 95%ci= 0.2759351 0.9314433


