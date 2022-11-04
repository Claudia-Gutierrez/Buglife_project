################################################
#                                              #
#       Improvement Function                   #
#                                              #
################################################

#This function calculates the proportional increase of habitat quality and area before and after intervention

#INPUTS:
# before - *.tif of habitat before intervention
# after- *.tif of habitat before intervention
# Area given in ha assuming a 10m^2 resolution adjust accordingly

Improvement_func<-function(before, after){
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
  impro$site<- 
  impro$areabef<-befcount/10000
  impro$areaaft<-aftcount/10000
  colnames(impro)<- c("qual_improve","area_incr","site","area_before_ha","area_after_ha")
  return(impro)
}


