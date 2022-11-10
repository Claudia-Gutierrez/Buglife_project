################################################
#                                              #
#    Quality improvement and area increase     #
#                                              #
################################################

#This script calculates the proportional change of quality and proportional area increase before and after intervention

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

