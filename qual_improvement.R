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
#weighted_change- dataframe with sum of weighted change per site calculated with the 'sum_cond_change' script


# Improvement -------------------------------------------------------------


library(raster)

before<- raster("spatialdata/habitat231.tif")
after<- raster("spatialdata/habitatBL231.tif")


qbef<-cellStats(before, stat='sum', na.rm=TRUE)
qaft<-cellStats(after, stat='sum', na.rm=TRUE)

prop_impro<- (qaft-qbef)/qbef

bef1<- raster::reclassify(before, c(0.1, 1, 1))
befcount<-cellStats(bef1, 'sum')

aft1<- raster::reclassify(after, c(0.1, 1, 1))
aftcount<-cellStats(aft1, 'sum')
area_incr<-(aftcount-befcount)/befcount

impro231<-as.data.frame(prop_impro)
impro231$area_inc<-area_incr
impro231$site<- 231
colnames(impro231)<- c("qual_improve","area_incr","site")

improvement<-rbind(impro180,impro189,impro190,impro205,impro206, impro207,impro209,impro215, impro216,impro218,impro220,impro221,impro231)

write.csv(improvement, "spatialdata/improvement.csv")


# Plot improvement vs weighted change ----------------------------------------

impro_weight<-left_join(weighted_change, improvement, by='site')
plot(impro_weight$qual_improve,impro_weight$sum_weight_change)
text(impro_weight$qual_improve,impro_weight$sum_weight_change, labels=impro_weight$site, cex=0.6, pos=2, col="blue")

plot(impro_weight$qual_improve,log10(impro_weight$sum_weight_change))
text(impro_weight$qual_improve,log10(impro_weight$sum_weight_change), labels=impro_weight$site, cex=0.6, pos=2, col="blue")

plot(impro_weight$area_incr,log10(impro_weight$sum_weight_change))
text(impro_weight$area_incr,log10(impro_weight$sum_weight_change), labels=impro_weight$site, cex=0.6, pos=2, col="blue")

plot(impro_weight$area_incr,log10(impro_weight$sum_weight_change))
text(impro_weight$area_incr,log10(impro_weight$sum_weight_change), labels=impro_weight$site, cex=0.6, pos=2, col="blue")
