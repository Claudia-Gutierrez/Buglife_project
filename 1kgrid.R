library (rgdal)
library(raster)
library(terra)
library(phyloregion)
library(ggplot2)
library(rgeos)
library(dplyr)

memory.limit(size=30000) 

# Select Area of Interest (AOI) -------------------------------------------------


#Read habitat shapefile
Cumbriahab <- readOGR("spatialdata", "CumbriaHab")
# Create Habitat IDs (to be replace by quality value below)
hab<- unique(Cumbriahab$LNRNHab)
habdf<- data.frame(ID = 1:length(hab), hab = hab)
Cumbriahab$ID <- habdf$ID[match(Cumbriahab$LNRNHab,habdf$hab)]

#Read B-Lines shapefile 
BLines<-readOGR("spatialdata", "BLinesCumbria")

#Create a grid to select a computable landscape, e.g. 1k (1000m)
Blgrid1k<- fishnet(BLines, res = 1000, type = "square")

#add ID grid
Blgrid1k$ID<-as.character(Blgrid1k@plotOrder)

#calculate coordinates to label grid
Blgrid1k@data <- cbind(Blgrid1k@data, gCentroid(Blgrid1k,byid = T) %>% coordinates())

#visualize potential AOI 

pdf("spatialdata/1AOI_BL1k.pdf")
Blgrid1kmap<-ggplot() + 
  geom_polygon(data = Blgrid1k, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = Blgrid1k@data, aes(x = x, y = y),label = Blgrid1k$ID, size=1)+  
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)
Blgrid1kmap
dev.off()
Blgrid1kmap

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")
GCBpol<-ggplot() + 
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBpol

#Select grid polygons where GCB projects are distributed
pdf("spatialdata/2GCB1k.pdf")
GCBgrid<- Blgrid1k[GCB,]
ggplot() + 
  geom_polygon(data = GCBgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = GCBgrid@data, aes(x = x, y = y),label = GCBgrid$ID, size=1)+
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBgrid
dev.off()

#save pdf with potential areas of interest
pdf("spatialdata/3AOI_CGB1k.pdf")
GCBgridmap<- ggplot()+ 
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)+
  geom_polygon(data = Blgrid1k, aes(x = long, y = lat, group = group), colour = "grey", fill = NA)+
  geom_polygon(data = GCBgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = GCBgrid@data, aes(x = x, y = y),label = GCBgrid$ID, size=0.5, colour ="blue")+
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBgridmap
dev.off()
GCBgridmap

#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid1k[Blgrid1k$ID=='1107',] # [Blgrid1k$ID=='x'|Blgrid1k$ID=='y',]
crs (aoi)<-"EPSG:27700"
CumbriahabAOI<- crop(Cumbriahab, aoi)
plot(aoi+CumbriahabAOI)

# Evaluate Condatis condition ---------------------------------------------

# Set up a raster "template" for AOI raster
aoi@bbox #extent of AOI
ext <- extent(aoi@bbox[1,1],aoi@bbox[1,2],aoi@bbox[2,1],aoi@bbox[2,2]) 

#Select raster resolution
gridsize <- 2 #minimum 5m for 1k grid
#
r <- raster(ext, res=gridsize)

#Evaluate pixel count of AOI, if there are >60000 pixels in the AOI the script will stop
habcount<- rasterize (CumbriahabAOI, r,1) 
habcount<-cellStats(habcount, 'sum')
{
  if (habcount>60000)
    stop("too many cells")
  
  # Rasterize the habitat of AOI
  CHR <- rasterize(CumbriahabAOI, r,'ID')
  #plot(CHR)
  
  # Define habitat quality values in raster
  qual<- as.matrix(read.csv("spatialdata/habitat_quality.csv", header=TRUE))#habitat quality based on Buglife's Resistance Layer and expert opinion  
  hab_qual<-reclassify(CHR, qual) #replace habitat ID with quality value
  
  #save tif for Condatis analysis
  crs (hab_qual)<-"EPSG:27700"
  writeRaster(hab_qual,"spatialdata/habitat1k.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat1k.tif')
}

# Add B-line projects to habitat layer ------------------------------------
#If not interested comment from line below to section 'Create Source and targets for AOI'

GCB$qual<- 1 #add a column with the habitat quality value

# Rasterize the shapefile with the raster "template" (r) used above
GCBr <- rasterize(GCB, r,'qual')
crs (GCBr)<-"EPSG:27700"
plot(GCBr)

# #Add GCBr to hab_qual (habitat.tif)

hab_bl<- raster:::.ifel(GCBr > 0, 1,hab_qual)#if GCBr==1 then change value in hab_qual to 1
plot(hab_bl)

hab_blcount<-raster:::.ifel(hab_bl> 0, 1,hab_bl)
plot(hab_blcount)

{hab_blcount<-cellStats(hab_bl, 'sum')
  if (hab_blcount>60000)
    stop("too many cells")
  
  #save habitat layer with including B-lin projects  
  crs (hab_bl)<-"EPSG:27700"
  writeRaster(hab_bl,"spatialdata/habitatBL1k.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL1k.tif')
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement SOUTH to NORTH
st@nrows
st1<- as.matrix(st)
st1[1,]<-2
st1[st@nrows, ]<-1  

# movement NORTH TO SOUTH
# st1<- as.matrix(st)
# st1[1,]<-1
# st1[st@nrows, ]<-2

# #movement WEST TO EAST
# st1<- as.matrix(st)
# st1[,1]<-1
# st1[st@ncols, ]<-2

#movement EAST TO WEST
# st1<- as.matrix(st)
# st1[,1]<-2
# st1[st@ncols, ]<-1

st[]<-st1

plot(st,col=c("green", "red"), main= 'st.tif')
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/st1k.tif", overwrite=TRUE)

--------------------

  
  
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
hab<- raster("spatialdata/habitat1k.tif") #1k square @ 2m resolution
st<- raster("spatialdata/st1k.tif")
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

write.csv(con, "conductance/test1k.csv")




#Raster of AOI including B-line projects
hab<-raster("spatialdata/habitatBL1k.tif") #1k square @ 2m resolution
st<- raster("spatialdata/st1k.tif")
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

write.csv(con, "conductance/test_bl1k.csv")

# Plot results ------------------------------------------------------------

#Joining results of the conductance of landscapes with ('B-line') and without ('No B-line') B-line project intervention
cond<- data.frame(read.csv("conductance/test1k.csv"))
cond_bl<- data.frame(read.csv("conductance/test_bl1k.csv"))
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




# Estimate change of speed due to intervention ----------------------------

nobl_area<-AUC(conductance$disp_dist, conductance$`No B-line`)
nobl_area
bl_area<-AUC(conductance$disp_dist, conductance$`B-line`)
bl_area
change<-bl_area-nobl_area
perc_change1k<-(change/nobl_area)*100
perc_change1k


