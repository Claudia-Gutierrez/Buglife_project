################################################
#                                              #
#     PREPARATION OF LAYERS FOR CONDATIS       #
#       *WITHIN AND OUTSIDE B-LINES*           #    
#                                              #
################################################
## CLAUDIA GUTIERREZ, APRIL 2022 ###

#This script is designed to obtain the two layers required by Condatis — 'habitat' and 'source & tagets'— in '*.tif' format 
#The user can choose the Area of Interest (AOI) and resolution of the layers

#Processing is limited to a maximum 60,000 (six thousand) cells in the study area. A section of the code allows to validate the condition of <60000 cells to either proceed with the analysis or reset the extent and resolution of the AOI. 

# DATA REQUIRED
#1. Habitat shapefile: e.g. Cumbria Local Nature Recovery Habitats Basemap
#2. B-Line shapefile
#3. B-Line projects shapefile: e.g. 'Get Cumbria Buzzing projects' map

######################################################################

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

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")
GCBpol<-ggplot() + 
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBpol

#Create a grid to select a computable landscape, e.g. 3k (3000m)
GCBgrid<- fishnet(GCB, res = 3000, type = "square")

#Save grid (optional)
writeOGR(GCBgrid, "spatialdata", "GCBgrid3k", driver = "ESRI Shapefile") 


#add ID grid
GCBgrid$ID<-as.character(GCBgrid@plotOrder)

#calculate coordinates to label grid
GCBgrid@data <- cbind(GCBgrid@data, gCentroid(GCBgrid,byid = T) %>% coordinates())

#visualize potential AOI 

pdf("spatialdata/1AOI_WO.pdf")
GCBgridmap<-ggplot() + 
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_polygon(data = GCBgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = GCBgrid@data, aes(x = x, y = y),label = GCBgrid$ID, size=2)+  
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)
GCBgridmap
dev.off()
GCBgridmap


#Clip Area Of Interest 1, select one or more adjacent grid cells based on ID
aoi1<- GCBgrid[GCBgrid$ID=='17',] # [GCBgrid$ID=='x'|GCBgrid$ID=='y',]
crs (aoi1)<-"EPSG:27700"
CumbriahabAOI<- crop(Cumbriahab, aoi1)
plot(aoi1+CumbriahabAOI)

# Evaluate Condatis condition ---------------------------------------------

# Set up a raster "template" for AOI raster
aoi1@bbox #extent of aoi1
ext <- extent(aoi1@bbox[1,1],aoi1@bbox[1,2],aoi1@bbox[2,1],aoi1@bbox[2,2]) 

#Select raster resolution
gridsize <- 10 
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
  
  plot(hab_qual, main='habitat1.tif', col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Add B-line projects to habitat layer ------------------------------------

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
  
  #save habitat 1 layer within B-line  
  crs (hab_bl)<-"EPSG:27700"
  writeRaster(hab_bl,"spatialdata/habitatW.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatW.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI1 ------------------------------------
stW <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
stW[]<-NA

#movement SOUTH to NORTH
stW@nrows
stW1<- as.matrix(stW)
stW1[1,]<-2
stW1[stW@nrows, ]<-1  

# movement NORTH TO SOUTH
# stW1<- as.matrix(stW)
# stW1[1,]<-1
# stW1[stW@nrows, ]<-2

# #movement WEstW TO EAstW
# stW1<- as.matrix(stW)
# stW1[,1]<-1
# stW1[stW@ncols, ]<-2

#movement EAstW TO WEstW
# stW1<- as.matrix(stW)
# stW1[,1]<-2
# stW1[stW@ncols, ]<-1

stW[]<-stW1

plot(stW,col=c("magenta", "cyan3"), main= 'stW.tif')
stW

crs (stW)<-"EPSG:27700"
writeRaster(stW,"spatialdata/stW.tif", overwrite=TRUE)


###
#Clip Area Of Interest 2, select one or more adjacent grid cells based on ID
aoi2<- GCBgrid[GCBgrid$ID=='7',] # [GCBgrid$ID=='x'|GCBgrid$ID=='y',]
crs (aoi2)<-"EPSG:27700"
CumbriahabAOI<- crop(Cumbriahab, aoi2)
plot(aoi2+CumbriahabAOI)

# Evaluate Condatis condition ---------------------------------------------

# Set up a raster "template" for AOI raster
aoi2@bbox #extent of aoi2
ext <- extent(aoi2@bbox[1,1],aoi2@bbox[1,2],aoi2@bbox[2,1],aoi2@bbox[2,2]) 

#Select raster resolution
gridsize <- 10 
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
  
  plot(hab_qual, main='habitat2.tif', col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Add B-line projects to habitat layer ------------------------------------

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
  
  #save habitat 2 layer outside B-line projects  
  crs (hab_bl)<-"EPSG:27700"
  writeRaster(hab_bl,"spatialdata/habitatO.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatO.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI2 ------------------------------------
stO <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
stO[]<-NA

#movement SOUTH to NORTH
stO@nrows
stO1<- as.matrix(stO)
stO1[1,]<-2
stO1[stO@nrows, ]<-1  

# movement NORTH TO SOUTH
# stO1<- as.matrix(stO)
# stO1[1,]<-1
# stO1[stO@nrows, ]<-2

# #movement WEstO TO EAstO
# stO1<- as.matrix(stO)
# stO1[,1]<-1
# stO1[stO@ncols, ]<-2

#movement EAstO TO WEstO
# stO1<- as.matrix(stO)
# stO1[,1]<-2
# stO1[stO@ncols, ]<-1

stO[]<-stO1

plot(stO,col=c("magenta", "cyan3"), main= 'stO.tif')
stO

crs (stO)<-"EPSG:27700"
writeRaster(stO,"spatialdata/stO.tif", overwrite=TRUE)



