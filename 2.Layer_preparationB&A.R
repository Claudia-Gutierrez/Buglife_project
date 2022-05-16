################################################
#                                              #
#     PREPARATION OF LAYERS FOR CONDATIS       #
#      *BEFORE AND AFTER INTERVENTION*         #    
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

library(rgdal)
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

#Create a grid to select a computable landscape, e.g. 3k (3000m)
Blgrid3k<- fishnet(BLines, res = 3000, type = "square")

#add ID grid
Blgrid3k$ID<-as.character(Blgrid3k@plotOrder)

#calculate coordinates to label grid
Blgrid3k@data <- cbind(Blgrid3k@data, gCentroid(Blgrid3k,byid = T) %>% coordinates())

#visualize potential AOI 

pdf("spatialdata/1AOI_BL3k.pdf")
Blgrid3kmap<-ggplot() + 
  geom_polygon(data = Blgrid3k, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = Blgrid3k@data, aes(x = x, y = y),label = Blgrid3k$ID, size=1)+  
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)
Blgrid3kmap
dev.off()
Blgrid3kmap

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")
GCBpol<-ggplot() + 
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBpol

#Select grid polygons where GCB projects are distributed
pdf("spatialdata/2GCB3k.pdf")
GCBgrid<- Blgrid3k[GCB,]
ggplot() + 
  geom_polygon(data = GCBgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = GCBgrid@data, aes(x = x, y = y),label = GCBgrid$ID, size=1)+
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBgrid
dev.off()

#save pdf with potential areas of interest
pdf("spatialdata/3AOI_CGB3k.pdf")
GCBgridmap<- ggplot()+ 
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)+
  geom_polygon(data = Blgrid3k, aes(x = long, y = lat, group = group), colour = "grey", fill = NA)+
  geom_polygon(data = GCBgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = GCBgrid@data, aes(x = x, y = y),label = GCBgrid$ID, size=0.5, colour ="blue")+
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBgridmap
dev.off()
GCBgridmap

#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='180',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
crs (aoi)<-"EPSG:27700"
CumbriahabAOI<- crop(Cumbriahab, aoi)
plot(aoi+CumbriahabAOI)

# Evaluate Condatis condition ---------------------------------------------

# Set up a raster "template" for AOI raster
aoi@bbox #extent of AOI
ext <- extent(aoi@bbox[1,1],aoi@bbox[1,2],aoi@bbox[2,1],aoi@bbox[2,2]) 

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
  
  #save tif for Condatis analysis
  crs (hab_qual)<-"EPSG:27700"
  writeRaster(hab_qual,"spatialdata/habitat3k.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat3k.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL3k.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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

plot(st,col=c("magenta", "cyan3"), main= 'st.tif')
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/st3k.tif", overwrite=TRUE)

