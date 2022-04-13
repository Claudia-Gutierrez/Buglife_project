#############PREPARATION OF LAYERS FOR CONDATIS#############
#CLAUDIA GUTIERREZ, APRIL 2022

#This code is designed to obtain the two layers required by Condatis — 'habitat' and 'source & tagets'— in the desired format (*.tif). 
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


# Select Area of Interest -------------------------------------------------


#Read habitat shapefile
Cumbriahab <- readOGR("spatialdata", "CumbriaHab")
# Create Habitat IDs (to be replace by quality value below)
hab<- unique(Cumbriahab$LNRNHab)
habdf<- data.frame(ID = 1:length(hab), hab = hab)
Cumbriahab$ID <- habdf$ID[match(Cumbriahab$LNRNHab,habdf$hab)]

#Read B-Lines shapefile 
BLines<-readOGR("spatialdata", "BLinesCumbria")

#Create a grid to select a computable landscape, e.g. 2k (2000m)
grid<- fishnet(Cumbriahab, res = 2000, type = "square")

#Select grid polygons where B-lines are distributed
Blgrid<-grid[BLines,]

#add ID grid
Blgrid$ID<-as.character(Blgrid@plotOrder)

#calculate coordinates to label grid
Blgrid@data <- cbind(Blgrid@data, gCentroid(Blgrid,byid = T) %>% coordinates())

#visualize potential AOI 

pdf("spatialdata/AOI_BL.pdf")
BLgridmap<-ggplot() + 
  geom_polygon(data = Blgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = Blgrid@data, aes(x = x, y = y),label = Blgrid$ID, size=1)+  
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)
BLgridmap
dev.off()
BLgridmap

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")

#Select grid polygons where GCB projects are distributed
GCBgrid<- Blgrid[GCB,]
ggplot() + 
  geom_polygon(data = GCBgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = GCBgrid@data, aes(x = x, y = y),label = GCBgrid$ID, size=1)+
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

#save pdf with potential areas of interest
pdf("spatialdata/AOI_CGB.pdf")
GCBgridmap<- ggplot()+ 
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)+
  geom_polygon(data = Blgrid, aes(x = long, y = lat, group = group), colour = "grey", fill = NA)+
  geom_polygon(data = GCBgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = GCBgrid@data, aes(x = x, y = y),label = GCBgrid$ID, size=1)+
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBgridmap
dev.off()
GCBgridmap

#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid[Blgrid$ID=='400',] # [Blgrid$ID=='x'|Blgrid$ID=='y',]
crs (aoi)<-"EPSG:27700"
CumbriahabAOI<- Cumbriahab[aoi, ]
plot(CumbriahabAOI)

# Evaluate Condatis condition ---------------------------------------------

# Set up a raster "template" for AOI raster
CumbriahabAOI@bbox #extent of AOI
ext <- extent(CumbriahabAOI@bbox[1,1],CumbriahabAOI@bbox[1,2],CumbriahabAOI@bbox[2,1],CumbriahabAOI@bbox[2,2]) 
gridsize <- 10
r <- raster(ext, res=gridsize)

#Evaluate pixel count of AOI, if there are >60000 pixels in the AOI the script will stop
habcount<- rasterize (CumbriahabAOI, r,1) 
habcount<-cellStats(habcount, 'sum')
{
  if (habcount>60000)
    stop("too many cells")
  
  # Rasterize the habitat of AOI
  CHR <- rasterize(CumbriahabAOI, r,'ID')
  plot(CHR)
  
  # Define habitat quality values in raster
  qual<- as.matrix(read.csv("spatialdata/habitat_quality.csv", header=TRUE))#habitat quality based on Buglife's Resistance Layer and expert opinion  
  hab_qual<-reclassify(CHR, qual) #replace habitat ID with quality value
  
  #save tif for Condatis analysis
  crs (hab_qual)<-"EPSG:27700"
  writeRaster(hab_qual,"spatialdata/habitat.tif", overwrite=TRUE)
  
  plot(hab_qual)
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
  writeRaster(hab_bl,"spatialdata/habitatBL.tif", overwrite=TRUE)
  plot(hab_bl)
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

plot(st,col=c("green", "red"))
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/st.tif", overwrite=TRUE)


