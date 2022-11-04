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
# Create Habitat IDs (to be replaced by quality value below)
hab<- unique(Cumbriahab$LNRNHab)
#create table of Habitat type and respective ID
write.csv(hab, "data/habID.csv")
#assign habitat ID to shapefile polygons
habdf<- data.frame(ID = 1:length(hab), hab = hab)
Cumbriahab$ID <- habdf$ID[match(Cumbriahab$LNRNHab,habdf$hab)]

#Read B-Lines shapefile 
BLines<-readOGR("spatialdata", "BLinesCumbria")

#Create a grid to select a computable landscape, e.g. 3k (3000m)
Blgrid3k<- fishnet(BLines, res = 3000, type = "square")

#Save grid (optional)
#writeOGR(Blgrid3k, "spatialdata", "Blgrid3k", driver = "ESRI Shapefile") 

#add ID grid
Blgrid3k$ID<-as.character(Blgrid3k@plotOrder)

#calculate coordinates to label grid
Blgrid3k@data <- cbind(Blgrid3k@data, gCentroid(Blgrid3k,byid = T) %>% coordinates())


# 180 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


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
  writeRaster(hab_qual,"spatialdata/habitat180.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat180.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL180.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW180.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW180.tif", overwrite=TRUE)


#



# 189 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='189',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat189.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat189.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL189.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW189.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW180.tif", overwrite=TRUE)


#
# 190 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='190',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat190.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat190.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL190.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW190.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW190.tif", overwrite=TRUE)


#
################################################ 205 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='205',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat205.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat205.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL205.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------

st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW205.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW205.tif", overwrite=TRUE)



#
################################################ 206 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='206',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat206.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat206.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL206.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------

st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW206.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW206.tif", overwrite=TRUE)


#
################################################ 207 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='207',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat207.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat207.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL207.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW207.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW207.tif", overwrite=TRUE)

#

################################################ 209 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='209',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat209.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat209.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL209.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW209.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW209.tif", overwrite=TRUE)

#

#
################################################ 215 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='215',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat215.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat215.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL215.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW215.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW215.tif", overwrite=TRUE)


#
################################################ 216 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='216',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat216.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat216.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL216.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW216.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW216.tif", overwrite=TRUE)


################################################ 218 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='218',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat218.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat218.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL218.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA


#movement EAST TO WEST
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW218.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW218.tif", overwrite=TRUE)


#
################################################ 220 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='220',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat220.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat220.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL220.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW220.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW220.tif", overwrite=TRUE)


################################################ 221 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='221',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat221.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat221.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL221.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW221.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW221.tif", overwrite=TRUE)


#
################################################# 231 ---------------------------------------------------------------------

#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")


#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Blgrid3k[Blgrid3k$ID=='231',] # [Blgrid3k$ID=='x'|Blgrid3k$ID=='y',]
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
  writeRaster(hab_qual,"spatialdata/habitat231.tif", overwrite=TRUE)
  
  plot(hab_qual, main='habitat231.tif',col = topo.colors(5,rev = TRUE),zlim=c(0,1))
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
  writeRaster(hab_bl,"spatialdata/habitatBL231.tif", overwrite=TRUE)
  plot(hab_bl,main='habitatBL3k.tif',  col = topo.colors(5,rev = TRUE),zlim=c(0,1))
}

# Create Source and targets for AOI ------------------------------------
st <- raster(ext, res=gridsize) #extent=CumbriahabAOI@bbox, gridsize defined for habitat 
st[]<-NA

#movement EAST TO WEST
st@ncols
st1<- as.matrix(st)
st1[,1]<-2
st1[,st@ncols]<-1

st[]<-st1

plot(st,col=c("magenta", "cyan3"), main= 'stEW231.tif')
image(hab_bl, add = TRUE)
st

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/stEW231.tif", overwrite=TRUE)

#

