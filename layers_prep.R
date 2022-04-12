library (rgdal)
library(raster)
library(terra)
library(phyloregion)
library(ggplot2)

#######LANDSCAPE CUMBRIA NATURAL RECOVERY HABITATS#############

########
#Spatial data needed:
#Region of interest: B-Lines shapefile
#Habitat: Cumbria habitat shapefile
#B-line projects: Get Cumbria Buzzing project shapefile

########

#Read original habitat shapefile
Cumbriahab <- readOGR("spatialdata", "CumbriaHab")
# Place Habitats IDs
hab<- unique(Cumbriahab$LNRNHab)
habdf<- data.frame(ID = 1:length(hab), hab = hab)
Cumbriahab$ID <- habdf$ID[match(Cumbriahab$LNRNHab,habdf$hab)]

#Read B-Lines shapefile 
BLines<-readOGR("spatialdata", "BLinesCumbria")


#Create a grid to select a smaller landscape e.g. 2k
grid<- fishnet(Cumbriahab, res = 2000, type = "square")

#Select grid polygons where B-lines are distributed
Blgrid<-grid[BLines,]
#add ID grid
Blgrid$ID<-Blgrid@plotOrder

ggplot() + geom_polygon(data = Blgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)



#Clip Area Of Interest
aoi <- grid[grid$Id=='430',]
plot(aoi)

CumbriahabAOI<- Cumbriahab[aoi, ]

## Set up a raster "template" for a 10 m grid
aoi@bbox #extent of AOI
ext <- extent(299941.2,301941.2,523082.8,525082.8) #aoi@bbox
gridsize <- 10
r <- raster(ext, res=gridsize)

## Rasterize the shapefile
CHR <- rasterize(Cumbriahab, r,'ID')
plot(CHR)

## Define habitat quality values in raster

qual<- as.matrix(read.csv("spatialdata/habitat_quality.csv", header=TRUE))#habitat quality based on Buglife's Resistance Layer and expert opinion  

hab_qual<-reclassify(CHR, qual) #replace habitat ID with quality value

#save tif for Condatis analysis
crs (hab_qual)<-"EPSG:27700"
writeRaster(hab_qual,"spatialdata/habitat.tif", overwrite=TRUE)

plot(hab_qual)

#######LANDSCAPE INCLUDING 'GET CUMBRIA BUZZING (GCB)' PROJECTS#############

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")

GCB$qual<- 1 #add a column with the habitat quality value

## Rasterize the shapefile with the raster "template" (r) used above
GCBr <- rasterize(GCB, r,'qual')
crs (GCBr)<-"EPSG:27700"

plot(GCBr)

###Add GCBr to hab_qual (habitat.tif)

hab_bl<- raster:::.ifel(GCBr > 0, 1,hab_qual)#if GCBr==1 then change value in hab_qual to 1

crs (hab_bl)<-"EPSG:27700"
writeRaster(hab_bl,"spatialdata/habitatBL.tif", overwrite=TRUE)

plot(hab_bl)

###########CREATE SOURCE AND TARGET RASTER#############

st <- raster(ext, res=gridsize) #extent=aoi@bbox, gridsize defined for habitat 
st[]<-NA

#movement SOUTH to NORTH
st@nrows
st1<- as.matrix(st)
st1[1:2,]<-2
st1[199:200, ]<-1 # st@nrows-1:st@nrows   
 
# #movement NORTH TO SOUTH
# st1<- as.matrix(sandt)
# st1[1:2,]<-1
# st1[399:400, ]<-2 # st@nrows-1:st@nrows   
# #movement WEST TO EAST
#st@ncols
# st1<- as.matrix(sandt)
# st1[,1:2]<-1
# st1[,491:492]<-2 # st@ncols-1:st@ncols 
# #movement EAST TO WEST
# st1<- as.matrix(sandt)
# st1[,1:2]<-1
# st1[,491:492]<-2 # st@ncols-1:st@ncols 

st[]<-st1

plot(st,col=grey(1:10/10))

crs (st)<-"EPSG:27700"
writeRaster(st,"spatialdata/st.tif", overwrite=TRUE)


