
library(raster)
library(sf)

ex <- as(raster::extent(Blgrid3k), "SpatialPolygons") %>% 
  st_as_sf()
Grid3k<- st_make_grid(ex, cellsize = c(3000, 3000)) %>%
  st_set_crs("EPSG:27700")
Grid3k<-as_Spatial(Grid3k, cast = TRUE, IDs = paste0("ID", seq_along(x)))


Grid3k$ID<-as.character(Grid3k@plotOrder)

#calculate coordinates to label grid
Grid3k@data <- cbind(Grid3k@data, gCentroid(Grid3k,byid = T) %>% coordinates())


ggplot() + 
  geom_polygon(data = Grid3k, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = Grid3k@data, aes(x = x, y = y),label = Grid3k$ID, size=1)+  
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)



#If the interest is to evaluate 'before and after intervention', then add B-line projects map. Otherwise comment from line below to section to #'Clip Area of Interest'

#Read original GCB projects shapefile
GCB <- readOGR("spatialdata", "gcb")
GCBpol<-ggplot() + 
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBpol

#Select grid polygons where GCB projects are distributed
pdf("spatialdata/2GCB3k.pdf")
GCBgrid<- Grid3k[GCB,]
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
  geom_polygon(data = Grid3k, aes(x = long, y = lat, group = group), colour = "grey", fill = NA)+
  geom_polygon(data = GCBgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_text(data = GCBgrid@data, aes(x = x, y = y),label = GCBgrid$ID, size=5, colour ="blue")+
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
GCBgridmap
dev.off()
GCBgridmap

#Clip Area Of Interest, select one or more adjacent grid cells based on ID
aoi<- Grid3k[Grid3k$ID=='180',] # [Grid3k$ID=='x'|Grid3k$ID=='y',]
crs (aoi)<-"EPSG:27700"
CumbriahabAOI<- crop(Cumbriahab, aoi)
plot(aoi+CumbriahabAOI)
