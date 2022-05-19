
library(ggplot2)
library(ggspatial)
library (rgdal)
library(raster)

UKbor<-readOGR("spatialdata", "UK_border")

BLines@bbox

cumbria<- crop(UKbor, BLines@bbox )
plot(cumbria)


SA<- ggplot() + 
  geom_polygon(data = cumbria , aes(x = long, y = lat, group = group), colour = "grey", fill = "grey83")+
  geom_polygon(data = GCB, aes(x = long, y = lat, group = group), colour = "blue", fill = NA)  +
  geom_polygon(data = BLines, aes(x = long, y = lat, group = group), colour = "red", fill = NA)+
  annotation_scale(location = "bl", width_hint = 0.25)
  
SA


