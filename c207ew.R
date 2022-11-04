######## Condatis function (Buglife) -------------------------------------------------------


CondatisNR <- function(hab, st, R, disp){
  
  library(raster)
  library(sf)
  library(rgdal)
  library(dplyr)
  library(maptools)
  
  smart.round <- function(x) { #this function rounds numeric to integer while preserving their sum 
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y
  }
  
  # Oporator that  is the oposite of %in%
  `%!in%` = Negate(`%in%`)
  
  # Check if the habitat is in meters, and if it is make sure the cellside etc is divided by 1000
  
  if (grepl('units=m', hab@crs@projargs)){
    
    scaler <- 1000
    
  } else {
    
    scaler <- 1
    
  }
  
  amap <- hab
  
  # Take the habitat raster, convert to a dataframe
  apt <- as.data.frame(rasterToPoints(amap, fun = function(x){!is.na(x)}, spatial = F))
  names(apt) <- c('xm', 'ym', 'cover')
  apt <- apt[apt$cover>0,]
  apt$x <- apt$xm/scaler # Create new columns for coordinates in km #
  apt$y <- apt$ym/scaler
  cellside <- xres(amap)/scaler
  
  #convert st raster to dataframe#
  st <- as.data.frame(rasterToPoints(st,fun = function(x){!is.na(x)}, spatial = F))
  names(st) <- c('xm', 'ym', 'label')
  st$x <- st$xm/scaler
  st$y <- st$ym/scaler
  
  #Get the distances between each cell of habitat and every other cell
  len<-dim(apt)[1]
  dm <- dist(apt[, c('x','y')])
  
  #Get x and y coordinates for sources and targets
  origin <- st[st$label == 1, c('x','y')]
  target <- st[st$label == 2, c('x','y')]
  
  # Define alpha (mean dispersal) and normalisation so the area under the dispersal kernel integrates to 1
  alpha <- 2/disp
  norm <- R*alpha^2/2/pi*cellside^4 
  
  #### Core Condatis Calculations ####
  
  #Current between cells
  Cfree <- norm*outer(apt$cover, apt$cover, '*')*exp(-alpha*as.matrix(dm))
  diag(Cfree) <- 0
  
  #Current into a cell and out of a cell
  Cin <- norm*apt$cover*
    rowSums(exp(-alpha*sqrt(outer(apt[, 'x'], origin[, 'x'], '-')^2 +
                              outer(apt[, 'y'], origin[, 'y'], '-')^2)))
  
  Cout <- norm*apt$cover*
    rowSums(exp(-alpha*sqrt(outer(apt[, 'x'], target[, 'x'], '-')^2 +
                              outer(apt[, 'y'], target[, 'y'], '-')^2)))
  M0 <- diag(Cin + Cout + rowSums(Cfree)) - Cfree
  w <- Cin - Cout
  v0 <- solve(M0, w, tol = exp(-256)) # This produces the resistance values in the network and is where errors will most likely occur, see 'try' function for error handling
  
  I0 <- (v0 + 1) * Cout
  I1 <- (1 - v0) * Cin
  
  #Conductance value of whole network 
  cond <- (sum(I0) + sum(I1))/4 #4 to make overall voltage difference 1 rather than 2
  
  # Save conductance and respective dispersal distance
  C <- cbind(disp=disp, conductance = cond)
  
  #clean up to save memory - if sure no longer needed
  rm(Cfree)
  gc()
  
  # Return results #
  results <- list(C)
  names(results) <- c('conductance')
  return(results)
}

library(raster)
library(sf)
library(tidyverse)
library(rgdal)
library(dplyr)
library(ggplot2)
library(maptools)
library(scales)
library(DescTools)


############## 207 ---------------------------------------------------------------------
# Run Condatis with dispersal distance iteration --------------------------

#Raster of AOI without B-line project
hab<- raster("spatialdata/habitat207.tif") #207 grid @ 10m resolution
st<- raster("spatialdata/stEW207.tif")
R<-1000

#Range defined between the minimum distance for bees and maximum distance between the source and target[Dispersal distance for bees and hoverflies: 0.015-10.4km; and for moths 0.00043-81.1km]
disper <-c(10^seq(-1.8,0.5,0.1))


test_result<-data.frame()
for(i in disper) {
  test<-CondatisNR(hab=hab, st=st, R=R, disp=i)
  test_result<- rbind(test_result, test$conductance)
}

colnames(test_result)<-c("disp" , "conductance")

con<- data.frame(test_result %>%
                   group_by(disp)%>%
                   summarise(Conduct = mean(conductance)))

write.csv(con, "conductanceEW/test207ew.csv")



#Raster of AOI including B-line projects
hab<-raster("spatialdata/habitatBL207.tif") #207 grid @ 10m resolution
st<- raster("spatialdata/stEW207.tif")
R<-1000

#Range defined between the minimum distance for bees and maximum distance between the source and target[Dispersal distance for bees and hoverflies: 0.015-10.4km; and for moths 0.00043-81.1km]
disper <-c(10^seq(-1.8,0.5,0.1))

test_result<-data.frame()
for(i in disper) {
  test<-CondatisNR(hab=hab, st=st, R=R, disp=i)
  test_result<- rbind(test_result, test$conductance)
}

colnames(test_result)<-c("disp" , "conductance")

con<- data.frame(test_result %>%
                   group_by(disp)%>%
                   summarise(Conduct = mean(conductance)))

write.csv(con, "conductanceEW/test207ew_bl.csv")



# Conductance file --------------------------------------------------------

#Joining results of the conductance of landscapes with ('B-line') and without ('No B-line') B-line project intervention Plot results 

cond<- data.frame(read.csv("conductanceEW/test207ew.csv"))
cond_bl<- data.frame(read.csv("conductanceEW/test207ew_bl.csv"))
conductance<-data.frame(cond$disp, cond$Conduct, cond_bl$Conduct)
colnames(conductance)<-c('disp_dist', 'before GCB','after GCB')
write.csv(conductance, "conductanceEW/test207ew_diff.csv")
