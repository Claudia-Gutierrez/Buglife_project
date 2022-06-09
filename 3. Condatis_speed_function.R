####################################################
#                                                  #
# Condatis Speed Function  (Buglife's adaptation)  #
#                                                  #
####################################################
##Author(s): Jenny Hodgson, Thomas Travers
##Shared by Thomas Travers (February 2022) 
## Adapted by: Claudia Gutierrez (April 2022)

#Citation: Hodgson JA, Wallis DW, Krishna R, et al. 2016. How to manipulate landscapes to improve the potential for range expansion. Methods in Ecology and Evolution 7: 1558-1566.


# This function runs Condatis without generating the raster and shapefile outputs.

# Condatis function (Buglife) -------------------------------------------------------


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
