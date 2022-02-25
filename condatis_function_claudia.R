################################################
#                                              #
#                Condatis Function             #
#                                              #
################################################

# The function needs the following inputs:

    # Hab - raster of the habitat you wish to measure connectivity over
    # st - raster of location of sources and targets
    # R - R value of the species moving (number of movers produced per km^2 of habitat)
    # powerthresh - value between 0 and 1, used to define what a bottleneck is ( selects bottlenecks that account for x proportion of the total power in the network)
    # disp - Dispersal value of the species in km 

Condatis <- function(hab, st, R, powerthresh, disp){
  
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
  
  # Operator that  is the opposite of %in%
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
  
  ### flow by cell ###
  flo <- apply(Cfree * outer(v0, v0, '-'), 1, function(x) {
    sum(abs(x))
  })
  
  flo <- ((flo)/2 + I0 + I1)/2
  
  # combine progress, flow, standardised flow, and conductance into a data.frame for saving later
  f <- cbind(apt, progress = (v0 + 1)/2, flow = flo, std_flow = flo/max(flo), conductance = cond)
  
  # Create shapefile of standardised flow values (can be skipped if not needed)
  f_shp <- SpatialPointsDataFrame(f[, c('xm', 'ym')], f[, 4:ncol(f)],
                                  proj4string = crs(amap)) %>%
    as('sf') %>%
    st_buffer(dist = (xres(amap)/2), endCapStyle = 'SQUARE')
  
  # Create a raster of standardised flow and the 'progress' statistic (can be skipped if not needed)
  r <- raster(extent(amap), res = xres(amap), crs = crs(f_shp))
  r_f <- rasterize(f_shp, r, field = 'std_flow')
  r_p <- rasterize(f_shp, r, field = 'progress')
  
  ## Power calculations - for bottlenecks ##
  
  powr <- Cfree * outer(v0, v0, '-')^2
  
  powlong <- data.frame(
    a = c(matrix(1:len, nrow = len, ncol = len)[upper.tri(powr)]),
    b = c(matrix(1:len, nrow = len, ncol = len, byrow = T)[upper.tri(powr)]),
    powr=c(powr[upper.tri(powr)])
  )
  
  
  powlong <- powlong[order(-powlong$powr), ]#sorting the data frame so highest power comes first
  sumpow <- sum(powlong$powr)
  powlong$thresh <- cumsum(powlong$powr)/sumpow 
  
  # subset the power scores by the powerthreshold you have set
  if (nrow(subset(powlong, powlong$thresh <= powerthresh)) == 0){
    powlong <- powlong[1:5,]
  } else if (nrow(subset(powlong, powlong$thresh <= powerthresh)) > 10000) {
    powlong <- powlong[1:10000,]
  } else {
    powlong <- subset(powlong, powlong$thresh <= powerthresh)
  }
  
  #create dataframe of power scores and location of ends of the bottleneck for later
  powlong$label <- paste(powlong$a, powlong$b, sep = '_')
  powpoints <- cbind(apt[powlong$a, c('xm', 'ym')], powlong[,c('label')], type = 'a')
  powpoints <- rbind(powpoints, cbind(apt[powlong$b, c('xm', 'ym')], powlong[, c('label')], type = 'b'))
  names(powpoints) <- c('xm', 'ym', 'label', 'type')
  
  #clean up to save memory - if sure no longer needed
  rm(Cfree)
  rm(powr)
  gc()
  
  #### create shapefile of the location of the top bottlenecks ####
  
  powers <- powlong[,c(3,5)]
  pow <- full_join(powpoints, powers, by = 'label')
  
  power <- left_join(subset(pow, type == 'b')[,-c(4,5)], subset(pow,type == 'a')[,-4], by = 'label')[,c(1,2,4,5,3,6)]
  names(power) <- c('x1', 'y1','x2', 'y2', 'label', 'power') 
  
  listpows <- split(pow, f = pow$label)
  
  noms <- names(listpows)
  listlines <- setNames(lapply(listpows, function(x)
    SpatialLines(list(Lines(list(Line(x[, c(1,2)])), ID = unique(x$label))))),
    paste0(unlist(noms)))
  
  
  joined = SpatialLines(lapply(listlines, function(x){x@lines[[1]]}))
  proj4string(joined) <- as.character(crs(r))
  
  jdata = SpatialLinesDataFrame(joined, data.frame(id = names(joined), power = unlist(lapply(listpows, function(x) x[1,5]))), FALSE)
  
  # Retunr results #
  results <- list(f, r_f, f_shp, r_p, power, jdata)
  names(results) <- c('flow', 'flow_raster', 'flow_shp', 'progress_raster', 'power', 'bottlenecks')
  return(results)
}



