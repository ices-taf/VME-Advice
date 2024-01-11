st_over <- function (x, y) 
{
  require(sf)
  sapply(sf::st_intersects(x, y), function(z) if (length(z) == 
                                                  0) {
    NA_integer_
  }
  else {
    z[1]
  })
}

########################################################################################

getCSquare <- function(lat, lon, res) {
  Q <- 100000
  MAXLON <- 17999999
  MAXLAT <- 8999999
  OUTLON <- 18000000
  OUTLAT <- 9000000
  
  getX <- function(ilat, ilon) {
    if (ilat >= 0) {
      return(ifelse(ilon >= 0, '1', '7'))
    }
    return(ifelse(ilon >= 0, '3', '5'))
  }
  
  getY <- function(latdigit, londigit) {
    if (latdigit < 5) {
      return(ifelse(londigit < 5, '1', '2'))
    }
    return(ifelse(londigit < 5, '3', '4'))
  }
  
  res <- as.integer(res * Q)
  if (res < 1) {
    res <- 1
  }
  
  result <- sapply(1:length(lat), function(i) {
    ilat <- as.integer(lat[i]*Q)
    ilon <- as.integer(lon[i]*Q)
    sb <- c(getX(ilat, ilon))
    llat <- abs(ilat)
    llon <- abs(ilon)
    
    if (llat >= OUTLAT) {
      llat <- MAXLAT
    }
    
    if (llon >= OUTLON) {
      llon <- MAXLON
    }
    
    q <- 10*Q
    i <- llat %/% q
    j <- llon %/% q
    sb <- c(sb, as.character(i), sprintf("%02d", j))
    
    curr <- 10*Q
    
    while (curr > res) {
      llat <- llat - i*q
      llon <- llon - j*q
      q <- q / 10
      i <- llat %/% q
      j <- llon %/% q
      curr <- curr / 2
      
      sb <- c(sb, ':', getY(i,j))
      if (curr > res) {
        sb <- c(sb, as.character(i), as.character(j))
        curr <- curr / 5
      }
    }
    
    return(paste(sb, collapse = ""))
  })
  
  return(result)
}


##########################################################################################

getCoordinates <- function (cSquare, degrees = 0.05) {
  
  csq.len <- nchar(cSquare)
  gq <- as.numeric(substr(cSquare, 1, 1))
  res <- 10^(1-floor((csq.len-4)/4)) - ((round((csq.len-4)/4, 1) - floor((csq.len-4)/4)) * 10^(1-floor((csq.len-4)/4)))
  glbl.qd.lt <- (round(abs(gq-4)*2, digits = -1)/5) - 1
  glbl.qd.lg <- ((2 * (round(gq, digits = -1)/10)) - 1) * -1
  
  if(degrees == 0.05){
    
    t2 <- as.numeric(substr(cSquare, 2, 2))
    t7 <- as.numeric(substr(cSquare, 7, 7))
    t11 <- as.numeric(substr(cSquare, 11, 11))
    t14 <- as.numeric(substr(cSquare, 14, 14))
    
    m1 <- round(t14 * 2, digits = -1) / 10
    
    
    centre.lat <- ((t2 * 10) + t7 + (t11 * 0.1) + (m1 * 0.05) + 0.025) * glbl.qd.lt
    
    r3 <- as.numeric(substr(cSquare, 3, 4))
    r8 <- as.numeric(substr(cSquare, 8, 8))
    r12 <- as.numeric(substr(cSquare, 12, 12))
    
    m2 <- (round((t14 - 1) / 2, digits = 1) - floor((t14 - 1) / 2)) * 2
    
    centre.lon <- ((r3 * 10) + r8 + (r12 *0.1) + (m2 * 0.05) + 0.025) * glbl.qd.lg
    
  }
  
  return(data.frame("lat" = centre.lat, "lon" = centre.lon))
  
}



##########################################################################################

get_adjacent_csquares <- function(input_csquares, csq_degrees = 0.05, diagonals = T) {
  
    input_points <- vmstools::CSquare2LonLat(input_csquares, degrees= csq_degrees)
    names(input_points) <- c("lat", "lon")
  
    if (diagonals == T ){
    
    adjacent_points <- data.frame(lon = c(input_points$lon + csq_degrees, input_points$lon + csq_degrees,input_points$lon + csq_degrees,
                                          input_points$lon, input_points$lon - csq_degrees, input_points$lon - csq_degrees, 
                                          input_points$lon - csq_degrees, input_points$lon),
                                  lat = c(input_points$lat - csq_degrees, input_points$lat, input_points$lat + csq_degrees,
                                          input_points$lat + csq_degrees, input_points$lat + csq_degrees, input_points$lat,
                                          input_points$lat - csq_degrees, input_points$lat - csq_degrees))
  } else if (diagonals ==F){
    
    adjacent_points <- data.frame(lon = c(input_points$lon + csq_degrees,
                                          input_points$lon, input_points$lon - csq_degrees, 
                                          input_points$lon),
                                  lat = c(input_points$lat, 
                                          input_points$lat + csq_degrees, input_points$lat,
                                          input_points$lat - csq_degrees))
  }
  
  output <- point_to_csquare(lon = adjacent_points$lon,
               lat = adjacent_points$lat, 
               degrees = csq_degrees)
  
  output <- output[output %in% input_csquares == FALSE]
  
  return(output)
  
}

######################################################################################

csquare_buffer <- function(vme.tab){
  
  if("sf" %in% is(vme.tab) == FALSE){print("To run, this function needs to be provided with a table of c-squares in an 'sf' object") 
    next
  }
  
  bboxes <- lapply(st_geometry(vme.tab), st_bbox)
  
  # Create a new rectangle buffered by 0.025 degrees for each original rectangle
  buffered_rects <- lapply(bboxes, function(bbox) {
    st_bbox(c(xmin = bbox[["xmin"]] - 0.02501, 
              ymin = bbox[["ymin"]] - 0.02501, 
              xmax = bbox[["xmax"]] + 0.02501, 
              ymax = bbox[["ymax"]] + 0.02501), 
            crs = st_crs(vme.tab))
  })
  
  # Convert the bboxes to rectangular polygons
  buffered_rects <- lapply(buffered_rects, st_as_sfc)
  buffered_rects <- do.call(c, buffered_rects)
  buffered_rects <- st_sf(geometry = buffered_rects)
  
  out.tab <- vme.tab
  
  st_geometry(out.tab) <- st_geometry(buffered_rects)
  
  
  return(out.tab)
  
}
