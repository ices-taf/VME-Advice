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
  
    # input_points <- vmstools::CSquare2LonLat(input_csquares, degrees= csq_degrees)
    input_points <- getCoordinates(input_csquares, degrees = csq_degrees)
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
  
  output <- getCSquare(lon = adjacent_points$lon,
               lat = adjacent_points$lat, 
               res = csq_degrees)
  
  output <- output[output %in% input_csquares == FALSE]
  
  return(output)
  
}

get_isolated_csquares <- function(csquares) {
  
  if(class(csquares) == "character"){
    ids <- csquares
  } else if(class(csquares) == "data.frame") {
    stopifnot("csquares" %in% colnames(csquares), "Column with name 'csquares' must be present")
    ids <- csquares$csquares
  }

  list_adjacents <- purrr::pmap(vme_elements, ~ get_adjacent_csquares(.x, csq_degrees = 0.05, diagonals = T))
  list_neighbours_within_input <- purrr::map(list_adjacents, ~ .x[.x %in% ids])
  isolated <- purrr::map_lgl(list_neighbours_within_input, ~is.null(.x)) 
  ids[isolated]
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

vme_scenario_A <- function(vme_index) {
  
  low_index <- dplyr::filter(vme_index, VME_Class == 0) # index low
  
  habitat_plus_high_medium_index <- dplyr::filter(vme_index, VME_Class %in% c(3,2,1)) #Habitat, Index high, index medium
  
  adjacent_csquares <- get_adjacent_csquares(habitat_plus_high_medium_index, diagonals = T, csq_degrees = 0,05)
  ## I have assumed "adjacent" means diagonally as well as above/below/left/right. If not, change this bit.
  
  adjacent_low_index <- dplyr::filter(low_index, csquares %in% adjacent_csquares)
  
  ## bring them back together again and select unique
  scenario_csquares <- dplyr::bind_rows(habitat_plus_high_medium_index, adjacent_low_index) %>% 
    dplyr::pull(csquares) %>% 
    unique() 
}

vme_scenario_B <- function(vme_index, vme_elements) {
  
  csq_elements_w_records <- dplyr::pull(vme_elements, csquares)
  
  #identify isolated elements with records
  isolated_csquares <- get_isolated_csquares(csq_elements_w_records)
  isolated_vme <- dplyr::filter(vme_index, csquares %in% isolated_csquares)
  vme_outside_elements <- dplyr::filter(vme_index, !csquares %in% elements_w_records)
  
  scenario_B_input <- dplyr::bind_rows(isolated_vme, vme_outside_elements)

  vme_scenario_A(scenario_B_input)
}

vme_scenario_C <- function(vme_index, sar_layer) {
  
  #This step could potentially be extracted to data.R 
  vme_index <- cbind(vme_index, "SAR" = sar_layer[match(vme_index$csquares,sar_layer$c.square), "SAR"])
  vme_index$SAR[is.na(vme_index$SAR)] <- 0
  
  habitat_plus_high_medium_index <- dplyr::filter(vme_index, VME_Class %in% c(3,2,1)) #Habitat, Index high, index medium
  
  low_index <- dplyr::filter(vme_index, VME_Class == 0) # index low
  low_index_low_fishing <- dplyr::filter(low_index, SAR < SAR_threshold)
  low_index_high_fishing <- dplyr::filter(low_index, SAR >= SAR_threshold)
  
  candidate_vmes <- dplyr::bind_rows(habitat_plus_high_medium_index, low_index_low_fishing)
  
  
  adjacent_csquares <- get_adjacent_csquares(candidate_vmes, csq_degrees = 0.05, diagonals = T)
  adjacent_low_index_high_fishing <- dplyr::filter(low_index_high_fishing, csquares %in% adjacent_csquares)
  
  ## then bind together unique csquares into output
  scenario_csquares <- dplyr::bind_rows(candidate_vmes, adjacent_low_index_high_fishing) %>% 
    dplyr::pull(csquares) %>% 
    unique()
}

vme_scenario_D <- function(vme_index, sar_layer) {
  
  #This step could potentially be extracted to data.R 
  vme_index <- cbind(vme_index, "SAR" = sar_layer[match(vme_index$csquares,sar_layer$c.square), "SAR"])
  vme_index$SAR[is.na(vme_index$SAR)] <- 0
  
  VME_below_SAR_thresh <- vme_index[vme_index$SAR < SAR_threshold,]  # select all below SAR threshold
  
  adjacent_csquares <- get_adjacent_csquares(VME_below_SAR_thresh, csq_degrees = 0.05, diagonals = T)
  
  ## bring them back together again and select unique
  scenario_csquares <- VME_below_SAR_thresh %>% 
    dplyr::pull(csquares) %>% 
    c(adjacent_csquares) %>%
    unique() 
  }