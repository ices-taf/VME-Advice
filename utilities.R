# Helper function for spatial intersection
# Returns the first intersection index or NA if no intersection exists
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

#' Gets the c-square identifier for given latitude-longitude coordinates
#' C-squares are a grid-based system for location referencing
#' 
#' @param lat Vector of latitude values
#' @param lon Vector of longitude values
#' @param res Resolution of the c-square grid
#' @return Vector of c-square identifiers
#' @examples 
#' getCSquare(c(0,0), c(0, 10), 10)
#' getCSquare(c(0,0), c(0, 10), 1)
getCSquare <- function(lat, lon, res) {
  # Constants for c-square calculations
  Q <- 100000  # Scaling factor
  MAXLON <- 17999999  # Maximum longitude value
  MAXLAT <- 8999999   # Maximum latitude value
  OUTLON <- 18000000  # Out of bounds longitude
  OUTLAT <- 9000000   # Out of bounds latitude
  
  # Helper function to determine first digit based on quadrant
  getX <- function(ilat, ilon) {
    if (ilat >= 0) {
      return(ifelse(ilon >= 0, '1', '7'))
    }
    return(ifelse(ilon >= 0, '3', '5'))
  }
  
  # Helper function to determine position within quadrant
  getY <- function(latdigit, londigit) {
    if (latdigit < 5) {
      return(ifelse(londigit < 5, '1', '2'))
    }
    return(ifelse(londigit < 5, '3', '4'))
  }
  
  # Convert resolution to integer and ensure minimum value
  res <- as.integer(res * Q)
  if (res < 1) {
    res <- 1
  }
  
  # Process each lat/lon pair
  result <- sapply(1:length(lat), function(i) {
    # Convert coordinates to integer format
    ilat <- as.integer(lat[i]*Q)
    ilon <- as.integer(lon[i]*Q)
    sb <- c(getX(ilat, ilon))
    llat <- abs(ilat)
    llon <- abs(ilon)
    
    # Handle out of bounds values
    if (llat >= OUTLAT) {
      llat <- MAXLAT
    }
    if (llon >= OUTLON) {
      llon <- MAXLON
    }
    
    # Calculate initial position
    q <- 10*Q
    i <- llat %/% q
    j <- llon %/% q
    sb <- c(sb, as.character(i), sprintf("%02d", j))
    
    curr <- 10*Q
    
    # Recursively subdivide until desired resolution
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

#' Convert c-square identifier to centroid coordinates
#' @param cSquare C-square identifier
#' @param degrees Resolution in degrees
#' @return Data frame with latitude and longitude
getCoordinates <- function (cSquare, degrees = 0.05) {
  # Extract components from c-square identifier
  csq.len <- nchar(cSquare)
  gq <- as.numeric(substr(cSquare, 1, 1))
  
  # Calculate resolution
  res <- 10^(1-floor((csq.len-4)/4)) - ((round((csq.len-4)/4, 1) - floor((csq.len-4)/4)) * 10^(1-floor((csq.len-4)/4)))
  
  # Calculate global quadrant adjustments
  glbl.qd.lt <- (round(abs(gq-4)*2, digits = -1)/5) - 1
  glbl.qd.lg <- ((2 * (round(gq, digits = -1)/10)) - 1) * -1
  
  if(degrees == 0.05){
    # Extract digits for precise calculations
    t2 <- as.numeric(substr(cSquare, 2, 2))
    t7 <- as.numeric(substr(cSquare, 7, 7))
    t11 <- as.numeric(substr(cSquare, 11, 11))
    t14 <- as.numeric(substr(cSquare, 14, 14))
    
    m1 <- round(t14 * 2, digits = -1) / 10
    
    # Calculate latitude
    centre.lat <- ((t2 * 10) + t7 + (t11 * 0.1) + (m1 * 0.05) + 0.025) * glbl.qd.lt
    
    # Extract longitude components
    r3 <- as.numeric(substr(cSquare, 3, 4))
    r8 <- as.numeric(substr(cSquare, 8, 8))
    r12 <- as.numeric(substr(cSquare, 12, 12))
    
    m2 <- (round((t14 - 1) / 2, digits = 1) - floor((t14 - 1) / 2)) * 2
    
    # Calculate longitude
    centre.lon <- ((r3 * 10) + r8 + (r12 *0.1) + (m2 * 0.05) + 0.025) * glbl.qd.lg
  }
  
  return(data.frame("lat" = centre.lat, "lon" = centre.lon))
}

#' Find adjacent c-squares for given input c-squares
#' @param input_csquares Vector of c-square identifiers
#' @param csq_degrees Resolution in degrees
#' @param diagonals Include diagonal adjacency
#' @return Vector of adjacent c-square identifiers
get_adjacent_csquares <- function(input_csquares, csq_degrees = 0.05, diagonals = T) {
  # Get coordinates for input c-squares
  input_points <- getCoordinates(input_csquares, degrees = csq_degrees)
  names(input_points) <- c("lat", "lon")
  
  # Calculate adjacent points based on whether diagonals are included
  if (diagonals == T ){
    adjacent_points <- data.frame(
      lon = c(input_points$lon + csq_degrees, input_points$lon + csq_degrees, 
              input_points$lon + csq_degrees, input_points$lon, 
              input_points$lon - csq_degrees, input_points$lon - csq_degrees,
              input_points$lon - csq_degrees, input_points$lon),
      lat = c(input_points$lat - csq_degrees, input_points$lat, 
              input_points$lat + csq_degrees, input_points$lat + csq_degrees,
              input_points$lat + csq_degrees, input_points$lat,
              input_points$lat - csq_degrees, input_points$lat - csq_degrees))
  } else {
    adjacent_points <- data.frame(
      lon = c(input_points$lon + csq_degrees, input_points$lon,
              input_points$lon - csq_degrees, input_points$lon),
      lat = c(input_points$lat, input_points$lat + csq_degrees,
              input_points$lat, input_points$lat - csq_degrees))
  }
  
  # Convert adjacent points to c-squares
  output <- getCSquare(lon = adjacent_points$lon,
                       lat = adjacent_points$lat, 
                       res = csq_degrees)
  
  # Remove any that match input c-squares
  output <- output[output %in% input_csquares == FALSE]
  
  return(output)
}

#' Identify isolated c-squares (those with no neighbors in the input set)
#' @param csquares Vector of c-square identifiers or data frame with csquares column
#' @return Vector of isolated c-square identifiers
get_isolated_csquares <- function(csquares) {
  # Handle input format
  if (class(csquares) == "character") {
    ids <- csquares
  } else if (class(csquares) == "data.frame") {
    stopifnot("csquares" %in% colnames(csquares), "Column with name 'csquares' must be present")
    ids <- csquares$csquares
  }
  
  # Get adjacent squares for each input square
  list_adjacents <- purrr::map(ids, ~ get_adjacent_csquares(.x, csq_degrees = 0.05, diagonals = T))
  # Find which adjacents are in the input set
  list_neighbours_within_input <- purrr::map(list_adjacents, ~ .x[.x %in% ids])
  # Identify squares with no neighbors in input set
  isolated <- purrr::map_lgl(list_neighbours_within_input, ~ length(.x) == 0)
  
  ids[isolated]
}

#' Create buffer around c-squares
#' @param vme.tab sf object containing c-squares
#' @return sf object with buffered geometries
csquare_buffer <- function(vme.tab){
  if("sf" %in% is(vme.tab) == FALSE){
    print("To run, this function needs to be provided with a table of c-squares in an 'sf' object") 
    next
  }
  
  # Get bounding boxes
  bboxes <- lapply(st_geometry(vme.tab), st_bbox)
  
  # Create buffered rectangles
  buffered_rects <- lapply(bboxes, function(bbox) {
    st_bbox(c(xmin = bbox[["xmin"]] - 0.02501, 
              ymin = bbox[["ymin"]] - 0.02501, 
              xmax = bbox[["xmax"]] + 0.02501, 
              ymax = bbox[["ymax"]] + 0.02501), 
            crs = st_crs(vme.tab))
  })
  
  # Convert to sf objects
  buffered_rects <- lapply(buffered_rects, st_as_sfc)
  buffered_rects <- do.call(c, buffered_rects)
  buffered_rects <- st_sf(geometry = buffered_rects)
  
  out.tab <- vme.tab
  st_geometry(out.tab) <- st_geometry(buffered_rects)
  
  return(out.tab)
}

#' VME Scenario A: Include low index squares adjacent to high/medium/habitat squares
#' @param vme_index VME index data
#' @return Vector of c-square identifiers meeting scenario criteria
vme_scenario_A <- function(vme_index) {
  # Get squares with low VME index
  low_index <- dplyr::filter(vme_index, VME_Class == 0)
  
  # Get squares with habitat or high/medium index
  habitat_plus_high_medium_index <- dplyr::filter(vme_index, VME_Class %in% c(3,2,1))
  
  # Find adjacent squares
  adjacent_csquares <- get_adjacent_csquares(habitat_plus_high_medium_index$CSquare, 
                                             diagonals = T, csq_degrees = 0.05)
  
  # Get low index squares that are adjacent
  adjacent_low_index <- dplyr::filter(low_index, CSquare %in% adjacent_csquares)
  
  # Combine and get unique squares
  scenario_csquares <- dplyr::bind_rows(habitat_plus_high_medium_index, adjacent_low_index) %>% 
    dplyr::pull(CSquare) %>% 
    unique() 
  
  return(scenario_csquares)
}

#' VME Scenario B: Consider isolated elements with records
#' @param vme_index VME index data
#' @param vme_elements VME elements data
#' @return Vector of c-square identifiers meeting scenario criteria
vme_scenario_B <- function(vme_index, vme_elements) {
  # Get squares with elements that have records
  csq_elements_w_records <- dplyr::pull(vme_elements, csquares)
  
  # Find isolated squares with records
  isolated_csquares <- get_isolated_csquares(csq_elements_w_records)
  isolated_vme <- dplyr::filter(vme_index, CSquare %in% isolated_csquares)
  
  # Get VMEs outside elements
  vme_outside_elements <- dplyr::filter(vme_index, !CSquare %in% csq_elements_w_records)
  
  # Combine and apply scenario A rules
  scenario_B_input <- dplyr::bind_rows(isolated_vme, vme_outside_elements)
  scenario_csquares <-  vme_scenario_A(scenario_B_input)
  
  return(scenario_csquares)
}

#' VME Scenario C: Analyze VME areas based on SAR (Swept Area Ratio) thresholds
#' This scenario considers both VME class and fishing intensity
#' @param vme_index VME index data containing class information
#' @param sar_layer Spatial layer with Swept Area Ratio data
#' @param SAR_threshold Threshold value for determining high vs low fishing activity (default 0.44)
#' @return Vector of c-square identifiers meeting scenario criteria
vme_scenario_C <- function(vme_index, sar_layer, SAR_threshold = 0.44) {
  # Extract SAR values and join with VME index data
  temp <- sar_layer %>% dplyr::select(c_square, SAR) %>%
    st_drop_geometry()
  
  vme_index <- vme_index %>%
    left_join(temp, by = c("CSquare" = "c_square"))
  
  # Set NA SAR values to 0
  vme_index$SAR[is.na(vme_index$SAR)] <- 0
  
  # Get squares with habitat or high/medium VME index
  habitat_plus_high_medium_index <- dplyr::filter(vme_index, VME_Class %in% c(3,2,1)) 
  
  # Split low index squares based on fishing activity
  low_index <- dplyr::filter(vme_index, VME_Class == 0)
  low_index_low_fishing <- dplyr::filter(low_index, SAR < SAR_threshold)
  low_index_high_fishing <- dplyr::filter(low_index, SAR >= SAR_threshold)
  
  # Combine habitat/high/medium index squares with low-fishing low-index squares
  candidate_vmes <- dplyr::bind_rows(habitat_plus_high_medium_index, low_index_low_fishing)
  
  # Find adjacent squares and filter for high-fishing low-index squares
  adjacent_csquares <- get_adjacent_csquares(candidate_vmes$CSquare, csq_degrees = 0.05, diagonals = T)
  adjacent_low_index_high_fishing <- dplyr::filter(low_index_high_fishing, CSquare %in% adjacent_csquares)
  
  # Combine all relevant squares and remove duplicates
  scenario_csquares <- dplyr::bind_rows(candidate_vmes, adjacent_low_index_high_fishing) %>% 
    dplyr::pull(CSquare) %>% 
    unique()
  return(scenario_csquares)
}

#' VME Scenario D: Focus on areas with low fishing activity
#' This scenario selects areas below SAR threshold and their adjacent squares
#' @param vme_index VME index data
#' @param sar_layer Spatial layer with Swept Area Ratio data
#' @param SAR_threshold Threshold for fishing intensity (default 0.44)
#' @return Vector of c-square identifiers meeting scenario criteria
vme_scenario_D <- function(vme_index, sar_layer, SAR_threshold = 0.44) {
  # Join SAR data with VME index
  temp <- sar_layer %>% dplyr::select(c_square, SAR) %>%
    st_drop_geometry()
  
  vme_index <- vme_index %>%
    left_join(temp, by = c("CSquare" = "c_square"))
  
  # Set NA SAR values to 0
  vme_index$SAR[is.na(vme_index$SAR)] <- 0
  
  # Select all squares below SAR threshold
  VME_below_SAR_thresh <- vme_index[vme_index$SAR < SAR_threshold,]
  
  # Get adjacent squares
  adjacent_csquares <- get_adjacent_csquares(VME_below_SAR_thresh$CSquare, csq_degrees = 0.05, diagonals = T)
  
  # Combine original and adjacent squares, remove duplicates
  scenario_csquares <- VME_below_SAR_thresh %>% 
    dplyr::pull(CSquare) %>% 
    c(adjacent_csquares) %>%
    unique() 
  return(scenario_csquares)
}

#' VME Scenario E: Combine approaches from scenarios B and C
#' This scenario considers both VME elements and SAR thresholds
#' @param vme_index VME index data
#' @param vme_elements VME elements data
#' @param sar_layer Spatial layer with SAR data
#' @param SAR_threshold Threshold for fishing intensity (default 0.44)
#' @return Vector of c-square identifiers meeting scenario criteria
vme_scenario_E <- function(vme_index, vme_elements, sar_layer, SAR_threshold = 0.44) {
  # Get results from scenarios B and C
  scenario_B_csquares <- vme_scenario_B(vme_index, vme_elements) 
  scenario_C_csquares <- vme_scenario_C(vme_index, sar_layer, SAR_threshold)
  
  # Combine results and remove duplicates
  scenario_csquares <- c(scenario_B_csquares, scenario_C_csquares) %>% 
    unique()
  
  return(scenario_csquares)
}

#' Process and analyze scenario results
#' This function takes scenario results and performs spatial analysis
#' @param scenario_csquares C-squares identified by scenario
#' @param scenario_name Name for saving output files
#' @param vme_records VME observation records
#' @param assessment_area Spatial layer defining study area
#' @param bathymetry Bathymetry data layer
#' @param fishing_footprint Fishing activity area layer
#' @return Spatial data frame with analyzed results
scenario_outputs <- function(scenario_csquares, scenario_name, vme_records, assessment_area, bathymetry, fishing_footprint) {
  # Check for required scenario name
  if(missing(scenario_name)){
    print("Please supply a scenario name to use when saving files. e.g Scenario_A")
    break
  }
  
  # Convert c-squares to spatial features
  joined_data <- cbind(scenario_csquares, getCoordinates(scenario_csquares, 0.05))
  joined_data <- cbind(joined_data, wkt = wkt_csquare(joined_data$lat, joined_data$lon))
  joined_data <- st_as_sf(joined_data, wkt = "wkt")
  joined_data <-  joined_data %>% 
    st_set_crs(4326)  # Set coordinate reference system to WGS84
  
  # Create buffered polygons and merge
  buffered_data <- csquare_buffer(joined_data)  # Add buffer around each c-square
  unioned_rects <- st_union(buffered_data, by_feature = F)  # Merge into single polygon
  
  # Clean up polygon geometry
  polygon_geoms <- unioned_rects[st_is(unioned_rects, "POLYGON") | st_is(unioned_rects, "MULTIPOLYGON")]
  no_holes_poly <- nngeo::st_remove_holes(polygon_geoms)  # Fill internal holes
  no_holes_poly <- st_cast(no_holes_poly, "POLYGON")  # Split into individual polygons
  no_holes_poly <- st_make_valid(no_holes_poly)  # Ensure valid geometry
  
  # Clip results to relevant spatial boundaries
  poly_in_ecoregion <- st_intersection(no_holes_poly, assessment_area) %>%
    st_make_valid()
  
  poly_in_depth <- st_intersection(poly_in_ecoregion, bathymetry) %>%
    st_make_valid()
  
  poly_in_footprint <- st_intersection(poly_in_depth, fishing_footprint) %>%
    st_make_valid() %>%
    st_as_sf() 
  
  # Join with VME records and calculate statistics
  poly_records <- st_join(poly_in_footprint, vme_records)
  
  # Count VME indicators by polygon
  poly_counts <- poly_records %>%
    group_by(geometry, VME_Indicator) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = VME_Indicator, values_from = count, values_fill = 0)
  
  # Join counts back to polygons
  poly_in_footprint_counts <- st_join(poly_in_footprint, poly_counts, join = st_equals)
  
  # Calculate area in square kilometers
  poly_in_footprint_counts$area_sqkm <- round(as.numeric(st_area(poly_in_footprint_counts)) / 1e6, 1)
  
  # Save results
  suppressWarnings(saveRDS(poly_in_footprint_counts, file = paste0("model/", scenario_name, ".rds")))
  print(paste("Complete for ", scenario_name, ".", sep = ""))  
  
  return(poly_in_footprint_counts)
}
