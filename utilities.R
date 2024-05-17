## Utility functions for the VME analysis


######################################################################################

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

#' Gets the csquare of lat-lon point data at a given resolution
#'
#' @param lat 
#' @param lon 
#' @param res 
#'
#' @return
#' @export
#'
#' @examples 
#' getCSquare(c(0,0), c(0, 10), 10)
#' getCSquare(c(0,0), c(0, 10), 1)
#' getCSquare(c(0,0.1), c(0, 0.1), 1)
#' getCSquare(c(0,0.1), c(0, 0.1), 0.1)

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

#' Get the centroid coordinates of a csquare to a specific resolution
#'
#' @param cSquare character or character vector in csquare format
#' @param degrees numeric vector defining the resoluion of returned lat lon data
#'
#' @return
#' @export
#'
#' @examples
#' getCoordinates("1000:100:100", 0.05)
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

#' Title
#'
#' @param input_csquares 
#' @param csq_degrees 
#' @param diagonals 
#'
#' @return
#' @export
#'
#' @examples
get_adjacent_csquares <- function(input_csquares, csq_degrees = 0.05, diagonals = T) {
  
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

##########################################################################################

get_isolated_csquares <- function(csquares) {
  if (class(csquares) == "character") {
    ids <- csquares
  } else if (class(csquares) == "data.frame") {
    stopifnot("csquares" %in% colnames(csquares), "Column with name 'csquares' must be present")
    ids <- csquares$csquares
  }
  
  list_adjacents <- purrr::map(ids, ~ get_adjacent_csquares(.x, csq_degrees = 0.05, diagonals = T))
  list_neighbours_within_input <- purrr::map(list_adjacents, ~ .x[.x %in% ids])
  isolated <- purrr::map_lgl(list_neighbours_within_input, ~ length(.x) == 0)
  
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

csquare_to_polygon <- function(scenario_csquares, degrees = 0.05) {
  
  csquare_w_coordinates <- cbind(scenario_csquares, getCoordinates(scenario_csquares, 0.05))
  csquare_sf <- cbind(csquare_w_coordinates, wkt = wkt_csquare(csquare_w_coordinates$lat, csquare_w_coordinates$lon)) %>% 
    st_as_sf( wkt = "wkt") %>%  
    st_set_crs(4326)
  
  return(csquare_sf)
}

######################################################################################

vme_scenario_A <- function(vme_index) {
  
  low_index <- dplyr::filter(vme_index, VME_Class == 0) # index low
  
  habitat_plus_high_medium_index <- dplyr::filter(vme_index, VME_Class %in% c(3,2,1)) #Habitat, Index high, index medium
  
  adjacent_csquares <- get_adjacent_csquares(habitat_plus_high_medium_index$CSquare, diagonals = T, csq_degrees = 0.05)
  ## I have assumed "adjacent" means diagonally as well as above/below/left/right. If not, change this bit.
  
  adjacent_low_index <- dplyr::filter(low_index, CSquare %in% adjacent_csquares)
  
  ## bring them back together again and select unique
  scenario_csquares <- dplyr::bind_rows(habitat_plus_high_medium_index, adjacent_low_index) %>% 
    dplyr::select(CSquare) %>% 
    unique() 
  scenario_csquares <- dplyr::mutate(scenario_csquares, VME = "Habitat and Index")
  return(scenario_csquares)
  
  #This works but only when clipping to the fishing layer is removed in the next step.
}

######################################################################################
vme_scenario_B <- function(vme_index, vme_records, vme_elements_raw, vme_elements_csquares, scenario_A_csquares) {
  
  intersects_sparse <- vme_elements_raw %>% st_intersects(vme_records)
  rows_with_points <- unique(as.data.frame(intersects_sparse)$row)

  csq_vme_record_elements <- vme_elements_raw[rows_with_points,] %>% 
    st_join(vme_elements_csquares) %>% 
    dplyr::select(CSquare = csquares) %>% 
    st_drop_geometry() %>% 
    dplyr::mutate(VME = "Element")
  
  scenario_csquares <- dplyr::bind_rows(scenario_A_csquares, csq_vme_record_elements) 
  
  return(scenario_csquares)
}
# This is also not quite right -> or it is, but in the original assessment csq_vme_record_elements do not go through the 0.25 csquare buffering process, so the following function needs to change 
# consider changing the scenario functions to return a list (csquares_for_buffering, csquares_not_for_buffering)

######################################################################################

vme_scenario_C <- function(vme_index, sar_layer, SAR_threshold = 0.43) {
  
  #This step could potentially be extracted to data.R 
  temp <- sar_layer %>% dplyr::select(c_square, SAR) %>%
            st_drop_geometry()
    
  vme_index <- vme_index %>%
    left_join(temp, by = c("CSquare" = "c_square"))
  
  vme_index$SAR[is.na(vme_index$SAR)] <- 0
  
  habitat_plus_high_medium_index <- dplyr::filter(vme_index, VME_Class %in% c(3,2,1)) #Habitat, Index high, index medium
  
  low_index <- dplyr::filter(vme_index, VME_Class == 0) # index low
  low_index_low_fishing <- dplyr::filter(low_index, SAR < SAR_threshold)
  low_index_high_fishing <- dplyr::filter(low_index, SAR >= SAR_threshold)
  
  candidate_vmes <- dplyr::bind_rows(habitat_plus_high_medium_index, low_index_low_fishing)
  
  
  adjacent_csquares <- get_adjacent_csquares(candidate_vmes$CSquare, csq_degrees = 0.05, diagonals = T)
  adjacent_low_index_high_fishing <- dplyr::filter(low_index_high_fishing, CSquare %in% adjacent_csquares)
  
  ## then bind together unique csquares into output
  scenario_csquares <- dplyr::bind_rows(candidate_vmes, adjacent_low_index_high_fishing) %>% 
    dplyr::select(CSquare) %>% 
    unique()
  scenario_csquares <- dplyr::mutate(scenario_csquares, VME = "Habitat and Index")
  
  return(scenario_csquares)
}

# This may be correct, as it follows the same logic as the original assessment, where low-index high fishing buffer is applied to the candidate csquares
# A substantial difference results nonetheless:
# the 'TAF workflow' considers all csquares in the first instance, before clipping to the fishing and depth footprints
# The original assessment does not appear to do so.

######################################################################################
# 
# vme_scenario_D <- function(vme_index, sar_layer, SAR_threshold = 0.43) {
#   
#   temp <- sar_layer %>% dplyr::select(c_square, SAR) %>%
#     st_drop_geometry()
#   
#   vme_index <- vme_index %>%
#     left_join(temp, by = c("CSquare" = "c_square"))
#   
#   vme_index$SAR[is.na(vme_index$SAR)] <- 0
#   
#   VME_below_SAR_thresh <- vme_index[vme_index$SAR < SAR_threshold,]  # select all below SAR threshold
#   
#   adjacent_csquares <- get_adjacent_csquares(VME_below_SAR_thresh$CSquare, csq_degrees = 0.05, diagonals = T)
#   
#   ## bring them back together again and select unique
#   scenario_csquares <- VME_below_SAR_thresh %>% 
#     dplyr::pull(CSquare) %>% 
#     c(adjacent_csquares) %>%
#     unique() 
#   return(scenario_csquares)
# }

######################################################################################

vme_scenario_D <- function(vme_index, sar_layer, SAR_threshold = 0.43) {
  
  temp <- sar_layer %>% dplyr::select(c_square, SAR) %>%
    st_drop_geometry()
  
  vme_index <- vme_index %>%
    left_join(temp, by = c("CSquare" = "c_square"))
  
  vme_index$SAR[is.na(vme_index$SAR)] <- 0
  
  VME_below_SAR_thresh <- vme_index[vme_index$SAR < SAR_threshold,]  # select all below SAR threshold
  
  # adjacent_csquares <- get_adjacent_csquares(VME_below_SAR_thresh$CSquare, csq_degrees = 0.05, diagonals = T)
  
  ## bring them back together again and select unique
  scenario_csquares <- VME_below_SAR_thresh %>% 
    dplyr::select(CSquare) %>% 
    dplyr::mutate(VME = "Habitat and Index")

  return(scenario_csquares)
  }

######################################################################################
# 
# vme_scenario_E <- function(vme_index, vme_elements, sar_layer, SAR_threshold = 0.43) {
# ## as scenario C, but also including the elements, as per scenario B
# scenario_B_csquares <- vme_scenario_B(vme_index, vme_elements) 
# scenario_C_csquares <- vme_scenario_C(vme_index, sar_layer, SAR_threshold)
# 
# scenario_csquares <- c(scenario_B_csquares, scenario_C_csquares) %>% 
#   unique()
# 
# return(scenario_csquares)
# }
# 
# ######################################################################################
# alt_vme_scenario_E <- function(vme_index, vme_elements, sar_layer, SAR_threshold = 0.43) {
# ## as scenario C, but also including the elements, as per scenario B
# scenario_B_csquares <- alt3_vme_scenario_B(vme_index, vme_elements) 
# scenario_C_csquares <- vme_scenario_C(vme_index, sar_layer, SAR_threshold)
# 
# scenario_csquares <- c(scenario_B_csquares, scenario_C_csquares) %>% 
#   unique()
# 
# return(scenario_csquares)
# }
######################################################################################
vme_scenario_E <- function(scenario_B_csquares, scenario_C_csquares) {

scenario_csquares <- dplyr::bind_rows(scenario_B_csquares, scenario_C_csquares) %>% 
  unique()


return(scenario_csquares)
}

######################################################################################

scenario_outputs <- function(scenario_csquares, scenario_name, vme_records, assessment_area, fishing_footprint, bathymetry) {
  
  if(missing(scenario_name)){
    print("Please supply a scenario name to use when saving files. e.g Scenario_A")
    break
  }

  if("Element" %in% scenario_csquares$VME) {
    vme_elements <- dplyr::filter(scenario_csquares, VME == "Element") %>% 
      dplyr::pull(CSquare) %>% 
      csquare_to_polygon() 
    # %>% 
    #   st_union(by_feature = F) %>%
    #   st_cast("POLYGON") %>% 
    #   st_sf() %>%
    #   mutate(group = 1) %>%
    #   group_by(group) %>%
    #   summarize(do_union = TRUE) %>%
    #   st_cast("MULTIPOLYGON") %>%
    #   st_sf()
    # 
    # 
    vme_habitat_and_index <- dplyr::filter(scenario_csquares, VME == "Habitat and Index") %>% 
      dplyr::pull(CSquare) %>% 
      csquare_to_polygon() %>% 
      csquare_buffer() 
    # >% 
    #   st_union(by_feature = F) %>% 
    #   %>%
    #   st_cast("POLYGON") %>% 
    #   st_sf() %>%
    #   mutate(group = 1) %>%
    #   group_by(group) %>%
    #   summarize(do_union = TRUE) %>%
    #   st_cast("MULTIPOLYGON") %>%
    #   st_sf()
    
    unioned_rects <- st_union(vme_elements, vme_habitat_and_index) %>% 
      st_cast("POLYGON") %>% 
        st_sf() %>%
        mutate(group = 1) %>%
        group_by(group) %>%
        summarize(do_union = TRUE) %>%
        st_cast("MULTIPOLYGON") %>%
        st_sf()
    
  } else {
    vme_habitat_and_index <- dplyr::pull(scenario_csquares, CSquare) %>% 
      csquare_to_polygon() %>% 
      csquare_buffer()
    
    unioned_rects <- st_union(vme_habitat_and_index, by_feature = F)
  }
  
  
  # Filter unioned_rects to include only POLYGON and MULTIPOLYGON geometries
  polygon_geoms <- unioned_rects[st_is(unioned_rects, "POLYGON") | st_is(unioned_rects, "MULTIPOLYGON")]
  
  #trial
  no_holes_poly <- polygon_geoms %>% st_cast("POLYGON") %>% st_make_valid() %>% nngeo::st_remove_holes(max_area = (0.05*0.1))
  
  # # Apply st_remove_holes() to the filtered geometry
  # no_holes_poly <- nngeo::st_remove_holes(polygon_geoms, max_area = (0.05*0.1)) ## fills in any holes within the area
  # 
  # no_holes_poly <- st_cast(no_holes_poly, "POLYGON") ## converts back from a single multipolygon to individual polgyons,
  ## so we can summarise what each contains

  no_holes_poly <- st_make_valid(no_holes_poly)
  
  ## Clip results to loaded shapefiles
  ###  ecoregion
  poly_in_ecoregion <- st_intersection(no_holes_poly, assessment_area) %>%
    st_make_valid
  
  ###  bathymetry
  poly_in_depth <- st_intersection(poly_in_ecoregion, bathymetry) %>%
    st_make_valid %>% st_as_sf()
  
  
  ###  fishing footprint
  # poly_in_footprint <- st_intersection(poly_in_depth, fishing_footprint) %>%
  #   st_make_valid() %>%
  #   st_as_sf() 
  # 
  # Perform a spatial join between poly_in_footprint and vme_records
  poly_records <- poly_in_depth  %>% st_join(vme_records) %>% st_make_valid() %>% rename(geometry = x)
  # poly_records <- st_join(poly_in_footprint, vme_records)
  
  # Group by polygon geometry and count the number of each VME_Indicator type
  poly_counts <- poly_records %>%
    group_by(geometry, VME_Indicator) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = VME_Indicator, values_from = count, values_fill = 0)
  
  # Perform a spatial join between poly_in_footprint and poly_counts
  #poly_in_footprint_counts <- st_join(poly_in_footprint, poly_counts, join = st_equals)
  poly_in_depth_counts <- st_join(poly_in_depth, poly_counts, join = st_equals)
  
  
  # Calculate the area in square kilometers
  # poly_in_footprint_counts$area_sqkm <- round(as.numeric(st_area(poly_in_footprint_counts)) / 1e6, 1)
  poly_in_depth_counts$area_sqkm <- round(as.numeric(st_area(poly_in_depth_counts)) / 1e6, 1)
  

  #suppressWarnings(saveRDS(poly_in_depth_counts, file = paste0("model/alt/", scenario_name, ".rds")))
  suppressWarnings(write_sf(poly_in_depth_counts, dsn = paste0("model/",Sys.Date(), "/", scenario_name, ".shp")))
  print(paste("Complete for ", scenario_name, ".", sep = ""))  
  return(poly_in_depth_counts)
}
