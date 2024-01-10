vme_scenario_csquares <- function(vme_index, vme_observations, vme_elements, sar_layer, sar.threshold = 0.43, scenario = "A"){

  browser()
  if(scenario %in% c("A", "B", "C", "D", "E") == FALSE){print("Error: Scenario must be A - E")}
  ## etc etc. for the other scenarios
  
  stopifnot(sum(c("lon", "long") %in% colnames(vme_index)) == 1)
  vme_index <- dplyr::rename(vme_index, lon = long)
  
  if(scenario == "A"){
  
    vme_index <- dplyr::select(vme_index, c(lon, lat, csquares, VME_Class)) # select the columns we need to work with
    
    low_index <- dplyr::filter(vme_index, VME_Class == 0) # index low
    
    habitat_plus_high_medium_index <- dplyr::filter(vme_index, VME_Class %in% c(3,2,1)) #Habitat, Index high, index medium
    
    adjacent_csquares <- get_adjacent_csquares(habitat_plus_high_medium_index, diagonals = T, degrees = 0,05)
    ## I have assumed "adjacent" means diagonally as well as above/below/left/right. If not, change this bit.
    
    adjacent_low_index <- dplyr::filter(low_index, csquares %in% adjacent_csquares)
    
    ## bring them back together again and select unique
    scenario_csquares <- dplyr::bind_rows(habitat_plus_high_medium_index, adjacent_low_index) %>% 
      st_drop_geometry() %>% 
      dplyr::pull(csquares) %>% 
      unique() 
  }
  
  if(scenario == "B"){
  if(missing(vme_elements) == TRUE){print("Error: Scenario B requires a VME elements object") }
    
    ## add an extra step to assign csquare to the elements coordinates, 
    
    closures <- st_drop_geometry(vme_elements) %>% dplyr::pull(csquares)
    
    vme_index <- dplyr::select(vme_index, c(lon, lat, csquares, VME_Class)) # select the columns we need to work with
    
    #vme$csquare <- point_to_csquare(vme$lon, vme$lat, 0.05) # just added this function to the utilities file
    ## does the habitat layer already have a c-square? if not, we should add it - I have assumed there is on
    
    #index <- dplyr::select(vme_index, c(lon, lat, csquares, VME_Class)) ## I'm assuming we have centroid and low/med/high
    
    low_index <- dplyr::filter(vme_index, VME_Class == 0) # index low
    high_index <- dplyr::filter(vme_index, VME_Class %in% c(3,2,1)) #Habitat, Index high, index medium
    
    adjacent_csquares <- get_adjacent_csquares(high_index, degrees = 0.05, diagonals = T)
    ## this gives us the c-square address of all the squares adjacent to medium and high index c-squares
    ## I have assumed "adjacent" means diagonally as well as above/below/left/right. If not, change this bit.
    
    low_index <- dplyr::filter(low_index, csquares %in% adjacent_csquares)
    
    index <- rbind(high_index, low_index) ## bring them back together again
    
    #output <- unique(c(vme$csquare, index$csquare))
    
  }  

  if(scenario == "C"){
    
    low_index <- dplyr::filter(low_index, csquare %in% adjacent_csquares)
    
    ## need to split the low index in two, depending on if SAR is < sar.threshold
    ## then generate adjacent c-squares for all cells in high, med, low<sar.threshold
    ## then check if cell in low but >sar.theshold are in this group
    ## then bind together unique csquares into output
    
    
    index <- rbind(high_index, low_index_filtered) ## bring them back together again
    
    #output <- unique(c(vme$csquare, index$csquare))
  }  

  if(scenario == "D"){
    
    #This step could potentially be extracted to data.R 
    VMEgrid <- cbind(VMEgrid, "SAR" = vmsreg[match(VMEgrid$csquares,vmsreg$c.square), "SAR"])
    VMEgrid$SAR[is.na(VMEgrid$SAR)] <- 0

    VME_below_SAR_thresh <- VMEgrid[VMEgrid$SAR < SAR_threshold,]  # select all below SAR threshold
    
    adjacent_csquares <- get_adjacent_csquares(VME_below_SAR_thresh, degrees = 0.05, diagonals = T)
    
    ## bring them back together again and select unique
    scenario_csquares <- st_drop_geometry(VME_below_SAR_thresh) %>% 
      dplyr::pull(csquares) %>% 
      c( adjacent_csquares) %>%
      unique() 
  }
  if(scenario == "E") {
    ## as scenario C, but also including the elements, as per scenario B
  }

  return(scenario_csquares)
  
}
