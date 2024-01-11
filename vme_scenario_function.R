vme_scenario_csquares <- function(vme_index, vme_observations, vme_elements, sar_layer, sar.threshold = 0.43, scenario = "A"){
  # stopifnot(vme_index)
  browser()
  if(scenario %in% c("A", "B", "C", "D", "E") == FALSE){print("Error: Scenario must be A - E")}
  ## etc etc. for the other scenarios
  
  stopifnot(sum(c("lon", "long") %in% colnames(vme_index)) == 1)
  vme_index <- dplyr::rename(vme_index, lon = long)
  vme_index <- dplyr::select(vme_index, c(lon, lat, csquares, VME_Class)) # select the columns we need to work with
  
  if(scenario == "A"){
    vme_scenario_A(vme_index)
    
  }
  
  if(scenario == "B"){
    if(missing(vme_elements) == TRUE){print("Error: Scenario B requires a VME elements object") }
    
    vme_scenario_B(vme_index, vme_elements) 
  }  

  if(scenario == "C"){
    
    vme_scenario_C(vme_index, sar_layer) 
  }  

  if(scenario == "D"){
    
    vme_scenario_D(vme_index, sar_layer) 
  }
  
  if(scenario == "E") {
    ## as scenario C, but also including the elements, as per scenario B
    scenario_B_csquares <- vme_scenario_B()
    scenario_C_csquares <- vme_scenario_C()
    
    scenario_csquares <- c(scenario_B_csquares, scenario_C_csquares) %>% 
      unique()
  }

  return(scenario_csquares)
  
}

