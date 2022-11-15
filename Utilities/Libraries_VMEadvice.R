
# Libraries_ICESVME

# ICES libraries
#remotes::install_github("ices-tools-prod/icesSharePoint")
# library(icesSharePoint), only needed to link to sharepoint

# devtools::install_github("ices-tools-prod/icesVMS")
# library(icesVMS), only needed to download VMS data from ICES data centre (data product is on sharepoint) 

# R libraries
library(sf)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(leaflet)
library(htmlwidgets)
library(smoothr)
library(raster)
library(tmap)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(htmlTable)
library(glue)


overlap_VME_element_scenario <- function(dataframe, element, scenario) {
  sum(table(dataframe[[element]], dataframe$New_VMEs, dataframe[[scenario]]))
  
}
overlap_element_all_scenarios <- function(dataframe, element, scenarios){
  
  purrr::map2_dfc(.x = rep(list(dataframe),length(scenarios)), 
                  .y = scenarios, 
                  .f = function(.x, .y) overlap_VME_element_scenario(dataframe = .x, scenario = .y, element = element))
}

overlap_all_elements_all_scenarios <- function(dataframe, elements, scenarios){
  
  purrr::map2_df(.x = rep(list(dataframe),length(elements)),
                 .y = elements,
                 .f = function(.x, .y) overlap_element_all_scenarios(dataframe = .x, element = .y, scenarios = scenarios))
}

