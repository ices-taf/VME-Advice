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
