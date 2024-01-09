## Take the output of the scenario process and export

joined_data <- icesVMS::get_csquare(vme_scenario_csquares) ## fetches c-square geometry from the ICES API
joined_data <- st_as_sf(joined_data, wkt = "wkt")
joined_data <-  joined_data %>% 
                  st_set_crs(4326)

buffered_data <- csquare_buffer(joined_data) ## generates a 0.025 degree buffer around each c-square

unioned_rects <- st_union(buffered_data, by_feature = F) ## merges them into a single polygon

no_holes_poly <- nngeo::st_remove_holes(unioned_rects) ## fills in any holes within the area

no_holes_poly <- st_cast(no_holes_poly, "POLYGON") ## converts back from a single multipolygon to individual polgyons, 
                                                   ## so we can summarise what each contains


###  <-

###  In here we would put the code to clip the VME polygons to the EEZ, depth range, etc.

###  <- 

###  And in here we could put some code using "st_over" to count the number of each type of VME record in each of the final polygons

st_write(no_holes_poly, "export.shp")