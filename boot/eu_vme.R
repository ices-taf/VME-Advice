# Set the path to your geopackage file -- needs to be a network location
gpkg_file <- taf.boot.path("../../../VME-advice_noGIT/EU/EUVME_Assessment_2022.gpkg") # contact neil.campbell@ices.dk for access

# load the swept area layer from the geopackage
sar_layer <- st_read(gpkg_file, layer = "VME_Assessment_SAR")
saveRDS(sar_layer, file = "sar_layer.rds")

# load the area being considered in the assessment
assessment_area <- st_read(gpkg_file, layer = "EUVME_Assessment_Extent") %>%
  st_make_valid()
saveRDS(assessment_area, file = "assessment_area.rds")

# load the fishing footprint
fishing_footprint <- st_read(gpkg_file, layer = "Footprint_All") %>%
  st_make_valid()
saveRDS(fishing_footprint, file = "fishing_footprint.rds")

# load the bathymetry layer
bathymetry <- st_read(gpkg_file, layer = "EU_Depth_400_800") %>%
  st_make_valid()
saveRDS(bathymetry, file = "bathymetry.rds")



