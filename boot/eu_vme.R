library(dplyr)

# Set the path to your geopackage file -- needs to be a network location
gpkg_file <- taf.boot.path("../../../VME-advice_noGIT/EU/EUVME_Assessment_2022.gpkg") # contact neil.campbell@ices.dk for access

# load the swept area layer from the geopackage
# sar_layer <- st_read(gpkg_file, layer = "VME_Assessment_SAR")

# sar_layer <- readRDS("../../VME-advice_noGIT/EU/VMS data repository/All_VMS_datacall2022.rds") %>% 
# replace(is.na(.), 0) %>% 
#   rename(c_square = c.square)
# nam <- c(paste("SAR_total",refyear,sep="_"))
# indexcol <- which(names(sar_layer) %in% nam) 
# sar_layer$SAR <- rowMeans(sar_layer[indexcol],na.rm=T)
# colnames(sar_layer)[1] <- "c_square"
# 
# saveRDS(sar_layer, file = "sar_layer.rds")

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

vme_elements_raw <- st_read(gpkg_file, layer = "VME_elements") %>%
  st_make_valid()
saveRDS(vme_elements_raw, file = "vme_elements_raw.rds")



