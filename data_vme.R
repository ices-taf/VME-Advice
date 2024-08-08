# read the VME data file
#vme_observations <- read.csv(taf.data.path("VME_observations_datacall_2022_eu.csv"), header=T)
vme_observations <- read.csv("../../VME-advice_noGIT/EU/VME data repository/VME observations and csquares/VME_observations_datacall_2022_eu_AD.csv", header=T)

# process the VME data
vme <- getCSquare(vme_observations$MiddleLatitude, vme_observations$MiddleLongitude, 0.05)
vme <- unique(vme)
vme <- cbind(vme, CSquare2LonLat(vme, 0.05))
names(vme) <- c("csquare", "Latitude", "Longitude")
vme$wkt <- wkt_csquare(vme$Latitude, vme$Longitude)
vme_csquare <- vme %>%
  dplyr::select("csquare", "wkt") %>%
  st_as_sf(wkt = "wkt") %>%
  st_set_crs(4326)


vme_records <- st_as_sf(vme_observations, coords = c("MiddleLongitude", "MiddleLatitude"), crs = 4326) %>%
  dplyr::select(VME_Indicator, HabitatType, geometry) %>%
      st_make_valid()


# THESE LINES ONLY FOR EU ASSESSMENT
if(eu_assessment == T) {
  
  VMEindic <- c('Black coral','Cup coral','Gorgonian','Soft coral','Sponge','Sea-pen','Stylasterids','Stony coral')
  VMEhabs <- c('Bryozoan patches','Cold-water coral reef','Coral garden','Deep-sea sponge aggregations','Mud and sand emergent fauna','Sea-pen fields','Tube-dwelling anemone aggregations')
  vme_records <- subset(vme_records,(vme_records$VME_Indicator %in% VMEindic | vme_records$HabitatType %in% VMEhabs))
}
#vme_records <- vme_records[vme_records$VME_Indicator != "", ]

# Write the data frame to data/.... as .rds or .rdata

saveRDS(vme_csquare, file = "data/vme_csquares.rds")
saveRDS(vme_records, file = "data/vme_records.rds")
