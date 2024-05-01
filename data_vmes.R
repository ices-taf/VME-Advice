# read the VME data file
vme_observations <- read.csv("VME_observations_datacall_2022_eu.csv", header=T)


vme_records <- st_as_sf(vme_observations, coords = c("MiddleLongitude", "MiddleLatitude"), crs = 4326) %>%
  dplyr::select(VME_Indicator, geometry) %>%
      st_make_valid()

vme_records <- vme_records[vme_records$VME_Indicator != "", ]

