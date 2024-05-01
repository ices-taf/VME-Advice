# read the VME data file
vme_observations <- read.csv(taf.data.path("VME_observations_datacall_2022_eu.csv"), header=T)

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
  dplyr::select(VME_Indicator, geometry) %>%
      st_make_valid()

vme_records <- vme_records[vme_records$VME_Indicator != "", ]

# Write the data frame to data/.... as .rds or .rdata

saveRDS(vme_csquare, file = "data/vme_csquares")
saveRDS(vme_records, file = "data/vme_records")
