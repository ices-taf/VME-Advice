# Read the VME data file
vme_observations <- load(taf.data.path("vme_db.rds"))

# Process the VME data
# Get the CSquare (grid cell) for each observation based on latitude, longitude, and resolution
vme <- getCSquare(vme_observations$MiddleLatitude, vme_observations$MiddleLongitude, 0.05)
vme <- unique(vme)  # Remove duplicate CSquares

# Convert CSquare to latitude and longitude coordinates
vme <- cbind(vme, CSquare2LonLat(vme, 0.05))
names(vme) <- c("csquare", "Latitude", "Longitude")

# Generate Well-Known Text (WKT) representation of each CSquare
vme$wkt <- wkt_csquare(vme$Latitude, vme$Longitude)

# Create a spatial data frame (sf) for CSquares
vme_csquare <- vme %>%
  dplyr::select("csquare", "wkt") %>%  # Select relevant columns
  st_as_sf(wkt = "wkt") %>%  # Convert to sf object using WKT
  st_set_crs(4326)  # Set the coordinate reference system (CRS) to WGS84

# Create a spatial data frame (sf) for VME records
vme_records <- st_as_sf(vme_observations, coords = c("MiddleLongitude", "MiddleLatitude"), crs = 4326) %>%
  dplyr::select(VME_Indicator, geometry) %>%  # Select relevant columns
  st_make_valid()  # Ensure valid geometry

# Filter out empty VME_Indicator values
vme_records <- vme_records[vme_records$VME_Indicator != "", ]

# Write the data frames to files in the "data" directory
saveRDS(vme_csquare, file = "data/vme_csquares.rds")
saveRDS(vme_records, file = "data/vme_records.rds")