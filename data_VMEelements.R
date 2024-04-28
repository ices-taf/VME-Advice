elements <- st_read("physical_VME_elements(fixed).shp")

elements$type <- as.factor(paste(elements$type))

# Define the resolution
res <- 0.05

# Create an empty raster with the desired resolution
r <- raster(floor(extent(elements)), res=res)

# Get unique types
unique_types <- unique(elements$type)

# Create an empty list to store the results for each type
type_results <- list()

# Process each type separately
for (type in unique_types) {
  # Subset elements by type
  type_elements <- elements[elements$type == type, ]
  
  # Convert the polygon to a raster
  poly_ras <- rasterize(type_elements, r, field = "type")
  
  # Convert the polygon to MULTILINESTRING first
  poly_multilines <- st_cast(type_elements, "MULTILINESTRING")
  
  # Then cast the MULTILINESTRING to LINESTRING
  poly_lines <- st_cast(poly_multilines, "LINESTRING")
  
  # Rasterize the lines
  lines_ras <- rasterize(poly_lines, r, field = "type")
  
  # Combine the two rasters
  combined_ras <- merge(poly_ras, lines_ras)
  
  # Rasterize the polygon
  r_polygons <- rasterToPolygons(combined_ras)
  
  # Convert to sf objects
  r_polygons_sf <- st_as_sf(r_polygons)
  
  # Make the spatial objects valid
  r_polygons_sf <- st_make_valid(r_polygons_sf)
  
  # Set the CRS
  r_polygons_sf <- st_set_crs(r_polygons_sf, st_crs(elements))
  
  # Calculate the centroid of each intersecting raster polygon
  centroids <- st_centroid(r_polygons_sf)
  
  # Get the coordinates of these centroids
  coords <- st_coordinates(centroids)
  
  # Convert to a data frame
  df <- data.frame(coords)
  
  # Rename the columns
  names(df) <- c("Longitude", "Latitude")
  
  df$csquare <- CSquare(df$Longitude, df$Latitude, 0.05)
  
  df$wkt <- wkt_csquare(df$Latitude, df$Longitude)
  
  df <- st_as_sf(df, wkt = "wkt")
  
  df <- df %>% st_set_crs(4326)
  
  # Add a column for the current type with a value of 1
  df[[type]] <- 1
  
  # Convert the sf object to a regular data frame
  df_regular <- st_drop_geometry(df)
  
  # Store the result for the current type
  type_results[[type]] <- df_regular
}

# Merge the results for all types
merged_df <- Reduce(function(x, y) merge(x, y, all = TRUE), type_results)

# Replace NA values with 0
merged_df[is.na(merged_df)] <- 0

# Convert the merged data frame back to an sf object
merged_df$wkt <- wkt_csquare(merged_df$Latitude, merged_df$Longitude)
merged_sf <- st_as_sf(merged_df, wkt = "wkt")
merged_sf <- st_set_crs(merged_sf, 4326)

# Write the data frame to a shapefile
st_write(merged_sf, "elementscsquare.shp", append = FALSE)

