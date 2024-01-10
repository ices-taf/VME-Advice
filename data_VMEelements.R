elements <- st_read("physical_VME_elements.shp")

# Define the resolution
res <- 0.005

# Create an empty raster with the desired resolution
r <- raster(floor(extent(elements)), res=res)

# Rasterize the polygon
r_polygons <- rasterToPolygons(r)

# Convert to sf objects
r_polygons_sf <- st_as_sf(r_polygons)
elements_sf <- st_as_sf(elements)

# Identify which raster polygons intersect with the elements
intersects <- st_intersects(r_polygons_sf, elements_sf)

# Filter the raster polygons that intersect with the elements
r_polygons_intersect <- r_polygons_sf[unlist(intersects), ]

# Calculate the centroid of each intersecting raster polygon
centroids <- st_centroid(r_polygons_intersect)

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
# Write the data frame to a shapefile
st_write(df, "elementscsquare.shp", append = F)

