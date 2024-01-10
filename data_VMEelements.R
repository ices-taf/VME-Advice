library(raster)
library(sf)
library(vmstools)

elements <- st_read("physical_VME_elements(fixed).shp")

# Define the resolution
res <- 0.05

# Create an empty raster with the desired resolution
r <- raster(floor(extent(elements)), res=res)

# Convert the polygon to a raster
poly_ras <- rasterize(elements, r, field = 1)

# Convert the polygon to MULTILINESTRING first
poly_multilines <- st_cast(elements, "MULTILINESTRING")

# Then cast the MULTILINESTRING to LINESTRING
poly_lines <- st_cast(poly_multilines, "LINESTRING")

# Rasterize the lines
lines_ras <- rasterize(poly_lines, r, field = 1)

# Combine the two rasters
combined_ras <- merge(poly_ras, lines_ras)

# Rasterize the polygon
r_polygons <- rasterToPolygons(combined_ras)

# Convert to sf objects
r_polygons_sf <- st_as_sf(r_polygons)

# Make the spatial objects valid
r_polygons_sf <- st_make_valid(r_polygons_sf)

# set the CRS
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

# Write the data frame to a shapefile
st_write(df, "elementscsquare.shp", append = F)
