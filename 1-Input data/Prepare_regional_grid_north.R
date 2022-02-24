
# create spatial grid for ICES area North 

# install libraries
  library(rgdal)
  library(sp)
  library(raster)
  library(sf)

# set folder directory
  pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"
  
# obtain polygon with the area of interest
  # ICES area of interest --> Far, Icelandic waters, Greenland Sea, Nor Sea, Arctic Oc and barents Sea
  shapeEcReg <- st_read(paste(pathdir,"1-Input data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp",sep="/"))
  subEcReg   <- subset(shapeEcReg, Ecoregion %in% c("Arctic Ocean","Barents Sea","Icelandic Waters",
                                                    "Faroes", "Norwegian Sea", "Greenland Sea"))
  subEcReg   <- st_make_valid(subEcReg)
  subEcRegCo <- st_union(subEcReg)
  
  # combine the areas and create a buffer
  VMEarea   <- st_buffer(subEcRegCo,10)      # create a buffer to make sure no VMEs will be ignored on the borders 
  rm('shapeEcReg','subEcRegCo','subEcReg')
  
# create grid 
  gt               <- (GridTopology(c(-45.975, 56.025), c(0.05, 0.05), c(2300, 679))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
  grt              <- SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
  spix             <- as(grt, "SpatialPixels")
  spol             <- as(spix, "SpatialPolygons")
  rnames           <- sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
  LOCUNI           <- as.data.frame(seq(1,length(spix)))
  rownames(LOCUNI) <- rnames
  bargrid          <- SpatialPolygonsDataFrame(spol, LOCUNI)
  bargrid@bbox        # make sure "min" is a whole number
  save(bargrid, file = paste(pathdir,"1-Input data/Region_csquare_grid_north.RData",sep="/"))
  
  # load
  load(paste(pathdir,"1-Input data/Region_csquare_grid_north.RData",sep="/"))
  bargrid_sf1       <- st_as_sf(bargrid[1:750000,])
  bargrid_sf2       <- st_as_sf(bargrid[750001:1561700,])
  bargrid_sf        <- rbind(bargrid_sf1,bargrid_sf2)
  rm('bargrid_sf1','bargrid_sf2')

# assign c-squares
  source(paste(pathdir,"Utilities/coords_to_csquare_VMStools.R",sep="/"))
  coord   <- coordinates(bargrid)
  squares <- CSquare(coord[,1],coord[,2],0.05)
  bargrid@data$csquares <- squares

# remove id and rownames  
  bargrid                <- bargrid[,-1] 
  rownames(bargrid@data) <- NULL
  rm('grt','gt','spix','spol','squares','rnames','coord','LOCUNI')
  
# check for each c-square if it intersects with VMEarea
  tt                  <- st_intersects(bargrid_sf,VMEarea)
  bargrid@data$select <- as.numeric(tt)
  bargrid             <- subset(bargrid,!(is.na(bargrid@data$select)))
  save(bargrid, file = paste(pathdir,"1-Input data/Region_csquare_grid_north.RData",sep="/"))
  rm ('tt','VMEarea','bargrid_sf')

# assign ICES ecoregions based on the midpoint of the c-square
  shapeEcReg     <- st_read(paste(pathdir,"1-Input data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp",sep="/"))
  shapeEcReg     <- st_make_valid(shapeEcReg)
  coord          <- coordinates(bargrid)
  coords         <- SpatialPoints(coords = cbind(coord[,1],coord[,2]))
  coords         <- st_as_sf(coords)
  st_crs(coords) <- 4326
  tt             <- st_intersects(coords,shapeEcReg)
  bargrid$EcNo   <- as.numeric(tt)
  Ec             <- data.frame(EcNo = unique(bargrid@data$EcNo))
  Ec             <- cbind(Ec,shapeEcReg[match(Ec$EcNo,shapeEcReg$OBJECTID),c(2)])
  Ec             <- Ec[,-3]
  bargrid        <- cbind(bargrid,Ec[match(bargrid@data$EcNo,Ec$EcNo),c(2)]) 
  colnames(bargrid@data)[ncol(bargrid@data)] <- "EcReg"
  
# assign EEZ
  shapeEEZ       <- st_read(paste(pathdir,"1-Input data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp",sep="/"))
  shapeEEZ       <- st_make_valid(shapeEEZ)
  tt             <- st_intersects(coords,shapeEEZ)
  tt             <- stack(setNames(tt, seq_along(tt)))[2:1]
  tt             <- tt[!(duplicated(tt$ind)),]
  bargrid$id     <- 1:nrow(coords)
  bargrid        <- cbind(bargrid,tt[match(bargrid@data$id,tt$ind),c(2)])
  colnames(bargrid@data)[ncol(bargrid@data)] <- "EEZNo"
  EEZ            <- data.frame(EEZNo = unique(bargrid@data$EEZNo))
  shapeEEZ$id    <- 1:nrow(shapeEEZ)
  EEZ            <- cbind(EEZ,shapeEEZ[match(EEZ$EEZNo,shapeEEZ$id),c(1)])
  EEZ            <- EEZ[,-3]
  bargrid        <- cbind(bargrid,EEZ[match(bargrid@data$EEZNo,EEZ$EEZNo),c(2)]) 
  colnames(bargrid@data)[ncol(bargrid@data)] <- "EEZ"
  
# get surface area of each grid cell 
  bargrid@data$area_sqkm <- area(bargrid) / 1000000

  # get coordinates
  bargrid@data              <- bargrid@data[,c(1,4,7,8)] ## get c-square, ecoreg, eez, area_sqkm
  colnames(bargrid@data)[2] <- "Ecoregion"
  dd                        <- coordinates(bargrid)
  colnames(dd)              <- c("long","lat")
  bargrid@data$long         <- dd[,1]
  bargrid@data$lat          <- dd[,2]

  save(bargrid, file = paste(pathdir,"1-Input data/Region_csquare_grid_north.RData",sep="/"))
  