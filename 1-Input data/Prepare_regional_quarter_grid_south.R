
# get one quarter of a c-square 0.05 x 0.05 grid, this is used to estimate buffers 

# install libraries
  library(rgdal)
  library(sp)
  library(raster)
  library(sf)

# set folder directory
  pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"

# create grid     
  gt                <-(GridTopology(c(-43.9375, 32.0625), c(0.025, 0.025), c(2300, 1350))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
  grt               <-SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
  spix              <- as(grt, "SpatialPixels")
  spol              <- as(spix, "SpatialPolygons")
  rnames            <- sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
  LOCUNI            <- as.data.frame(seq(1,length(spix)))
  rownames(LOCUNI)  <- rnames
  quar_grid           <- SpatialPolygonsDataFrame(spol, LOCUNI)
  
  # save file
  save(quar_grid, file = paste(pathdir,"1-Input data/Region_0.25_csquare_grid_south.rdata",sep="/"))
  quar_grid@bbox 
  
# get coordinates
  coord               <- coordinates(quar_grid)
  quar_grid@data$long <- coord[,1]
  quar_grid@data$long <- round(quar_grid@data$long, digits = 4)
  quar_grid@data$lat  <- coord[,2]
  quar_grid@data$lat  <- round(quar_grid@data$lat, digits = 4)
  quar_grid@data$uni  <- paste(quar_grid@data$long,quar_grid@data$lat) 
  
# save file
  save(quar_grid, file = paste(pathdir,"1-Input data/Region_0.25_csquare_grid_south.rdata",sep="/"))
  
# save in steps of million rows - will be removed afterwards
  qc1 <- quar_grid[1:1000000,]
  save(qc1, file = paste(pathdir,"1-Input data/qc1_grid.rdata",sep="/"))
  
  qc2 <- quar_grid[1000001:2000000,]
  save(qc2, file = paste(pathdir,"1-Input data/qc2_grid.rdata",sep="/"))
  
  qc3 <- quar_grid[2000001:nrow(quar_grid),]
  save(qc3, file = paste(pathdir,"1-Input data/qc3_grid.rdata",sep="/"))
  
# clean R
  rm(list=setdiff(ls(), "pathdir"))
  
# open original c-sqs
  load(paste(pathdir,"1-Input data/Region_csquare_grid_south.RData",sep="/"))
  tt           <- data.frame(bargrid@data$csquares,bargrid@data$long,bargrid@data$lat)
  colnames(tt) <- c("csquares","long","lat")
  tt$long      <- round(tt$long, digits = 4)
  tt$lat       <- round(tt$lat, digits = 4)
  
  # get all quarter csq long x lat
  tt1 <- tt ;  tt2 <- tt ;  tt3 <- tt ;   tt4 <- tt
  tt1$long   <- tt$long - 0.05/4
  tt1$lat    <- tt$lat - 0.05/4
  tt2$long   <- tt$long +  0.05/4
  tt2$lat    <- tt$lat +  0.05/4
  tt3$long   <- tt$long -  0.05/4
  tt3$lat    <- tt$lat +  0.05/4
  tt4$long   <- tt$long +  0.05/4
  tt4$lat    <- tt$lat - 0.05/4
  tt1$coords <- paste(tt1$long,tt1$lat)
  tt2$coords <- paste(tt2$long,tt2$lat)
  tt3$coords <- paste(tt3$long,tt3$lat)
  tt4$coords <- paste(tt4$long,tt4$lat)
  ttall      <- rbind(tt1,tt2,tt3,tt4) 
  
  rm(list=setdiff(ls(), c("ttall","pathdir")))
  
  # remove all from the 0.25 grid that is not in 
  load(paste(pathdir,"1-Input data/qc1_grid.rdata",sep="/"))
  qc1 <- cbind(qc1, ttall[match(qc1@data$uni,ttall$coords), c("csquares")])
  colnames(qc1@data)[5] <- "csquares"
  qc1 <- subset(qc1,!(is.na(qc1@data$csquares)))
  save(qc1, file = paste(pathdir,"1-Input data/qc1_grid.rdata",sep="/"))
  
  load(paste(pathdir,"1-Input data/qc2_grid.rdata",sep="/"))
  qc2 <- cbind(qc2, ttall[match(qc2@data$uni,ttall$coords), c("csquares")])
  colnames(qc2@data)[5] <- "csquares"
  qc2 <- subset(qc2,!(is.na(qc2@data$csquares)))
  save(qc2, file = paste(pathdir,"1-Input data/qc2_grid.rdata",sep="/"))
  
  load(paste(pathdir,"1-Input data/qc3_grid.rdata",sep="/"))
  qc3 <- cbind(qc3, ttall[match(qc3@data$uni,ttall$coords), c("csquares")])
  colnames(qc3@data)[5] <- "csquares"
  qc3 <- subset(qc3,!(is.na(qc3@data$csquares)))
  save(qc3, file = paste(pathdir,"1-Input data/qc3_grid.rdata",sep="/"))

  quar_grid <- rbind(qc1,qc2,qc3)

# save file
  save(quar_grid, file = paste(pathdir,"1-Input data/Region_0.25_csquare_grid_south.rdata",sep="/"))
  
  files <- dir(paste(pathdir,"1-Input data/",sep="/"), "qc")
  unlink(paste0(paste(pathdir,"1-Input data/",sep="/"), files))
