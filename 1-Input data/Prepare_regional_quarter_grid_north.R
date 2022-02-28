
# get one quarter of a c-square 0.05 x 0.05 grid, this is used to estimate buffers 
# computer might not manage...

# install libraries
  library(rgdal)
  library(sp)
  library(raster)

# set folder directory
  pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"

  xx <- c(-46.9375,-46.9375,12.9375,12.9375)
  yy <- c(56.0625, 73.0625,56.0625, 73.0625)
  xlen <- c(2400,2400,2300,2300)
  ylen <- c(700,677,700,677)
  
  for (p in 1:4){
    # create grid     
    gt               <-(GridTopology(c(xx[p],yy[p]), c(0.025, 0.025), c(xlen[p], ylen[p]))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
    grt               <-SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
    spix              <- as(grt, "SpatialPixels")
    spol              <- as(spix, "SpatialPolygons")
    rnames            <- sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
    LOCUNI            <- as.data.frame(seq(1,length(spix)))
    rownames(LOCUNI)  <- rnames
    quar_grid           <- SpatialPolygonsDataFrame(spol, LOCUNI)
    
    # save file
    save(quar_grid, file = paste(pathdir,paste("Region_0.25_csquare_grid_north",p,".rdata",sep=""),sep="/"))
    quar_grid@bbox 
    
    rm(list=setdiff(ls(), c("pathdir",'xx','yy','xlen','ylen','p','quar_grid')))
    
    # get coordinates
    coord               <- coordinates(quar_grid)
    quar_grid@data$long <- coord[,1]
    quar_grid@data$long <- round(quar_grid@data$long, digits = 4)
    quar_grid@data$lat  <- coord[,2]
    quar_grid@data$lat  <- round(quar_grid@data$lat, digits = 4)
    quar_grid@data$uni  <- paste(quar_grid@data$long,quar_grid@data$lat) 
    save(quar_grid, file = paste(pathdir,paste("Region_0.25_csquare_grid_north",p,".rdata",sep=""),sep="/"))
    
    # load the c-square file
    load(paste(pathdir,"Region_csquare_grid_north.RData",sep="/"))
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
    
    rm(list=setdiff(ls(), c("ttall","pathdir",'xx','yy','xlen','ylen','p','quar_grid')))
    
    quar_grid <- cbind(quar_grid, ttall[match(quar_grid@data$uni,ttall$coords), c("csquares")])
    colnames(quar_grid@data)[5] <- "csquares"
    quar_grid <- subset(quar_grid,!(is.na(quar_grid@data$csquares)))
    
    # save file
    save(quar_grid, file = paste(pathdir,paste("Region_0.25_csquare_grid_north",p,".rdata",sep=""),sep="/"))
    
    rm(list=setdiff(ls(), c("pathdir",'xx','yy','xlen','ylen','p')))
  }