### scenario 2 -- option 3
  load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/")) 
  
  # load VME observations
  VMEobs <- read.csv(paste(pathdir_nogit,paste(
    "VME data repository/VME observations and csquares/VME_observations_datacall_",
    datacallyear,".csv",sep=""),sep="/"),
    header=T,sep=",",row.names = NULL)
  
  # get all in ICES area
  VMEobs <- subset(VMEobs,VMEobs$StartLongitude > -20)
  
  # get all presences
  VMEobs <- subset(VMEobs,!(VMEobs$VME_Indicator =="NULL" & VMEobs$HabitatType =="NULL")) 
  
  # create polypoints based on the middle lat and long
  data <- data.frame(VMEid =VMEobs$ï..ICES_ID,
                     VME_longitude = VMEobs$MiddleLongitude,
                     VME_latitude  = VMEobs$MiddleLatitude,
                     stringsAsFactors = F)
  
  coordinates(data) <- c("VME_longitude","VME_latitude")
  data@data$VME_longitude <- VMEobs$MiddleLongitude
  data@data$VME_latitude <- VMEobs$MiddleLatitude
  VMEobs_points <- data
  
  # load VME elements
  Bank       <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Bank.shp",sep="/"))
  Bank       <- st_make_valid(Bank)
  Coralmound <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_CoralMounds.shp",sep="/"))
  Coralmound <- st_make_valid(Coralmound)
  Mudvolcano <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Mud_Volcano.shp",sep="/"))
  Mudvolcano <- st_make_valid(Mudvolcano)
  Seamount   <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Seamount.shp",sep="/"))
  Seamount   <- st_make_valid(Seamount)
  Elements   <- rbind(Bank,Coralmound,Mudvolcano,Seamount)
  
  # change VME observations to same projection
  Proj     <- as(Bank, 'Spatial')
  proj4string(VMEobs_points) <- CRS(proj4string(Proj)) 
  VMEobs_points <- st_as_sf(VMEobs_points)
  VMEobs_poly   <- VMEobs_points
  
  # get all VME elements that overlap with VME observations
  Elements_over   <- st_intersects(Elements,VMEobs_poly)
  idx             <- as.data.frame(Elements_over)
  idx             <- unique(idx[,1])
  Elements        <- Elements[c(idx),]
  
  # overlay with csquare grid 
  element_grid    <- bargrid
  element_grid    <- st_as_sf(element_grid)
  grid_over       <- st_intersects(element_grid,Elements)
  idx             <- as.data.frame(grid_over)
  idx             <- unique(idx[,1])
  element_grid    <- element_grid[c(idx),]
  unicsq          <- element_grid$csquares   
  
  # select all vme element grid cells that are closed
  element_close <- subset(bargrid,bargrid@data$csquares %in% unicsq)
  element_close@data$elementclose <- 1  

# now continue as scenario 2 option 1 but remove all VMEs inside element as these are already protected,
# unless the VMEs are isolated c-squares
  
  # add VMEs
  VME <- read.csv(paste(pathdir_nogit,paste(
    "VME data repository/VME observations and csquares/VME_csquares_datacall_",
    datacallyear,".csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]
  VME <-  cbind(VME, bargrid@data[match(VME$CSquare,bargrid@data$csquares), c("long","lat")])
  VME$uni <- paste(VME$long,VME$lat)
  
  # find all isolated c-squares within elements that are also not connected to another VME habitat/index
  element_close@data$uni   <- paste(element_close@data$long, element_close@data$lat)
  
  isocsquare <- c()
  for (j in 1:nrow(element_close@data)){
    long <- element_close@data$long[j]
    lat <- element_close@data$lat[j]
    
    sublong <- c(long,long-0.05,long+0.05)
    sublat  <- c(lat,lat-0.05,lat+0.05)
    all <- merge(sublong,sublat)
    uni <- paste(all[,1],all[,2])
    
    # nb of c-squares with elements that are surrounding
    nb <- length(which(!(is.na(match(element_close@data$uni,uni)))))
    if(nb == 1){
      
      # check if other VMEs are connected
      nb2 <- length(which(!(is.na(match(VME$uni,uni)))))
      if (nb2 == 1){
        element_close@data$elementclose[j] <- 100
      }
    }
  }
  
  # get all long x lat at 0.25 c-square format
  tt <- bargrid@data
  tt$long <- round(tt$long, digits = 4)
  tt$lat <- round(tt$lat, digits = 4)
  
  tt1 <- tt
  tt2 <- tt
  tt3 <- tt
  tt4 <- tt
  
  tt1$long <- tt$long - 0.05/4
  tt1$lat  <- tt$lat - 0.05/4
  tt2$long <- tt$long +  0.05/4
  tt2$lat  <- tt$lat +  0.05/4
  tt3$long <- tt$long -  0.05/4
  tt3$lat  <- tt$lat +  0.05/4
  tt4$long <- tt$long +  0.05/4
  tt4$lat  <- tt$lat - 0.05/4
  tt1$coords <- paste(tt1$long,tt1$lat)
  tt2$coords <- paste(tt2$long,tt2$lat)
  tt3$coords <- paste(tt3$long,tt3$lat)
  tt4$coords <- paste(tt4$long,tt4$lat)
  
  # remove all VMEs inside element as these are already protected (and not buffered)
  VME <- cbind(VME, element_close@data[match(VME$CSquare,element_close@data$csquares), c("elementclose")])
  colnames(VME)[ncol(VME)] <-"elementclose"
  VME$elementclose[is.na(VME$elementclose)] <- 100 # same number as the isolated c-squares as above
  VME <- subset(VME,VME$elementclose == 100)
  
  # continue as in scenario 2 option 1
  # create VME spatial grid
  VMEgrid       <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
  VMEgrid       <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
  colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
  VMEgrid       <- subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class)))
  
  # get vms data
  vmsreg <- readRDS(paste(pathdir_nogit,paste("VMS data repository/All_VMS_datacall",datacallyear,".rds",sep=""),sep="/"))  
  nam <- c(paste("SAR_total",refyear,sep="_"))
  indexcol <- which(names(vmsreg) %in% nam) 
  vmsreg$SAR <- rowMeans(vmsreg[indexcol],na.rm=T)
  
  # add to grid
  VMEgrid <- cbind(VMEgrid, vmsreg[match(VMEgrid@data$csquares,vmsreg$c_square), c("SAR")])
  colnames(VMEgrid@data)[ncol(VMEgrid@data)] <- "SAR" 
  VMEgrid@data$SAR[is.na(VMEgrid@data$SAR)] <- 0
  VMEgrid@data$VME_Class[VMEgrid@data$VME_Class == 0 & VMEgrid@data$SAR < SAR_threshold] <- 5 # low index with low SAR
  
  # and select all habitat + index high and medium + low index with low SAR
  VME_high      <- VMEgrid@data[VMEgrid@data$VME_Class %in% c(3,2,1,5),]
  VME_high$long <- round(VME_high$long, digits = 3)
  VME_high$lat  <- round(VME_high$lat, digits = 3)

  # create buffer around all closed VMEs
  tt1$buffer <- NA
  tt2$buffer <- NA
  tt3$buffer <- NA
  tt4$buffer <- NA
  
  for (i in 1:nrow(VME_high)){
    long <- VME_high$long[i]
    lat  <- VME_high$lat[i]
    
    sublong <- c(long-(0.05*0.75),long-(0.05*0.25),long+(0.05*0.25),long+(0.05*0.75))
    sublong <- round(sublong, digits = 4)
    sublat  <- c(lat-(0.05*0.75),lat-(0.05*0.25),lat+(0.05*0.25),lat+(0.05*0.75))
    sublat <- round(sublat, digits = 4)
    
    all <- merge(sublong,sublat)
    uni <- paste(all[,1],all[,2])
    uni <- data.frame(uni,1)
    
    tt1[tt1$coords %in% c(as.character(uni[,1])),"buffer"] <- 100
    tt2[tt2$coords %in% c(as.character(uni[,1])),"buffer"] <- 100
    tt3[tt3$coords %in% c(as.character(uni[,1])),"buffer"] <- 100
    tt4[tt4$coords %in% c(as.character(uni[,1])),"buffer"] <- 100
  }
  
  # select all VME low index that are adjacent/joining
  VME_low <- VMEgrid@data[VMEgrid@data$VME_Class %in% c(0),]
  VME_low$uni <- paste(VME_low$long,VME_low$lat)
  VME_low$joining <- NA
  VME_low$long <- round(VME_low$long, digits = 4)
  VME_low$lat <- round(VME_low$lat, digits = 4)
  
  for (i in 1:nrow(VME_high)){
    long <- VME_high$long[i]
    lat  <- VME_high$lat[i]
    
    sublong <- c(long-(0.05),long+(0.05),long)
    sublong <- round(sublong, digits = 4)
    sublat  <- c(lat-(0.05),lat+(0.05),lat)
    sublat <- round(sublat, digits = 4)
    
    all <- merge(sublong,sublat)
    uni <- paste(all[,1],all[,2])
    uni <- data.frame(uni,1)
    VME_low[VME_low$uni %in% c(as.character(uni[,1])),"joining"] <- 100
  }
  
  # and add buffer around all adjacent/joining low index cells
  tt1$buffer_low <- NA
  tt2$buffer_low <- NA
  tt3$buffer_low <- NA
  tt4$buffer_low <- NA
  
  VME_low <- subset(VME_low,VME_low$joining == 100)
  for (i in 1:nrow(VME_low)){
    long <- VME_low$long[i]
    lat  <- VME_low$lat[i]
    
    sublong <- c(long-(0.05*0.75),long-(0.05*0.25),long+(0.05*0.25),long+(0.05*0.75))
    sublong <- round(sublong, digits = 4)
    sublat  <- c(lat-(0.05*0.75),lat-(0.05*0.25),lat+(0.05*0.25),lat+(0.05*0.75))
    sublat <- round(sublat, digits = 4)
    
    all <- merge(sublong,sublat)
    uni <- paste(all[,1],all[,2])
    uni <- data.frame(uni,1)
    
    tt1[tt1$coords %in% c(as.character(uni[,1])),"buffer_low"] <- 100
    tt2[tt2$coords %in% c(as.character(uni[,1])),"buffer_low"] <- 100
    tt3[tt3$coords %in% c(as.character(uni[,1])),"buffer_low"] <- 100
    tt4[tt4$coords %in% c(as.character(uni[,1])),"buffer_low"] <- 100
  }
  
  ttall <- rbind(tt1,tt2,tt3,tt4)
  rm("tt1","tt2","tt3","tt4","bargrid")  
  
  ## get 0.25 c-square grid to combine all data
  uni_cquare <- subset(ttall,ttall$buffer == 100 | ttall$buffer_low == 100)
  uni_cquare <- c(unique(uni_cquare$csquares),VME_high$csquares,VME_low$csquares,element_close$csquares)
  uni_cquare <- unique(uni_cquare)
  
  # this file is too big - so added in a loop (warnings are okay)
  nam <- c("south","north1","north2","north3","north4")
  
  for (iGrid in 1:5){
    load(paste(pathdir,paste(paste("1-Input data/Region_0.25_csquare_grid",nam[iGrid],sep="_"),".RData",sep=""),sep="/"))
    
    # select all quarter grids that are important
    quar_grid <- subset(quar_grid,quar_grid@data$csquares %in% uni_cquare)
    
    # get all quarter c-sq with buffer based on longitude - latitude
    quar_grid <- cbind(quar_grid, ttall[match(quar_grid@data$uni,ttall$coords), c("buffer","buffer_low")])
    
    # add all VMEs (habitat + high/medium) cells based on c-sq id
    quar_grid <- cbind(quar_grid, VME_high[match(quar_grid@data$csquares,VME_high$csquares), c("VME_Class")])
    colnames(quar_grid@data)[ncol(quar_grid@data)] <- "VME"
    
    # add all selected VME low cells based on c-sq id
    quar_grid <- cbind(quar_grid, VME_low[match(quar_grid@data$csquares,VME_low$csquares), c("VME_Class")])
    colnames(quar_grid@data)[ncol(quar_grid@data)]  <- "VME_low"
    
    # add all selected VME low cells based on c-sq id
    quar_grid <- cbind(quar_grid,  element_close@data[match(quar_grid@data$csquares, element_close@data$csquares), c("elementclose")])
    colnames(quar_grid@data)[ncol(quar_grid@data)]  <- "elementclose"
    
    # now get all grid cells that should be closed 
    quar_grid@data$summing <- rowSums(quar_grid@data[,c("buffer","buffer_low","VME","VME_low","elementclose")],na.rm = T) 
    sce23 <- subset(quar_grid,quar_grid@data$summing > 0)
    sce23 <- spTransform(sce23, CRS("+init=epsg:4326"))
    assign(nam[iGrid],sce23)  
  }
  
  # save 0.25 c-sq output
  sce23 <- rbind(north1,north2,north3,north4,south)
  sce23 <- sce23[,-1]
  rownames(sce23) <- NULL
  sce23 <- sce23[!(duplicated(sce23@data$uni)),]
  save(sce23,file=paste(pathdir,"2-Data processing/sce23_quarter_csq_grid.RData",sep="/"))
  
  ## fill holes
  sce23$summing <-  1
  tt <- unionSpatialPolygons(sce23,sce23$summing)
  reg <- gUnaryUnion(tt)
  
  # add gbuffer to make sure that filling holes works
  reg <- spTransform(reg, CRS( "+init=epsg:3347" ) ) 
  reg <- gBuffer(reg,width=0.0001)
  reg <- spTransform( reg, CRS("+init=epsg:4326")  )
  reg   <- st_as_sf(reg)
  reg   <- st_make_valid(reg)
  reg  <- st_cast(reg,"POLYGON")
  
  # since size of area differs with latitude 
  midlat  <- round(coordinates(as_Spatial(reg))[,2]) # get midpoint
  load(paste(pathdir,"Utilities/Gridsize_csquare_latitude.RData",sep="/")) # get size of c-sq per latitude
  midlat <- cbind(midlat, gridsize[match(midlat,gridsize$latitude), c(2)])  # combine
  colnames(midlat)[2] <- "area_km2" 
  
  # estimate threshold size - this should be 2 c-sq  
  # multiply with 2.2 as mid-point and upper latitudinal boundary of each polygon may vary
  area_thresh <- units::set_units(midlat[,2]*2.2, km^2) 
  
  # check for holes
  reg_dropped <- fill_holes(reg[1,], threshold = area_thresh[1]) # fill first 
  
  for (iclos in 2:length(area_thresh)){
    reg_fill <- fill_holes(reg[iclos,], threshold = area_thresh[iclos]) # and all others
    reg_dropped <- rbind(reg_dropped,reg_fill)
  }
  
  # write to shp file
  reg_dropped <- st_set_precision(reg_dropped,precision = 10000)
  clos23 <- reg_dropped
  clos23$id <- 1:nrow(clos23) 
  write_sf(clos23, paste0(paste(pathdir,"2-Data processing/",sep="/"),"Scenario2_option3.shp"))
  
  # and clean
  rm(list=setdiff(ls(), c("pathdir" , "pathdir_nogit","datacallyear","refyear","SAR_threshold")))  
  