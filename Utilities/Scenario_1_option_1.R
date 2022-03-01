
### scenario 1 -- option 1

  load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  

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

# add VMEs
  VME <- read.csv(paste(pathdir_nogit,
                        "VME data repository/VME observations and csquares/VME_csquares_datacall_2020.csv",sep="/"),
                  header=T,sep=",",row.names = NULL)
  VME <- as.data.frame(VME)
  VME <- VME[,-1]
  
# create VME spatial grid
  VMEgrid       <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
  VMEgrid       <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
  colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
  VMEgrid       <- subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class)))

# and select all habitat + index high and medium
  VME_high      <- VMEgrid@data[VMEgrid@data$VME_Class %in% c(3,2,1),]
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
  uni_cquare <- c(unique(uni_cquare$csquares),VME_high$csquares,VME_low$csquares)
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
    
  # now get all grid cells that should be closed 
    quar_grid@data$summing <- rowSums(quar_grid@data[,c("buffer","buffer_low","VME","VME_low")],na.rm = T) 
    sce11 <- subset(quar_grid,quar_grid@data$summing > 0)
    sce11 <- spTransform(sce11, CRS("+init=epsg:4326"))
    assign(nam[iGrid],sce11)  
  }
    
# save 0.25 c-sq output
  sce11 <- rbind(north1,north2,north3,north4,south)
  sce11 <- sce11[,-1]
  rownames(sce11) <- NULL
  sce11 <- sce11[!(duplicated(sce11@data$uni)),]
  save(sce11,file=paste(pathdir,"2-Data processing/sce11_quarter_csq_grid.RData",sep="/"))
  
## fill holes
  sce11$summing <-  1
  tt <- unionSpatialPolygons(sce11,sce11$summing)
  reg <- gUnaryUnion(tt)
  
  # add gbuffer to make sure that filling holes works
  reg <- spTransform(reg, CRS( "+init=epsg:3347" ) ) 
  reg <- gBuffer(reg,width=0.0001)
  reg <- spTransform( reg, CRS("+init=epsg:4326")  )
  
  reg   <- st_as_sf(reg)
  reg   <- st_make_valid(reg) 
  area_thresh <- units::set_units(50, km^2) # this should be 2 c-sq (but mininum size of c-sq = 13, max = 25) -- problematic
  reg_dropped <- fill_holes(reg, threshold = area_thresh)

# write to shp file
  reg_dropped <- st_set_precision(reg_dropped,precision = 10000)
  clos11 <- st_cast(reg_dropped,"POLYGON")
  clos11$id <- 1:nrow(clos11) 
  write_sf(clos11, paste0(paste(pathdir,"2-Data processing/",sep="/"),"Scenario1_option1.shp"))
  
# and clean
rm(list=setdiff(ls(), c("pathdir" , "pathdir_nogit")))
