#
# estimate footprint based on latest VMS data
# 
# load grid
  load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  

# load depths
  load(paste(pathdir,"1-Input data/Region_depth_prelim.RData",sep="/"))
  IREG <- subset(depth,!(depth$min_depth_emodnet > 800))
  IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 400)) 
  IREG$within <- 1  # if TRUE
  depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
  colnames(depth)[ncol(depth)] <- "within"
  depth$within[is.na(depth$within)] <- 0 # if not TRUE

# get region within 400-800 meter
  Reg_w <- cbind(bargrid,depth[match(bargrid@data$csquares,depth$csquares),c("within")])
  colnames(Reg_w@data)[ncol(Reg_w@data)] <- "within"
  Reg_w <- subset(Reg_w@data,Reg_w@data$within == 1)

# get fishing data - mobile and static
  vmsreg             <- readRDS(paste(pathdir_nogit,paste("VMS data repository/All_VMS_datacall",datacallyear,".rds",sep=""),sep="/"))  
  
  # get c-sq with mobile fishing
  nam_footprint      <- c(paste("SAR_total",refyear_footprint,sep="_"))
  indexcol_footprint <- which(names(vmsreg) %in% nam_footprint) 
  vmsreg$mob         <- rowSums(vmsreg[indexcol_footprint],na.rm=T)
  vmsreg$mob[vmsreg$mob > 0] <- 1
  vmsreg$mob[is.na(vmsreg$mob)] <- 0
  
  # get c-sq with static fishing  
  nam_footprint <- c(paste("Static",refyear_footprint,sep="_"))
  indexcol_footprint <- which(names(vmsreg) %in% nam_footprint) 
  vmsreg$stat <- rowSums(vmsreg[indexcol_footprint],na.rm=T)
  vmsreg$stat[vmsreg$stat > 0] <- 1
  vmsreg$stat[is.na(vmsreg$stat)] <- 0
  
  # get c-sq with any fishing
  vmsreg$comb <- vmsreg$mob + vmsreg$stat
  vmsreg$comb[vmsreg$comb > 0] <- 1
  
# combine with 400-800 region
  Reg_w <- cbind(Reg_w, vmsreg[match(Reg_w$csquares,vmsreg$c_square), c("mob","stat","comb")])

# calculate mobile footprint  
  
  # calculate adjacent squares to apply footprint scenario
  dat      <- Reg_w[Reg_w$mob == 1,]
  dat$long <- round(dat$long,3)
  dat$lat  <- round(dat$lat,3)
  lonlats  <- paste(dat$long, dat$lat, sep=":")

  for(i in (1:dim(dat)[1])){
    
    nw  <- paste(dat$long[i]-0.05, dat$lat[i]+0.05, sep=":")
    nn  <- paste(dat$long[i]+0.00, dat$lat[i]+0.05, sep=":")
    ne  <- paste(dat$long[i]+0.05, dat$lat[i]+0.05, sep=":")
    ee  <- paste(dat$long[i]+0.05, dat$lat[i]+0.00, sep=":")
    se  <- paste(dat$long[i]+0.05, dat$lat[i]-0.05, sep=":")
    ss  <- paste(dat$long[i]+0.00, dat$lat[i]-0.05, sep=":")
    sw  <- paste(dat$long[i]-0.05, dat$lat[i]-0.05, sep=":")
    ww  <- paste(dat$long[i]-0.05, dat$lat[i]+0.00, sep=":")
    
    dat$adjacent.cells[i] <- sum(nw %in% lonlats[-i], nn %in% lonlats[-i], ne %in% lonlats[-i],
                                 ee %in% lonlats[-i], se %in% lonlats[-i], ss %in% lonlats[-i],
                                 sw %in% lonlats[-i], ww %in% lonlats[-i])
  }
    
  Reg_w <- merge(x = Reg_w, y = dat[ , c("csquares", "adjacent.cells")], by = "csquares", all.x=TRUE)
  Reg_w$adjacent.cells[is.na(Reg_w$adjacent.cells)] <- 0
  Reg_w$adjacent.cells[Reg_w$adjacent.cells > 0 ] <- 1
  colnames(Reg_w)[ncol(Reg_w)] <- "MBCG_footprint"

# calculate static footprint  
  
  # calculate adjacent squares to apply footprint scenario
  dat      <- Reg_w[Reg_w$stat == 1,]
  dat$long <- round(dat$long,3)
  dat$lat  <- round(dat$lat,3)
  lonlats  <- paste(dat$long, dat$lat, sep=":")
  
  for(i in (1:dim(dat)[1])){
    
    nw  <- paste(dat$long[i]-0.05, dat$lat[i]+0.05, sep=":")
    nn  <- paste(dat$long[i]+0.00, dat$lat[i]+0.05, sep=":")
    ne  <- paste(dat$long[i]+0.05, dat$lat[i]+0.05, sep=":")
    ee  <- paste(dat$long[i]+0.05, dat$lat[i]+0.00, sep=":")
    se  <- paste(dat$long[i]+0.05, dat$lat[i]-0.05, sep=":")
    ss  <- paste(dat$long[i]+0.00, dat$lat[i]-0.05, sep=":")
    sw  <- paste(dat$long[i]-0.05, dat$lat[i]-0.05, sep=":")
    ww  <- paste(dat$long[i]-0.05, dat$lat[i]+0.00, sep=":")
    
    dat$adjacent.cells[i] <- sum(nw %in% lonlats[-i], nn %in% lonlats[-i], ne %in% lonlats[-i],
                                 ee %in% lonlats[-i], se %in% lonlats[-i], ss %in% lonlats[-i],
                                 sw %in% lonlats[-i], ww %in% lonlats[-i])
  }
  
  Reg_w <- merge(x = Reg_w, y = dat[ , c("csquares", "adjacent.cells")], by = "csquares", all.x=TRUE)
  Reg_w$adjacent.cells[is.na(Reg_w$adjacent.cells)] <- 0
  Reg_w$adjacent.cells[Reg_w$adjacent.cells > 0 ] <- 1
  colnames(Reg_w)[ncol(Reg_w)] <- "Static_footprint"
  
# calculate combined footprint  
  
  # calculate adjacent squares to apply footprint scenario
  dat      <- Reg_w[Reg_w$comb == 1,]
  dat$long <- round(dat$long,3)
  dat$lat  <- round(dat$lat,3)
  lonlats  <- paste(dat$long, dat$lat, sep=":")
  
  for(i in (1:dim(dat)[1])){
    
    nw  <- paste(dat$long[i]-0.05, dat$lat[i]+0.05, sep=":")
    nn  <- paste(dat$long[i]+0.00, dat$lat[i]+0.05, sep=":")
    ne  <- paste(dat$long[i]+0.05, dat$lat[i]+0.05, sep=":")
    ee  <- paste(dat$long[i]+0.05, dat$lat[i]+0.00, sep=":")
    se  <- paste(dat$long[i]+0.05, dat$lat[i]-0.05, sep=":")
    ss  <- paste(dat$long[i]+0.00, dat$lat[i]-0.05, sep=":")
    sw  <- paste(dat$long[i]-0.05, dat$lat[i]-0.05, sep=":")
    ww  <- paste(dat$long[i]-0.05, dat$lat[i]+0.00, sep=":")
    
    dat$adjacent.cells[i] <- sum(nw %in% lonlats[-i], nn %in% lonlats[-i], ne %in% lonlats[-i],
                                 ee %in% lonlats[-i], se %in% lonlats[-i], ss %in% lonlats[-i],
                                 sw %in% lonlats[-i], ww %in% lonlats[-i])
  }
  
  Reg_w <- merge(x = Reg_w, y = dat[ , c("csquares", "adjacent.cells")], by = "csquares", all.x=TRUE)
  Reg_w$adjacent.cells[is.na(Reg_w$adjacent.cells)] <- 0
  Reg_w$adjacent.cells[Reg_w$adjacent.cells > 0 ] <- 1
  colnames(Reg_w)[ncol(Reg_w)] <- "Both_footprint"
 
# now make spatial objects
  barsub    <- subset(bargrid,bargrid@data$csquares %in% Reg_w$csquares)
  Footprint <- cbind(barsub, Reg_w[match(barsub@data$csquares,Reg_w$csquares), 
                                    c("MBCG_footprint","Static_footprint","Both_footprint")])
  
  # for mobile footprint
  Footprint_mobile <- subset(Footprint,Footprint@data$MBCG_footprint == 1)
  Freg <- unionSpatialPolygons(Footprint_mobile,Footprint_mobile$MBCG_footprint)
  Footprint_mobile <- gUnaryUnion(Freg)
  Footprint_mobile   <- st_as_sf(Footprint_mobile)
  Footprint_mobile <-  st_transform(Footprint_mobile, "EPSG:4326")
  write_sf(Footprint_mobile, paste0(paste(pathdir,"2-Data processing/",sep="/"),"Footprint_mobile.shp"))
  
  # for static footprint
  Footprint_static <- subset(Footprint,Footprint@data$Static_footprint == 1)
  Freg <- unionSpatialPolygons(Footprint_static,Footprint_static$Static_footprint)
  Footprint_static <- gUnaryUnion(Freg)
  Footprint_static   <- st_as_sf(Footprint_static)
  Footprint_static <-  st_transform(Footprint_static, "EPSG:4326")
  write_sf(Footprint_static, paste0(paste(pathdir,"2-Data processing/",sep="/"),"Footprint_static.shp"))
  
  # for combined footprint
  Footprint_both <- subset(Footprint,Footprint@data$Both_footprint == 1)
  Freg <- unionSpatialPolygons(Footprint_both,Footprint_both$Both_footprint)
  Footprint_both <- gUnaryUnion(Freg)
  Footprint_both   <- st_as_sf(Footprint_both)
  Footprint_both <-  st_transform(Footprint_both, "EPSG:4326")  
  write_sf(Footprint_both, paste0(paste(pathdir,"2-Data processing/",sep="/"),"Footprint_both.shp"))
  
  # and clean
  rm(list=setdiff(ls(), c("pathdir" , "pathdir_nogit","NEAFCFootp",
                          "EUFootp","EUFootp_mob","EUFootp_stat","Footprint_both",
                          "Footprint_static","Footprint_mobile")))
  