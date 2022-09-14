#
# estimate fished area based on latest VMS data  - this might be a too heavy calculation with all ecoregions
# 
# load grid
  load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  

# load depths
  load(paste(pathdir,"1-Input data/Region_depth_prelim.RData",sep="/"))
  depth <- subset(depth, !(depth$max_depth_emodnet  < 200)) 

# get region with depth > 200 meter
  Reg_w <- subset(bargrid, bargrid@data$csquares %in% depth$csquares)
  
# get fishing data - mobile and static
  vmsreg <- readRDS(paste(pathdir_nogit,paste("VMS data repository/All_VMS_datacall",datacallyear_VMS,".rds",sep=""),sep="/"))  
  
# get area fished reference years
  # get c-sq with mobile fishing
  nam_fished      <- c(paste("SAR_total",refyear_fished,sep="_"))
  indexcol_fished <- which(names(vmsreg) %in% nam_fished) 
  vmsreg$mobref         <- rowSums(vmsreg[indexcol_fished],na.rm=T)
  vmsreg$mobref[vmsreg$mobref > 0] <- 1
  vmsreg$mobref[is.na(vmsreg$mobref)] <- 0
  
  # get c-sq with static fishing  
  nam_fished <- c(paste("Static",refyear_fished,sep="_"))
  indexcol_fished <- which(names(vmsreg) %in% nam_fished) 
  vmsreg$statref <- rowSums(vmsreg[indexcol_fished],na.rm=T)
  vmsreg$statref[vmsreg$statref > 0] <- 1
  vmsreg$statref[is.na(vmsreg$statref)] <- 0

  # get combined
  vmsreg$combref <- vmsreg$statref + vmsreg$mobref
  vmsreg$combref[vmsreg$combref > 0] <- 1
  
# get area fished latest years
  # get c-sq with mobile fishing
  nam_fished      <- c(paste("SAR_total",newyear_fished,sep="_"))
  indexcol_fished <- which(names(vmsreg) %in% nam_fished) 
  vmsreg$mobnew         <- rowSums(vmsreg[indexcol_fished],na.rm=T)
  vmsreg$mobnew[vmsreg$mobnew > 0] <- 1
  vmsreg$mobnew[is.na(vmsreg$mobnew)] <- 0
  
  # get c-sq with static fishing  
  nam_fished <- c(paste("Static",newyear_fished,sep="_"))
  indexcol_fished <- which(names(vmsreg) %in% nam_fished) 
  vmsreg$statnew <- rowSums(vmsreg[indexcol_fished],na.rm=T)
  vmsreg$statnew[vmsreg$statnew > 0] <- 1
  vmsreg$statnew[is.na(vmsreg$statnew)] <- 0
  
  # get combined
  vmsreg$combnew <- vmsreg$statnew + vmsreg$mobnew
  vmsreg$combnew[vmsreg$combnew > 0] <- 1
  
# combine fishing data with depth >200 region
  Reg_w <- cbind(Reg_w, vmsreg[match(Reg_w$csquares,vmsreg$c_square), c("mobref","statref","combref",
                                                                        "mobnew","statnew","combnew")])

  # for mobile fishing
  Ref_mobile   <- subset(Reg_w,Reg_w@data$mobref == 1)
  Ref_mobile   <- raster::aggregate(Ref_mobile)
  Ref_mobile   <- gUnaryUnion(Ref_mobile)
  Ref_mobile   <- st_as_sf(Ref_mobile)
  Ref_mobile   <-  st_transform(Ref_mobile, "EPSG:4326")

  New_mobile   <- subset(Reg_w,Reg_w@data$mobnew == 1)
  New_mobile   <- raster::aggregate(New_mobile)
  New_mobile   <- gUnaryUnion(New_mobile)
  New_mobile   <- st_as_sf(New_mobile)
  New_mobile   <-  st_transform(New_mobile, "EPSG:4326")
  
  # for static fishing
  Ref_static   <- subset(Reg_w,Reg_w@data$statref == 1)
  Ref_static   <- raster::aggregate(Ref_static)
  Ref_static   <- gUnaryUnion(Ref_static)
  Ref_static   <- st_as_sf(Ref_static)
  Ref_static   <-  st_transform(Ref_static, "EPSG:4326")
  
  New_static   <- subset(Reg_w,Reg_w@data$statnew == 1)
  New_static   <- raster::aggregate(New_static)
  New_static   <- gUnaryUnion(New_static)
  New_static   <- st_as_sf(New_static)
  New_static   <-  st_transform(New_static, "EPSG:4326")
  
  # for combined fishing
  Ref_comb   <- subset(Reg_w,Reg_w@data$combref == 1)
  Ref_comb   <- raster::aggregate(Ref_comb)
  Ref_comb   <- gUnaryUnion(Ref_comb)
  Ref_comb   <- st_as_sf(Ref_comb)
  Ref_comb   <-  st_transform(Ref_comb, "EPSG:4326")
  
  New_comb   <- subset(Reg_w,Reg_w@data$combnew == 1)
  New_comb   <- raster::aggregate(New_comb)
  New_comb   <- gUnaryUnion(New_comb)
  New_comb   <- st_as_sf(New_comb)
  New_comb   <-  st_transform(New_comb, "EPSG:4326")
  
  # and clean
  rm(Reg_w,depth,vmsreg,indexcol_fished,nam_fished,refyear_fished,bargrid)
  