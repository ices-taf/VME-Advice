################################################
#### code to compile underlying data Table 1 ##
################################################

# load footprint workspace
  #load(paste(pathdir,"2-Data processing/Map_layers_workspace.RData",sep="/"))  
  load(paste(pathdir,"2-Data processing", paste(datacallyear_VMS, fishing_data, "Map_layers_workspace.RData", sep = "_"), sep="/"))

# select all new c-squares
  VMEgrid_old$uni  <- paste(VMEgrid_old$csquares,VMEgrid_old$VME_Class)  
  VMEgrid_tot      <- VMEgrid_new 
  VMEgrid_new$uni  <- paste(VMEgrid_new$csquares,VMEgrid_new$VME_Class)
  VMEgrid_new      <- VMEgrid_new[!(VMEgrid_new$uni %in% VMEgrid_old$uni ),]

# load all regions
  load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  

  
# load depths
  load(paste(pathdir,"1-Input data/Region_depth_prelim.RData",sep="/"))

  IREG <- subset(depth,!(depth$min_depth_emodnet > 200))
  IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 50)) 
  IREG$within <- 1  # if TRUE
  depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
  colnames(depth)[ncol(depth)] <- "de05_2"
  depth$de05_2[is.na(depth$de05_2)] <- 0 # if not TRUE
  
  
  # get area 200-400  
  IREG <- subset(depth,!(depth$min_depth_emodnet > 400))
  IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 200)) 
  IREG$within <- 1  # if TRUE
  depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
  colnames(depth)[ncol(depth)] <- "de2_4"
  depth$de2_4[is.na(depth$de2_4)] <- 0 # if not TRUE
  
  # get area 400-800
  IREG <- subset(depth,!(depth$min_depth_emodnet > 800))
  IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 400)) 
  IREG$within <- 1  # if TRUE
  depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
  colnames(depth)[ncol(depth)] <- "de4_8"
  depth$de4_8[is.na(depth$de4_8)] <- 0 # if not TRUE
  
  IREG <- subset(depth,depth$max_depth_emodnet > 800)
  IREG$within <- 1  # if TRUE
  depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
  colnames(depth)[ncol(depth)] <- "de8"
  depth$de8[is.na(depth$de8)] <- 0 # if not TRUE
  
  depth$cat <- ifelse(depth$de4_8 ==1,"de4_8",NA)
  depth$cat <- ifelse(depth$de2_4 ==1 & depth$de4_8 ==0,"de2_4",depth$cat)
  depth$cat <- ifelse(depth$de8 ==1 & depth$de4_8 ==0,"de8",depth$cat)
  
  # get depth ranges
  bargrid <- subset(bargrid,bargrid@data$csquares %in% depth$csquares)
  bargrid <- cbind(bargrid,depth[match(bargrid@data$csquares,depth$csquares),c("cat","mean_depth_emodnet")])
  bargrid <- subset(bargrid,!(is.na(bargrid@data$cat)))
  
  # select all c-squares
  bargrid <- cbind(bargrid,as.data.frame(VMEgrid_old)[match(bargrid@data$csquares,VMEgrid_old$csquares),c("VME_Class")])
  colnames(bargrid@data)[ncol(bargrid@data)] <- "Exist_VMEs"
  bargrid <- cbind(bargrid,as.data.frame(VMEgrid_new)[match(bargrid@data$csquares,VMEgrid_new$csquares),c("VME_Class")])
  colnames(bargrid@data)[ncol(bargrid@data)] <- "New_VMEs"
  bargrid <- cbind(bargrid,as.data.frame(VMEgrid_tot)[match(bargrid@data$csquares,VMEgrid_tot$csquares),c("VME_Class")])
  colnames(bargrid@data)[ncol(bargrid@data)] <- "Tot_VMEs"
  
  bargrid_sf <- st_as_sf(bargrid)
  
  # get old element data
  grid_over    <- st_intersects(bargrid_sf,Elements)
  idx          <- as.data.frame(grid_over)
  idx$csquare  <- bargrid_sf$csquares[idx$row.id]
  idx$element  <- Elements$Name[idx$col.id]
  dat          <- data.frame(csquare = unique(idx$csquare))
  banks        <- subset(idx,idx$element %in% c("bk","sedBk"))
  dat$banks[dat$csquare %in% unique(banks$csquare)] <- 1
  corMd        <- subset(idx,idx$element %in% c("corMd"))
  dat$corMd[dat$csquare %in% unique(corMd$csquare)] <- 1
  mudVolc        <- subset(idx,idx$element %in% c("mudVolc"))
  dat$mudVolc[dat$csquare %in% unique(mudVolc$csquare)] <- 1
  seaMt        <- subset(idx,idx$element %in% c("seaMt"))
  dat$seaMt[dat$csquare %in% unique(seaMt$csquare)] <- 1
  bargrid <- cbind(bargrid,dat[match(bargrid@data$csquares,dat$csquare),
                                                      c("banks","corMd","mudVolc","seaMt")])
  
  # add new data layers with difference between old and new VME element data (not included)

  # now add fishing layers
  grid_over    <- st_intersects(bargrid_sf,New_mobile)
  idx          <- as.data.frame(grid_over)
  bargrid@data$FareaM[bargrid@data$csquares %in% unique(bargrid_sf$csquares[idx$row.id])] <- 1
  
  grid_over    <- st_intersects(bargrid_sf,New_static)
  idx          <- as.data.frame(grid_over)
  bargrid@data$FareaS[bargrid@data$csquares %in% unique(bargrid_sf$csquares[idx$row.id])] <- 1
  
  grid_over    <- st_intersects(bargrid_sf,New_comb)
  idx          <- as.data.frame(grid_over)
  bargrid@data$FareaC[bargrid@data$csquares %in% unique(bargrid_sf$csquares[idx$row.id])] <- 1
  
  
  # add NEAFC Region
  grid_over    <- st_intersects(bargrid_sf,NEAFCReg)
  idx          <- as.data.frame(grid_over)
  idx$csquare  <- bargrid_sf$csquares[idx$row.id]
  idx$NEAFCregion  <- NEAFCReg$region[idx$col.id]
  bargrid <- merge(bargrid,idx[,3:4],by.x='csquares',by.y='csquare')
  

  # now add which c-squares are 100% within the old VME polygon
  tt <- st_intersection(VMEgrid_new,st_make_valid(scen11_prev))
  tt$area <- st_area(st_make_valid(tt))/10^6
  tt$frac <- as.numeric(tt$area/tt$area_sqkm)
  tt <- subset(tt,tt$frac> 0.9)
  bargrid@data$scen11[bargrid@data$csquares %in% unique(tt$csquares)] <- 1
  
  tt <- st_intersection(VMEgrid_new,st_make_valid(scen12_prev))
  tt$area <- st_area(st_make_valid(tt))/10^6
  tt$frac <- as.numeric(tt$area/tt$area_sqkm)
  tt <- subset(tt,tt$frac> 0.9)
  bargrid@data$scen12[bargrid@data$csquares %in% unique(tt$csquares)] <- 1
  
  tt <- st_intersection(VMEgrid_new,st_make_valid(scen21_prev))
  tt$area <- st_area(st_make_valid(tt))/10^6
  tt$frac <- as.numeric(tt$area/tt$area_sqkm)
  tt <- subset(tt,tt$frac> 0.9)
  bargrid@data$scen21[bargrid@data$csquares %in% unique(tt$csquares)] <- 1
  
  tt <- st_intersection(VMEgrid_new,st_make_valid(scen22_prev))
  tt$area <- st_area(st_make_valid(tt))/10^6
  tt$frac <- as.numeric(tt$area/tt$area_sqkm)
  tt <- subset(tt,tt$frac> 0.9)
  bargrid@data$scen22[bargrid@data$csquares %in% unique(tt$csquares)] <- 1
  
  bargrid@data$scen23 <- NA
  
  bargrid_sf2 <- st_as_sf(bargrid)
  table1 <- bargrid@data 
  

  
  
  
  
  
  