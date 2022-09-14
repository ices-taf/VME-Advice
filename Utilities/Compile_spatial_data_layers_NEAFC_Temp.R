#-------------------------------------------------------------------------------------
# script to obtain all spatial data layers for rmarkdown
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
# load VME polygons from last year (datacallyear-1)
 dir_polprev  <- paste(pathdir,paste("2-Data processing/VME_polygons",datacallyear-1,sep="_"),sep="/")
 scen11_prev  <- st_read(paste(dir_polprev,"Scenario1_option1.shp",sep="/"))
 scen12_prev  <- st_read(paste(dir_polprev,"Scenario1_option2.shp",sep="/"))
 scen21_prev  <- st_read(paste(dir_polprev,"Scenario2_option1.shp",sep="/"))
 scen22_prev  <- st_read(paste(dir_polprev,"Scenario2_option2.shp",sep="/"))
 scen23_prev  <- st_read(paste(dir_polprev,"Scenario2_option3.shp",sep="/"))
 rm(dir_polprev)
 
#-------------------------------------------------------------------------------------  
# load VME polygons from datacallyear
 dir_pol <- paste(pathdir,paste("2-Data processing/VME_polygons",datacallyear,sep="_"),sep="/")
 scen11  <- st_read(paste(dir_pol,"Scenario1_option1.shp",sep="/"))
 scen12  <- st_read(paste(dir_pol,"Scenario1_option2.shp",sep="/"))
 scen21  <- st_read(paste(dir_pol,"Scenario2_option1.shp",sep="/"))
 scen22  <- st_read(paste(dir_pol,"Scenario2_option2.shp",sep="/"))
 scen23  <- st_read(paste(dir_pol,"Scenario2_option3.shp",sep="/"))  
 rm(dir_pol)
 
#------------------------------------------------------------------------------------- 
# load VME physical elements
 Bank       <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Bank.shp",sep="/"))
 Bank       <- st_make_valid(Bank)
 Coralmound <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_CoralMounds.shp",sep="/"))
 Coralmound <- st_make_valid(Coralmound)
 Mudvolcano <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Mud_Volcano.shp",sep="/"))
 Mudvolcano <- st_make_valid(Mudvolcano)
 Seamount   <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Seamount.shp",sep="/"))
 Seamount   <- st_make_valid(Seamount)
 Elements   <- rbind(Bank,Coralmound,Mudvolcano,Seamount)
 rm(Bank,Coralmound,Mudvolcano,Seamount)
 
 
#-------------------------------------------------------------------------------------
# load all available VMEs from previous year (datacallyear - 1)
 VME <- read.csv(paste(pathdir_nogit,paste(
                 "VME data repository/VME observations and csquares/VME_csquares_datacall_",
                                  datacallyear-1,".csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
 VME <- as.data.frame(VME)
 VME <- VME[,-1]
 
 # create VME spatial grid
 load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  
 VMEgrid       <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
 VMEgrid       <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
 colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
 VMEgrid       <- st_as_sf(subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class))))
 VMEgrid_old <- VMEgrid %>% mutate(VME_Class_Lab = factor(case_when(
                                   VME_Class==3  ~ "VME Habitat" ,
                                   VME_Class==2  ~ "High VME Index" ,
                                   VME_Class==1  ~ "Med VME Index" ,
                                   VME_Class==0  ~ "Low VME Index"), 
                                   levels=c("VME Habitat","High VME Index","Med VME Index","Low VME Index")))

#-------------------------------------------------------------------------------------
# load latest VME information (datacallyear)
 VME <- read.csv(paste(pathdir_nogit,paste(
   "VME data repository/VME observations and csquares/VME_csquares_datacall_",
   datacallyear,".csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
 VME <- as.data.frame(VME)
 VME <- VME[,-1]
 
 VMEgrid       <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
 VMEgrid       <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
 colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
 VMEgrid       <- st_as_sf(subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class))))
 VMEgrid_new <- VMEgrid %>% mutate(VME_Class_Lab = factor(case_when(
                                   VME_Class==3  ~ "VME Habitat" ,
                                   VME_Class==2  ~ "High VME Index" ,
                                   VME_Class==1  ~ "Med VME Index" ,
                                   VME_Class==0  ~ "Low VME Index"), 
                                   levels=c("VME Habitat","High VME Index","Med VME Index","Low VME Index")))
 
 rm(VMEgrid,VME)
 

#-------------------------------------------------------------------------------------
# load Fishing layers latest year with available data (datacallyear_VMS)
 load(paste(pathdir,paste("2-Data processing/Fishing_layers",datacallyear_VMS,sep="_"),"Fishing_workspace.RData",sep="/"))

# load the temporary NEAFC fishing shapefiles to replace the ICES files created in an earlier script
New_comb       <- st_read(paste(pathdir_nogit,"VMS data repository/NewCombNEAFC.shp",sep="/"))
New_comb       <- st_make_valid(New_comb[,-c(1:ncol(New_comb)-1)])
New_static       <- st_read(paste(pathdir_nogit,"VMS data repository/NewStaticNEAFC.shp",sep="/"))
New_static       <- st_make_valid(New_static[,-c(1:ncol(New_static)-1)])
New_mobile       <- st_read(paste(pathdir_nogit,"VMS data repository/NewMobileNEAFC.shp",sep="/"))
New_mobile       <- st_make_valid(New_mobile[,-c(1:ncol(New_mobile)-1)])
  
#-------------------------------------------------------------------------------------
# load VME database summary used for the popup tables in the rmarkdown
 source(paste(pathdir,"Utilities/VME Database summary.R",sep="/"))  # (warnings are okay)
 
 
#-------------------------------------------------------------------------------------
# load current NEAFC VME closures (EU not available)
 source(paste(pathdir,"Utilities/Obtain_NEAFC_closures.R",sep="/"))  # NEAFC closures (warnings are okay)

 
#-------------------------------------------------------------------------------------
# obtain polygons for geographic areas
 
 # ices ecoregions
 ICESEcReg    <- st_read(paste(pathdir,"1-Input data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp",sep="/"))
 ICESEcReg     <- subset(ICESEcReg, Ecoregion %in% c("Bay of Biscay and the Iberian Coast","Celtic Seas",
                                                 "Greater North Sea", "Azores","Faroes",
                                                 "Greenland Sea","Arctic Ocean","Icelandic Waters",
                                                  "Barents Sea","Norwegian Sea","Oceanic Northeast Atlantic" ))
 ICESEcReg   <- st_make_valid(ICESEcReg)
 
 # NEAFC region
 NEAFCReg <- st_read(paste(pathdir,"1-Input data/NEAFC_regions.gpkg",sep="/"))
 
 # EEZs
 shapeEEZ  <- st_read(paste(pathdir,"1-Input data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp",sep="/"))  # get southern part of Portugal/Spain
 shapeEEZ  <- subset(shapeEEZ, UNION %in% c("Spain","France","Portugal",'Ireland',
                                            "United Kingdom","Norway","Denmark","Sweden",
                                            "Germany","Iceland","Belgium","Greenland","Azores","Netherlands"))

 
#-------------------------------------------------------------------------------------
# get 400-800 meter depths
# Notes:
# now split in two - the already defined EUVME 400-800 m depth
# and the other area - EMODNET depth has been updated - need to decide
# which depth to use in the update of the advice
 
 # EUVME depth based on EmodNet 2018
 load(paste(pathdir,"1-Input data/Region_depth_EUVME.RData",sep="/"))
 IREG <- subset(depth,!(depth$min_depth_emodnet > 800))
 IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 400)) 
 IREG$within <- 1  # if TRUE
 depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
 colnames(depth)[ncol(depth)] <- "within"
 depth$within[is.na(depth$within)] <- 0 # if not TRUE
 depth <- cbind(depth,bargrid@data[match(depth$csquares,bargrid@data$csquares),c(2,3)])
 depth_EUVME <- depth
 
 #  depth based on EmodNet 2020 (for areas without Emodnet coverage GEBCO is used)
 load(paste(pathdir,"1-Input data/Region_depth_prelim.RData",sep="/"))
 IREG <- subset(depth,!(depth$min_depth_emodnet > 800))
 IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 400)) 
 IREG$within <- 1  # if TRUE
 depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
 colnames(depth)[ncol(depth)] <- "within"
 depth$within[is.na(depth$within)] <- 0 # if not TRUE
 depth <- cbind(depth,bargrid@data[match(depth$csquares,bargrid@data$csquares),c(2,3)])
 
 depth <- subset(depth, !(depth$Ecoregion %in% c("Celtic Seas","Greater North Sea",
                                                 "Bay of Biscay and the Iberian Coast")))
 # combined depth 
 depth <- rbind(depth,depth_EUVME)
 
 # create polygon of 400-800m depths 
 Reg_w <- subset(bargrid, bargrid@data$csquares %in% depth$csquares)
 Reg_w <- cbind(Reg_w,depth[match(Reg_w@data$csquares,depth$csquares),c("within")])
 colnames(Reg_w@data)[ncol(Reg_w@data)] <- "within"
 Reg_w <- subset(Reg_w,Reg_w@data$within == 1)
 Reg_w <- raster::aggregate(Reg_w)
 Reg_w <- gUnaryUnion(Reg_w)
 Reg_w   <- st_as_sf(Reg_w)
 Reg_depth <-  st_transform(Reg_w, "EPSG:4326")  
 
 rm(Reg_w,bargrid,IREG,depth,i)
 
# save.image(file = paste(pathdir,"2-Data processing/Map_layer_workspace.RData",sep="/"))     
 