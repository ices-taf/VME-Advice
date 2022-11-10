#-------------------------------------------------------------------------------------
# script to obtain all spatial data layers for rmarkdown
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
# load VME polygons from last year (datacallyear-2)
 dir_polprev  <- paste(pathdir,paste("2-Data processing/VME_polygons",datacallyear-2,sep="_"),sep="/")
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
 # Bank       <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Bank.shp",sep="/"))
 # Bank       <- st_make_valid(Bank)
 # Coralmound <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_CoralMounds.shp",sep="/"))
 # Coralmound <- st_make_valid(Coralmound)
 # Mudvolcano <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Mud_Volcano.shp",sep="/"))
 # Mudvolcano <- st_make_valid(Mudvolcano)
 # Seamount   <- st_read(paste(pathdir_nogit,"VME data repository/VME elements/EMODNET_Seamount.shp",sep="/"))
 # Seamount   <- st_make_valid(Seamount)
 # Elements   <- rbind(Bank,Coralmound,Mudvolcano,Seamount)
 # rm(Bank,Coralmound,Mudvolcano,Seamount)
 Elements <- st_read(paste(pathdir_nogit, "VME data repository/VME elements/VME_elements.gpkg", sep="/")) %>%
   st_make_valid()
 
 
#-------------------------------------------------------------------------------------
# load all available VMEs from previous year (datacallyear - 1) or previous assessment - could be datacallyear -2
 VME <- read.csv(paste(pathdir_nogit,paste(
                 "VME data repository/VME observations and csquares/VME_csquares_datacall_",
                                  datacallyear-2,"_eu.csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
 
 # VME <- read.csv(paste(pathdir_nogit,paste(
 #   "VME data repository/VME observations and csquares/VME_csquares_datacall_",
 #   datacallyear-2,".csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
 
 VME <- as.data.frame(VME)
 #VME <- VME[,-1]
 
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
   datacallyear,"_eu.csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
 VME <- as.data.frame(VME)
 #VME <- VME[,-1]
 
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

  
#-------------------------------------------------------------------------------------
# load VME database summary used for the popup tables in the rmarkdown
 #source(paste(pathdir,"Utilities/VME Database summary.R",sep="/"))  # (warnings are okay)
 source(paste(pathdir,"Utilities/VME Database summary EU VME List.R",sep="/"))  # (warnings are okay)
 
 
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

 # Combined ICES ecoregions and EEZs
 shape_ices_EEZ <- st_read(paste(pathdir,"1-Input data/eco_bathymetry_v2/ICES_Ecoregions_EEZext_Bob_CS_AZ_diss_noUK_single_export.shp",sep="/")) %>% 
   st_make_valid()
 
#-------------------------------------------------------------------------------------
# get 400-800 meter depths

 depth <- read.csv(paste(pathdir,"1-Input data/eco_bathymetry_v2/Extended_ICES_area_EMODNET_GEBCO_Combined.csv",sep="/"))

 depth <- depth %>% 
   mutate(within = case_when(Depth_min < 800 & Depth_max > 400 ~ 1, TRUE ~ 0)) %>% 
   left_join(bargrid@data, by=c("csquares")) %>% 
   select(-c(area_sqkm, long, lat))
 
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
 
