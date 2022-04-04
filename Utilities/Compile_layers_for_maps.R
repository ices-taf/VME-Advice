
##############################################################
########## script to obtain all spatial data layers ##########
##############################################################

# set path to folder VME advice
 pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"

# set path to folder with VMS and VME data from sharepoint
 pathdir_nogit <- "C:/Users/danie/Documents/Online for git/VME-advice_noGIT" 

# install libraries
 source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  
 
# get data call year
 datacallyear      <- 2021

# load VME closures
 scen11 <- st_read(paste(pathdir,"2-Data processing/Scenario1_option1.shp",sep="/"))
 scen12 <- st_read(paste(pathdir,"2-Data processing/Scenario1_option2.shp",sep="/"))
 scen21 <- st_read(paste(pathdir,"2-Data processing/Scenario2_option1.shp",sep="/"))
 scen22 <- st_read(paste(pathdir,"2-Data processing/Scenario2_option2.shp",sep="/"))
     
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
     
# clean and save workspace
 rm(list=setdiff(ls(), c("scen11" , "scen12","scen21","scen22","Elements",
                         "pathdir","pathdir_nogit","datacallyear")))
 
# load closure workspace
 load(paste(pathdir,"2-Data processing/Footprint_workspace.RData",sep="/"))
 
# load current NEAFC closures
 source(paste(pathdir,"Utilities/Obtain_NEAFC_closures.R",sep="/"))
 
# load current EU closures Scenario 2 option 1
 clos_EU <- st_read(paste(pathdir,"1-Input data/EU_closures/EU_ICESS2O1.shp",sep="/"))
 
# load existing VMEs datacallyear - 1
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
 
 
 # create VME spatial grid
 #load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  
 #VMEgrid       <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
 #VMEgrid       <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
 #colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
 #VMEgrid       <- subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class)))
 #VME_habitat <- subset(VMEgrid,VMEgrid@data$VME_Class == 3)
 #VME_high    <- subset(VMEgrid,VMEgrid@data$VME_Class == 2)
 #VME_medium  <- subset(VMEgrid,VMEgrid@data$VME_Class == 1)
 #VME_low     <- subset(VMEgrid,VMEgrid@data$VME_Class == 0)
 
# load VMEs in datacallyear 
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
 
 # create VME spatial grid
 #load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  
 #VMEgrid         <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
 #VMEgrid         <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
 #colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
 #VMEgrid         <- subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class)))
 #VME_habitat_new <- subset(VMEgrid,VMEgrid@data$VME_Class == 3)
 #VME_high_new    <- subset(VMEgrid,VMEgrid@data$VME_Class == 2)
 #VME_medium_new  <- subset(VMEgrid,VMEgrid@data$VME_Class == 1)
 #VME_low_new     <- subset(VMEgrid,VMEgrid@data$VME_Class == 0)
 
 rm("VMEgrid","VME")
 
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
                                            "Germany","Iceland","Belgium","Greenland","Azores"))

# get 400-800 meter depths
 load(paste(pathdir,"1-Input data/Region_depth_prelim.RData",sep="/"))
 IREG <- subset(depth,!(depth$min_depth_emodnet > 800))
 IREG <- subset(IREG, !(IREG$max_depth_emodnet  < 400)) 
 IREG$within <- 1  # if TRUE
 depth <- cbind(depth,IREG[match(depth$csquare,IREG$csquare),c("within")])
 colnames(depth)[ncol(depth)] <- "within"
 depth$within[is.na(depth$within)] <- 0 # if not TRUE
 
 # get region within 400-800 meter
 Reg_w <- subset(bargrid, bargrid@data$csquares %in% depth$csquares)
 Reg_w <- cbind(Reg_w,depth[match(Reg_w@data$csquares,depth$csquares),c("within")])
 colnames(Reg_w@data)[ncol(Reg_w@data)] <- "within"
 Reg_w <- subset(Reg_w,Reg_w@data$within == 1)
 Freg <- unionSpatialPolygons(Reg_w,Reg_w$within)
 Reg_w <- gUnaryUnion(Freg)
 Reg_w   <- st_as_sf(Reg_w)
 Reg_depth <-  st_transform(Reg_w, "EPSG:4326")  
 
 rm("Reg_w","Freg","bargrid","IREG","depth","datacallyear","i")
 
 save.image(file = paste(pathdir,"2-Data processing/Map_layer_workspace.RData",sep="/"))     
 