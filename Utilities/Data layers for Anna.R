
### obtain data layers for Anna

#### for now, these two lines need to be changed for each person running the code #####
  pathdir <- "C:/Users/danie/Documents/Online for git/VME-advice"
  pathdir_nogit <- "C:/Users/danie/Documents/Online for git/VME-advice_noGIT" # stores the VMS and VME data from sharepoint
#############################################################################

## install libraries
  source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  

# load ICES area
  load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  

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
  
# get spatialpolygondataframe for each VME index and habitat 
  VME_habitat <- subset(VMEgrid,VMEgrid@data$VME_Class == 3)
  VME_high    <- subset(VMEgrid,VMEgrid@data$VME_Class == 2)
  VME_medium  <- subset(VMEgrid,VMEgrid@data$VME_Class == 1)
  VME_low     <- subset(VMEgrid,VMEgrid@data$VME_Class == 0)
  
# get scenario 1 as sp/sf object
  closedir    <-  paste(pathdir,"2-Data processing",sep="/")
  scedat      <-  c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
  sce         <-  1
  scen1       <-  st_read(paste(closedir,paste(scedat[sce],"shp",sep="."),sep="/"))
  scen1_sp    <-  as_Spatial(scen1)
  scen1_sp@data$area_sqkm <- area(scen1_sp) / 1000000   # get surface area of each closure (if needed)
  
# ICES ecoregions
  shapeEcReg  <- st_read(paste(pathdir,"1-Input data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp",sep="/"))

  # VME advice region
  shapeEcReg    <- subset(shapeEcReg, Ecoregion %in% c("Bay of Biscay and the Iberian Coast","Celtic Seas",
                                                  "Greater North Sea", "Azores","Faroes",
                                                  "Greenland Sea","Arctic Ocean","Icelandic Waters",
                                                  "Barents Sea","Norwegian Sea","Oceanic Northeast Atlantic" ))
  
    
