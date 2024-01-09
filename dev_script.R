#dev_script


#-------------------------------------------------------------------------------------
# step 1 - assessment year, rlibraries and folder structure
#------------------------------------------------------------------------------------- 

datacallyear     <- 2022   # VME data call year with latest data
datacallyear_VMS <- 2022   # VMS data call year with latest data

# set path to the VME advice folder
pathdir <- "C:/Users/neilm/Documents/projects/VME-Advice/"

# set path to the folder with VMS and VME data (restricted data - outside github)
pathdir_nogit <- "C:/Users/neilm/Documents/VME-advice_noGIT/EU" 

# R libraries used
#source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  
source("data_utilities.R")  



#------------ load data +++++++++#

#VME INDEX

load(paste(pathdir,"1-Input data/Region_csquare_grid.RData",sep="/"))  

VME <- read.csv(paste(pathdir_nogit,paste(
  "VME data repository/VME observations and csquares/VME_csquares_datacall_",
  datacallyear,"_eu.csv",sep=""),sep="/"),header=T,sep=",",row.names = NULL)
VME <- as.data.frame(VME)
#VME <- VME[,-1]

# create VME spatial grid
VMEgrid       <- subset(bargrid,bargrid@data$csquares %in% unique(VME$CSquare))
VMEgrid       <- cbind(VMEgrid, VME[match(VMEgrid@data$csquares,VME$CSquare), c("VME_Class")])
colnames(VMEgrid@data)[ncol(VMEgrid)] <- "VME_Class"
VMEgrid       <- subset(VMEgrid,!(is.na(VMEgrid@data$VME_Class)))

VMEgrid <- st_as_sf(VMEgrid)


#VME observations
VMEobs <- read.csv(paste(pathdir_nogit,paste(
  "VME data repository/VME observations and csquares/VME_observations_datacall_",
  datacallyear,"_eu.csv",sep=""),sep="/"),
  header=T,sep=",",row.names = NULL)

# get all in ICES area
VMEobs <- subset(VMEobs,VMEobs$StartLongitude > -20)

# get all presences
VMEobs <- subset(VMEobs,!(VMEobs$VME_Indicator =="NULL" & VMEobs$HabitatType =="NULL")) 

# create polypoints based on the middle lat and long
data <- data.frame(VMEid = VMEobs[,2],
                   VME_longitude = VMEobs$MiddleLongitude,
                   VME_latitude  = VMEobs$MiddleLatitude,
                   stringsAsFactors = F)

coordinates(data) <- c("VME_longitude","VME_latitude")
data@data$VME_longitude <- VMEobs$MiddleLongitude
data@data$VME_latitude <- VMEobs$MiddleLatitude
VMEobs_points <- data
VMEobs_points <- st_as_sf(VMEobs_points)


#VME elements
Elements <- st_read(paste(pathdir_nogit, "VME data repository/VME elements/VME_elements.gpkg", sep="/")) %>%
  st_make_valid()

# change VME observations to same projection
Proj     <- as(Elements[1,], 'Spatial')
VMEobs_points_new_proj <- st_set_crs(VMEobs_points, CRS(proj4string(Proj)))  
VMEobs_poly   <- VMEobs_points_new_proj

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
vme_elements <- st_as_sf(element_close)



# VMS
vmsreg <- readRDS(paste(pathdir_nogit,paste("VMS data repository/All_VMS_datacall",datacallyear_VMS,".rds",sep=""),sep="/"))  
refyear       <- 2009:(datacallyear-1)   # specify years to estimate sar threshold
SAR_threshold <- 0.43                    # SAR threshold value
nam <- c(paste("SAR_total",refyear,sep="_"))
indexcol <- which(names(vmsreg) %in% nam) 
vmsreg$SAR <- rowMeans(vmsreg[indexcol],na.rm=T)


#Scenario 1.1 / A
vme_scenario_csquares(vme_index = VMEgrid, 
                      vme_observations = VMEobs_points,
                      vme_elements = vme_elements, 
                      scenario = "A" 
                        )
#Scenario 1.2 / B
vme_scenario_csquares(vme_index = VMEgrid, 
                      vme_observations = VMEobs_points,
                      vme_elements = vme_elements, 
                      scenario = "B" 
                        )

#Scenario 2.1 / C
vme_scenario_csquares(vme_index = VMEgrid, 
                      vme_observations = VMEobs_points,
                      vme_elements = vme_elements, 
                      scenario = "C" 
                        )
#Scenario 2.2 / D
vme_scenario_csquares(vme_index = VMEgrid, 
                      vme_observations = VMEobs_points,
                      vme_elements = vme_elements, 
                      sar_layer = vmsreg, 
                      sar.threshold = SAR_threshold,
                      scenario = "D" 
                        )
#Scenario 2.3 / E
vme_scenario_csquares(vme_index = VMEgrid, 
                      vme_observations = VMEobs_points,
                      vme_elements = vme_elements, 
                      sar_layer = vmsreg, 
                      sar.threshold = SAR_threshold,
                      scenario = "E" 
                        )



#### 
VMEobs <- read.csv(paste(pathdir_nogit,paste(
"VME data repository/VME observations and csquares/VME_observations_datacall_",
datacallyear,"_eu.csv",sep=""),sep="/"),
header=T,sep=",",row.names = NULL)
