## before: table of SAR values by year for each c-square, presence/absence for static gears, and wkt descriptors

## after: a spatial polygon of the area in which either mobile or static fishing takes place 
##        during a specified reference 

## files used    file.loc - All_VMS_datacall2022.rds
##              bathy.loc - Depthrange_shapefiles.shp (as per benchmark)
##            spatial.loc - ICES_Ecoregions_EEZext_Bob_CS_AZ_diss_noUK_single_export.shp


#file.loc = "C:/Work/VME-Advice_noGIT/EU/VMS data repository/All_VMS_datacall2022.rds"
#  bathy.loc = "C:/Work/Depth range/Depth range/Depthrange_shapefiles.shp"
#  spatial.loc = "C:/Work/VME-Advice/1-Input data/eco_bathymetry_v2/ICES_Ecoregions_EEZext_Bob_CS_AZ_diss_noUK_single_export.shp"
#  all.range = 2009:2011
#  fishing.type = "mobile"

process_fishing_area <- function(file.loc, bathy.loc, spatial.loc, fishing.type, all.range){
  
  if(missing(fishing.type)){
    print("Error: Fishing type not specified")
    break
  }

  if(fishing.type %in% c("static", "mobile", "all")==FALSE){
    print("Error: Please specify either 'static' or 'mobile' fishing")
    break
  }
    
  if(missing(all.range)){
    print("Error: Year range not specified. Pick some years and try again")
    break
  }

  ## read data
             vms.all <- readRDS(file.loc)
          ecoregions <- read_sf(spatial.loc)
  clipped.bathymetry <- read_sf(bathy.loc)
  
  if(fishing.type == "mobile"){
      avg.columns <- paste("SAR_total_", all.range, sep="")
  }
  
  if(fishing.type == "static"){
    avg.columns <- paste("Static_", all.range, sep="")
  }
  
  if(fishing.type == "all"){
    avg.columns <- c(paste("Static_", all.range, sep=""), paste("SAR_total_", all.range, sep=""))
  }
  
  vms.all$activity <- rowSums(vms.all[,colnames(vms.all) %in% avg.columns], na.rm=T)>0
  
  out.cols <- c("c.square", "c_square", "activity", "wkt")
  
  vms.out <- vms.all[vms.all$activity == 1,colnames(vms.all) %in% out.cols]
  
  vms.out <- st_as_sf(vms.out)
  
  ## clip to 400 - 800m depths 
  vms.join <- st_join(vms.out, clipped.bathymetry, left=FALSE, largest = T)
  
  vms.out <- filter(vms.join, within == 1)
  
  vms.out <- select(vms.out, -within)
  
  ## clip to EU member state EEZs parts of ecoregions
  
  vms.join <- st_join(vms.out, ecoregions, left = FALSE, largest = T)
  
  vms.out <- select(vms.join, -c(Eco_EEZ, ORIG_FID, Shape_Leng, Shape_Area))
  
  vms.out   <- st_transform(vms.out, "EPSG:4326")
  
  return(vms.out)
  
}



