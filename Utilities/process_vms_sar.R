## before: table of SAR values by year for each c-square, and wkt descriptors

## after: average sar, and sar values by year for the reference and recent period,
#         only for mobile bottom contacting gears clipped to the 400 - 800m depth range


## files used    file.loc - All_VMS_datacall2022.rds
##              bathy.loc - Depthrange_shapefiles.shp (as per benchmark)
##            spatial.loc - ICES_Ecoregions_EEZext_Bob_CS_AZ_diss_noUK_single_export.shp

process_vms_sar <- function(file.loc, bathy.loc, spatial.loc, all.range){
  
  if(missing(all.range)){
    all.range <- 2009:2021
    print("Year range not specified. Averaging SAR over 2009 - 2021")
  }
  
  ## read data
             vms.all <- readRDS(file.loc)
          ecoregions <- read_sf(spatial.loc)
  clipped.bathymetry <- read_sf(bathy.loc)
  
         avg.columns <- paste("SAR_total_", all.range, sep="")
  
         vms.all$SAR <- rowSums(vms.all[,colnames(vms.all) %in% avg.columns], na.rm=T)/sum(colnames(vms.all) %in% avg.columns)
  
  if(sum(colnames(vms.all) %in% avg.columns) != length(all.range)){
    print("Warning: Years in VMS data and range specified for average SAR calculation do not match.")
  }
  
  out.cols <- c("c.square", "c_square", paste("SAR_total_", all.range, sep=""), "SAR", "wkt")
 
  vms.out <- vms.all[,colnames(vms.all) %in% out.cols]
  
  vms.out <- st_as_sf(vms.out)
 
  ## clip to 400 - 800m depths 
  vms.join <- st_join(vms.out, clipped.bathymetry, left=FALSE, largest = T) ## joins VMS to bathymetry range
  
  vms.out <- filter(vms.join, within == 1) ## removes cells outside the depth band
  
  vms.out <- select(vms.out, -within) ## drops this field
  
  ## clip to EU member state EEZs parts of ecoregions
  
  vms.join <- st_join(vms.out, ecoregions, left = FALSE, largest = T)  ## clips vms to ecoregion
  
  vms.out <- select(vms.join, c(c.square, SAR, wkt))  ## remove unneccesary fields
  
  vms.out <- filter(vms.out, SAR > 0)  ## removes cells which are only present because of static gears
  
  return(vms.out)
  
}
  
