## Constrain VME observations to c-squares within the depth and ecoregion layers

#before: VME database extract table (produced by Carlos, 28 Nov. 22), 
#         bathymetry polygon and ecoregion shapefiles

#after: Table of VME indicator records, at c-square level, clipped to the depth and ecoregion boundaries

#### --- Base data for database summaries in pop-ups and bio tab table in compilation Markdowns -------- ####

## read in ecoregion and depth range shapefiles

## file locations where these are saved - this bit needs some prettifying, but does the job for now
bat.file <- "C:/Work/Depth range/Depth range/Depthrange_shapefiles.shp"
spat.file <- "C:/Work/VME-Advice/1-Input data/eco_bathymetry_v2/ICES_Ecoregions_EEZext_Bob_CS_AZ_diss_noUK_single_export.shp"

        ecoregions <- read_sf(spat.file)
clipped.bathymetry <- read_sf(bat.file)


# Current lists of VME indicators and VME habitats in Annex III of EU deepwater access regulations
VMEindic <- c('Black coral','Cup coral','Gorgonian','Soft coral','Sponge','Sea-pen','Stylasterids','Stony coral')
 VMEhabs <- c('Bryozoan patches','Cold-water coral reef','Coral garden','Deep-sea sponge aggregations','Mud and sand emergent fauna','Sea-pen fields','Tube-dwelling anemone aggregations')



# Set up table with all indicators and habitats
bioTblBase <- rbind(data.frame(VMEclass='VME Habitat',Name=VMEhabs),
                    data.frame(VMEclass='VME Indicators',Name=VMEindic))

# load the VME database extraction
vmedb <- read.csv(paste(pathdir_nogit,paste(
  "VME data repository/VME observations and csquares/VME_observations_datacall_",
  datacallyear,"_eu.csv",sep=""),sep="/"), header=T,sep=",",row.names = NULL)

vmedb <- vmedb[,-1]

#colnames(vmedb)[1] <- "Sample"

source(paste(pathdir,"Utilities/coords_to_csquare_VMStools.R",sep="/"))

vmedb$CSquare <- CSquare(vmedb$MiddleLongitude,vmedb$MiddleLatitude,0.05)  

# Point datalayer of vme database records for the named list of VME indicators and habitats
vmedb_sf <- vmedb %>%
  select(VME_Indicator, HabitatType,InsDateTime,MiddleLatitude,MiddleLongitude, CSquare) %>%
  filter(VME_Indicator %in% VMEindic | HabitatType %in% VMEhabs) %>%
  mutate(InputYear=as.numeric(substr(InsDateTime,1,4))) %>%
  st_as_sf(coords=c('MiddleLongitude','MiddleLatitude')) 

st_crs(vmedb_sf) <- "EPSG:4326"

vmedb_sf <- st_join(vmedb_sf, ecoregions, left = FALSE, largest = T)  ## clips vms to ecoregion

vmedb_sf <- st_join(vmedb_sf, clipped.bathymetry, left=FALSE)         ## and bathymetry

vmedb_sf <- select(vmedb_sf, c(VME_Indicator, HabitatType, InsDateTime, ## remove other columns 
                               CSquare, InputYear, Eco_EEZ, geometry))

write_sf(vmedb_sf, "C:/GIS/Data/vme_database_clipped.shp")
