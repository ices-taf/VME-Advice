###  a wrapper function to process the VMS data and produce static, mobile and combined
###  fishery areas for a reference period (2009 - 2011) and a recent period (2018 - 2021)
###  as well as the average SAR value over 2009 - 2021, all clipped to the ecoregion and
###  depth layer (400 - 800m) used in the benchmark

library(sp)
library(dplyr)


## these need updating in a more sexy way
 vms.file <- "C:/Work/VME-Advice_noGIT/EU/VMS data repository/All_VMS_datacall2022.rds" 
 bat.file <- "C:/Work/Depth range/Depth range/Depthrange_shapefiles.shp"
spat.file <- "C:/Work/VME-Advice/1-Input data/eco_bathymetry_v2/ICES_Ecoregions_EEZext_Bob_CS_AZ_diss_noUK_single_export.shp"


## applies the SAR function to the whole data range, and writes it out
vms.sar <- process_vms_sar(file.loc = vms.file, 
                           bathy.loc = bat.file,
                           spatial.loc = spat.file,
                           all.range = 2009:2021)

write_sf(vms.sar, "C:/GIS/VME_Assessment_SAR.shp")


## calculates the fished area for mobile gears in the reference period
vms.mob.ref <- process_fishing_area(file.loc = vms.file, 
                                    bathy.loc = bat.file,
                                    spatial.loc = spat.file,
                                    all.range = 2009:2011, fishing.type = "mobile")


write_sf(vms.mob.ref, "C:/GIS/VME_Mobile_Reference_Area.shp")


##  and for the new period
vms.mob.new <- process_fishing_area(file.loc = vms.file, 
                                    bathy.loc = bat.file,
                                    spatial.loc = spat.file,
                                    all.range = 2018:2021, fishing.type = "mobile")


write_sf(vms.mob.new, "C:/GIS/VME_Mobile_New_Area.shp")



## static gears
vms.stat.ref <- process_fishing_area(file.loc = vms.file, 
                                    bathy.loc = bat.file,
                                    spatial.loc = spat.file,
                                    all.range = 2009:2011, fishing.type = "static")


write_sf(vms.mob.ref, "C:/GIS/VME_Static_Reference_Area.shp")


vms.stat.new <- process_fishing_area(file.loc = vms.file, 
                                    bathy.loc = bat.file,
                                    spatial.loc = spat.file,
                                    all.range = 2018:2021, fishing.type = "static")


write_sf(vms.mob.new, "C:/GIS/VME_Static_New_Area.shp")



## all gears
vms.all.ref <- process_fishing_area(file.loc = vms.file, 
                                     bathy.loc = bat.file,
                                     spatial.loc = spat.file,
                                     all.range = 2009:2011, fishing.type = "all")


write_sf(vms.mob.ref, "C:/GIS/VME_All_Gears_Reference_Area.shp")


vms.all.new <- process_fishing_area(file.loc = vms.file, 
                                     bathy.loc = bat.file,
                                     spatial.loc = spat.file,
                                     all.range = 2018:2021, fishing.type = "all")


write_sf(vms.mob.new, "C:/GIS/VME_All_Gears_New_Area.shp")
