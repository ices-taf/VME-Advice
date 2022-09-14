#
# load current footprints of EU and NEAFC
#

# the EU footprint is based on ICES 2021 (EUVME workshop and advice) 
# it should be updated once the EU has established a footprint 

# load EU footprint 
  EUFootp       <- st_read(paste(pathdir,"1-Input data/EU_fishingareas/Footprint_all.shp",sep="/"))
  EUFootp       <- st_make_valid(EUFootp)

  #EUFootp_mob   <- st_read(paste(pathdir,"1-Input data/EU_fishingareas/Footprint_mobile.shp",sep="/"))
  #EUFootp_mob   <- st_make_valid(EUFootp_mob)
  
  #EUFootp_stat  <- st_read(paste(pathdir,"1-Input data/EU_fishingareas/Footprint_static.shp",sep="/"))
  #EUFootp_stat  <- st_make_valid(EUFootp_stat)

  st_crs(EUFootp)      = 4326
  #st_crs(EUFootp_mob)  = 4326
  #st_crs(EUFootp_stat) = 4326
  
# load NEAFC areas
  tt      <- read.csv(paste(pathdir,"1-Input data/NEAFC_fishingareas/Fishingareas_coords.csv",sep="/"),
                 header=T)
  areaNam <- unique(tt$Area)
  
  area_sub <- subset(tt,tt$Area == areaNam[1])
  p        <- Polygon(cbind(area_sub$Longitude,area_sub$Latitude))
  ps       <- Polygons(list(p),1)
  sps      <- SpatialPolygons(list(ps))          
  sps      <- st_as_sf(sps)
  sps$ID   <- areaNam[1]
  Footp_N  <- sps 
  
  for (i in 2:length(areaNam)){
   area_sub <- subset(tt,tt$Area == areaNam[i])
   p        <- Polygon(cbind(area_sub$Longitude,area_sub$Latitude))
   ps       <- Polygons(list(p),1)
   sps      <- SpatialPolygons(list(ps))          
   sps      <- st_as_sf(sps)
   sps$ID   <- areaNam[i]
   Footp_N  <- rbind(Footp_N,sps)      
  }
  
  NEAFCFootp   <- st_as_sf(Footp_N)
  st_crs(NEAFCFootp) = 4326
  
  # and clean
  rm(areaNam,area_sub,p,ps,sps,tt,i,Footp_N)
  
      