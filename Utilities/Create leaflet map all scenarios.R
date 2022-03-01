######
## make a leaflet map - for now quick and easy 
######

# load area
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

# get scenario as sp object
  closedir <- paste(pathdir,"2-Data processing",sep="/")
  scedat <- c("Scenario1_option1","Scenario1_option2","Scenario2_option1","Scenario2_option2")
  sce <- 1
  scen <- st_read(paste(closedir,paste(scedat[sce],"shp",sep="."),sep="/"))
  scen <- as_Spatial(scen)
  
# add VMEs
  VME_habitat <- subset(VMEgrid,VMEgrid@data$VME_Class == 3)
  VME_high    <- subset(VMEgrid,VMEgrid@data$VME_Class == 2)
  VME_medium  <- subset(VMEgrid,VMEgrid@data$VME_Class == 1)
  VME_low     <- subset(VMEgrid,VMEgrid@data$VME_Class == 0)

# create polygon of the area
  
  # load current NEAFC closures
  source(paste(pathdir,"Utilities/Obtain_NEAFC_closures.R",sep="/"))
  
  # EUVME area of interest --> NS, CS, BoBIC, Azores, Oceanic NE Atlantic
  shapeEcReg <- st_read(paste(pathdir,"1-Input data/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp",sep="/"))
  
  # NEAFC region
  NEAFCReg <- st_read(paste(pathdir,"1-Input data/NEAFC_regions.gpkg",sep="/"))
  
  # EUVME region
  EUVME    <- subset(shapeEcReg, Ecoregion %in% c("Bay of Biscay and the Iberian Coast","Celtic Seas",
                                                    "Greater North Sea", "Azores","Faroes",
                                                  "Greenland Sea","Arctic Ocean","Icelandic Waters",
                                                  "Barents Sea","Norwegian Sea","Oceanic Northeast Atlantic" ))
  EUVME   <- st_make_valid(EUVME)
  #EUVME <- st_union(EUVME)
  shapeEEZ  <- st_read(paste(pathdir,"1-Input data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp",sep="/"))  # get southern part of Portugal/Spain
  EEZtip    <- subset(shapeEEZ,UNION %in% c("Portugal","Spain")) 
  subMed    <- subset(shapeEcReg, Ecoregion %in% c("Western Mediterranean Sea"))   # remove Med Sea part of EEZs
  EEZtip    <- st_difference(EEZtip,st_make_valid(subMed))
  #EUVME   <- st_union(EUVME, st_union(EEZtip))  # combine the areas
  
mfs <- leaflet() %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(data = scen, group = "Sce1-O1",
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,fillColor =  "white") %>%
  addPolygons(data = VME_habitat, group = "VME habitat",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#2E8AC6") %>%
  addPolygons(data = VME_high, group = "VME index - high",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =  "#F40000") %>%
  addPolygons(data = VME_medium, group = "VME index - medium",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "#F67E11") %>%
  addPolygons(data = VME_low, group = "VME index - low",
              stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,fillColor =   "#FDF100") %>%
  
  addPolygons(data = EUVME, group = "ICES Atlantic Ecoregions",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  addPolygons(data = EEZtip, group = "southern tip",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  addPolygons(data = NEAFCReg, group = "NEAFC areas",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "red") %>%
  addPolygons(data = clos_neafc, group = "NEAFC closures",
              stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
  
  # Layers control
  addLayersControl(
    overlayGroups = c("Sce1-O1",
                      "VME habitat","VME index - high",
                      "VME index - medium","VME index - low","ICES Atlantic Ecoregions","southern tip",
                      "NEAFC areas","NEAFC closures"),
    options = layersControlOptions(collapsed = FALSE)
  )

# save output
outdir <- paste(pathdir,"Output",sep="/") 
setwd(outdir)
saveWidget(mfs, file="Scenario1_option1.html")
