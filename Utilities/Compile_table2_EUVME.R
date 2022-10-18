################################################
#### code to compile Table 2  for EUVME ##
################################################

# get table 1
  Regiontab1 <- subset(table1, table1$EEZ %in% Region_ID)
  
# get fishable domain in the region
  Fishdom <- s_EUFootp

# get fished area in the region
  Mobile_fish <- st_intersection(s_New_mobile,s_reg) ; Mobile_fish <- st_make_valid(Mobile_fish)
  Static_fish <- st_intersection(s_New_static,s_reg) ; Static_fish <- st_make_valid(Static_fish)

# get fishing SAR in the region
  vmsreg          <- readRDS(paste(pathdir_nogit,paste("VMS data repository/All_VMS_datacall",datacallyear_VMS,".rds",sep=""),sep="/"))  
  nam_fished      <- c(paste("SAR_total",newyear_fished,sep="_"))
  indexcol_fished <- which(names(vmsreg) %in% nam_fished) 
  vmsreg$mobeff   <- rowMeans(vmsreg[indexcol_fished],na.rm=T)
  vmsreg$mobeff[is.na(vmsreg$mobeff)] <- 0
  Regiontab1 <- cbind(Regiontab1, vmsreg[match(Regiontab1$csquares,vmsreg$c_square), c("mobeff")])
  colnames(Regiontab1)[ncol(Regiontab1)] <- "mobeff"
  Regiontab1 <- subset(Regiontab1,Regiontab1$cat =="de4_8")
  
# make the table
  tab2 <- as.data.frame(matrix(data=NA,nrow = 13, ncol= 6))

# calculate overlap with fishable domain
  FD_scen11 <- st_intersection(s_scen11,Fishdom); FD_scen11 <- st_make_valid(FD_scen11)
  tab2[4,2] <- round(sum(st_area(FD_scen11))/sum(st_area(Fishdom)) * 100,digits = 1)
  
  FD_scen12 <- st_intersection(s_scen12,Fishdom); FD_scen12 <- st_make_valid(FD_scen12)
  tab2[4,3] <- round(sum(st_area(FD_scen12))/sum(st_area(Fishdom)) * 100,digits = 1)
  
  FD_scen21 <- st_intersection(s_scen21,Fishdom); FD_scen21 <- st_make_valid(FD_scen21)
  tab2[4,4] <- round(sum(st_area(FD_scen21))/sum(st_area(Fishdom)) * 100,digits = 1)
  
  FD_scen22 <- st_intersection(s_scen22,Fishdom); FD_scen22 <- st_make_valid(FD_scen22)
  tab2[4,5] <- round(sum(st_area(FD_scen22))/sum(st_area(Fishdom)) * 100,digits = 1)
  
  FD_scen23 <- st_intersection(s_scen23,Fishdom); FD_scen23 <- st_make_valid(FD_scen23)
  tab2[4,6] <- round(sum(st_area(FD_scen23))/sum(st_area(Fishdom)) * 100,digits = 1)

# Number of VME polygons and their average areal extent (size)
  tab2[6,2] <- paste(nrow(s_scen11)," (",round(mean(st_area(s_scen11)/10^6),digits=1)," km<sup>2</sup>)",sep="")
  tab2[6,3] <- paste(nrow(s_scen12)," (",round(mean(st_area(s_scen12)/10^6),digits=1)," km<sup>2</sup>)",sep="")
  tab2[6,4] <- paste(nrow(s_scen21)," (",round(mean(st_area(s_scen21)/10^6),digits=1)," km<sup>2</sup>)",sep="")
  tab2[6,5] <- paste(nrow(s_scen22)," (",round(mean(st_area(s_scen22)/10^6),digits=1)," km<sup>2</sup>)",sep="")
  tab2[6,6] <- paste(nrow(s_scen23)," (",round(mean(st_area(s_scen23)/10^6),digits=1)," km<sup>2</sup>)",sep="")

# Number of large VME polygons in upper 25th percentile of the size distribution nd their average areal extent (size)
  sc11areas <- sort(st_area(s_scen11)/10^6)
  sc12areas <- sort(st_area(s_scen12)/10^6)
  sc21areas <- sort(st_area(s_scen21)/10^6)
  sc22areas <- sort(st_area(s_scen22)/10^6)
  sc23areas <- sort(st_area(s_scen23)/10^6)
  
  sc11p25 <- which(as.numeric(cumsum(sort(st_area(s_scen11)/10^6))/sum(st_area(s_scen11)/10^6))>0.75)
  sc12p25 <- which(as.numeric(cumsum(sort(st_area(s_scen12)/10^6))/sum(st_area(s_scen12)/10^6))>0.75)
  sc21p25 <- which(as.numeric(cumsum(sort(st_area(s_scen21)/10^6))/sum(st_area(s_scen21)/10^6))>0.75)
  sc22p25 <- which(as.numeric(cumsum(sort(st_area(s_scen22)/10^6))/sum(st_area(s_scen22)/10^6))>0.75)
  sc23p25 <- which(as.numeric(cumsum(sort(st_area(s_scen23)/10^6))/sum(st_area(s_scen23)/10^6))>0.75)

  tab2[7,2] <- paste(length(sc11p25)," (",round(mean(sc11areas[sc11p25]),digits=1)," km<sup>2</sup>)",sep="")
  tab2[7,3] <- paste(length(sc12p25)," (",round(mean(sc12areas[sc12p25]),digits=1)," km<sup>2</sup>)",sep="")
  tab2[7,4] <- paste(length(sc21p25)," (",round(mean(sc21areas[sc21p25]),digits=1)," km<sup>2</sup>)",sep="")
  tab2[7,5] <- paste(length(sc22p25)," (",round(mean(sc22areas[sc22p25]),digits=1)," km<sup>2</sup>)",sep="")
  tab2[7,6] <- paste(length(sc23p25)," (",round(mean(sc23areas[sc23p25]),digits=1)," km<sup>2</sup>)",sep="")

# overlap between static fishing effort and vme polygons
# not available

# overlap between static fishing area and vme polygons
  SF_scen11 <- st_intersection(s_scen11,Static_fish); SF_scen11 <- st_make_valid(SF_scen11)
  tab2[11,2] <- round(sum(st_area(SF_scen11))/sum(st_area(Static_fish)) * 100,digits = 1)
  
  SF_scen12 <- st_intersection(s_scen12,Static_fish); SF_scen12 <- st_make_valid(SF_scen12)
  tab2[11,3] <- round(sum(st_area(SF_scen12))/sum(st_area(Static_fish)) * 100,digits = 1)
  
  SF_scen21 <- st_intersection(s_scen21,Static_fish); SF_scen21 <- st_make_valid(SF_scen21)
  tab2[11,4] <- round(sum(st_area(SF_scen21))/sum(st_area(Static_fish)) * 100,digits = 1)
  
  SF_scen22 <- st_intersection(s_scen22,Static_fish); SF_scen22 <- st_make_valid(SF_scen22)
  tab2[11,5] <- round(sum(st_area(SF_scen22))/sum(st_area(Static_fish)) * 100,digits = 1)
  
  SF_scen23 <- st_intersection(s_scen23,Static_fish); SF_scen23 <- st_make_valid(SF_scen23)
  tab2[11,6] <- round(sum(st_area(SF_scen23))/sum(st_area(Static_fish)) * 100,digits = 1)

# overlap between mobile fishing effort and vme polygons
  load(paste(pathdir,paste("2-Data processing/VME_polygons",datacallyear,sep="_"),"sce11_quarter_csq_grid.RData",sep="/"))
  sce11 <- subset(sce11,sce11@data$csquares %in% Regiontab1$csquares)
  sce11@data$area_sqkm <- area(sce11)/10^6
  sce11 <- cbind(sce11, Regiontab1[match(sce11$csquares,Regiontab1$csquares), c("mobeff")])
  colnames(sce11@data)[ncol(sce11@data)] <- "mobeff"
  tab2[12,2] <- round(sum(sce11@data$area_sqkm * sce11@data$mobeff,na.rm=T) / sum(Regiontab1$area_sqkm * Regiontab1$mobeff,na.rm=T) * 100,digits =1)
  
  load(paste(pathdir,paste("2-Data processing/VME_polygons",datacallyear,sep="_"),"sce12_quarter_csq_grid.RData",sep="/"))
  sce12 <- subset(sce12,sce12@data$csquares %in% Regiontab1$csquares)
  sce12@data$area_sqkm <- area(sce12)/10^6
  sce12 <- cbind(sce12, Regiontab1[match(sce12$csquares,Regiontab1$csquares), c("mobeff")])
  colnames(sce12@data)[ncol(sce12@data)] <- "mobeff"
  tab2[12,3] <- round(sum(sce12@data$area_sqkm * sce12@data$mobeff,na.rm=T) / sum(Regiontab1$area_sqkm * Regiontab1$mobeff,na.rm=T) * 100,digits =1)
  
  load(paste(pathdir,paste("2-Data processing/VME_polygons",datacallyear,sep="_"),"sce21_quarter_csq_grid.RData",sep="/"))
  sce21 <- subset(sce21,sce21@data$csquares %in% Regiontab1$csquares)
  sce21@data$area_sqkm <- area(sce21)/10^6
  sce21 <- cbind(sce21, Regiontab1[match(sce21$csquares,Regiontab1$csquares), c("mobeff")])
  colnames(sce21@data)[ncol(sce21@data)] <- "mobeff"
  tab2[12,4] <- round(sum(sce21@data$area_sqkm * sce21@data$mobeff,na.rm=T) / sum(Regiontab1$area_sqkm * Regiontab1$mobeff,na.rm=T) * 100,digits =1)
  
  load(paste(pathdir,paste("2-Data processing/VME_polygons",datacallyear,sep="_"),"sce22_quarter_csq_grid.RData",sep="/"))
  sce22 <- subset(sce22,sce22@data$csquares %in% Regiontab1$csquares)
  sce22@data$area_sqkm <- area(sce22)/10^6
  sce22 <- cbind(sce22, Regiontab1[match(sce22$csquares,Regiontab1$csquares), c("mobeff")])
  colnames(sce22@data)[ncol(sce22@data)] <- "mobeff"
  tab2[12,5] <- round(sum(sce22@data$area_sqkm * sce22@data$mobeff,na.rm=T) / sum(Regiontab1$area_sqkm * Regiontab1$mobeff,na.rm=T) * 100,digits =1)
  
  load(paste(pathdir,paste("2-Data processing/VME_polygons",datacallyear,sep="_"),"sce23_quarter_csq_grid.RData",sep="/"))
  sce23 <- subset(sce23,sce23@data$csquares %in% Regiontab1$csquares)
  sce23@data$area_sqkm <- area(sce23)/10^6
  sce23 <- cbind(sce23, Regiontab1[match(sce23$csquares,Regiontab1$csquares), c("mobeff")])
  colnames(sce23@data)[ncol(sce23@data)] <- "mobeff"
  tab2[12,6] <- round(sum(sce23@data$area_sqkm * sce23@data$mobeff,na.rm=T) / sum(Regiontab1$area_sqkm * Regiontab1$mobeff,na.rm=T) * 100,digits =1)
  
# overlap between mobile fishing area and vme polygons
  SF_scen11 <- st_intersection(s_scen11,Mobile_fish); SF_scen11 <- st_make_valid(SF_scen11)
  tab2[13,2] <- round(sum(st_area(SF_scen11))/sum(st_area(Mobile_fish)) * 100,digits = 1)
  
  SF_scen12 <- st_intersection(s_scen12,Mobile_fish); SF_scen12 <- st_make_valid(SF_scen12)
  tab2[13,3] <- round(sum(st_area(SF_scen12))/sum(st_area(Mobile_fish)) * 100,digits = 1)
  
  SF_scen21 <- st_intersection(s_scen21,Mobile_fish); SF_scen21 <- st_make_valid(SF_scen21)
  tab2[13,4] <- round(sum(st_area(SF_scen21))/sum(st_area(Mobile_fish)) * 100,digits = 1)
  
  SF_scen22 <- st_intersection(s_scen22,Mobile_fish); SF_scen22 <- st_make_valid(SF_scen22)
  tab2[13,5] <- round(sum(st_area(SF_scen22))/sum(st_area(Mobile_fish)) * 100,digits = 1)
  
  SF_scen23 <- st_intersection(s_scen23,Mobile_fish); SF_scen23 <- st_make_valid(SF_scen23)
  tab2[13,6] <- round(sum(st_area(SF_scen23))/sum(st_area(Mobile_fish)) * 100,digits = 1)

tab2[,1] <- c("VME polygon description","","VME polygon outcomes","% of fishable domain identified as VME polygon",
              "% of VME polygon protected by existing VME fishery closures",
              "Number of VME polygons and their average areal extent (size)",
              "Number (and average size) of large VME polygons in upper 25<sup>th</sup> percentile of the size distribution",
              "Risk to VME*","Fishery consequences",
              "% of effort per year by static gear (400-800m depth) overlapping with VME polygons (average annual effort between 2018 to 2020)",
              "% of fished area (400-800m depth) by static gear overlapping with VME polygons between 2018 to 2020",
              "% of SAR  by mobile gear (400-800m depth) overlapping with VME polygons (average annual SAR between 2018 to 2020)",
              "% of fished area (400-800m depth) by mobile gear overlapping with VME polygons between 2018 to 2020"
              )

tab2[1,2:6] <- c("All VME habitats, High and Medium VME Index. Low VME Index: only if adjacent to medium or high Index VMEs.",
               "Option 1 + selected VME elements (banks, seamounts, coral mounds, mud volcanoes) associated with any VME records.",
               "All VME habitats, High and Medium VME Index. Low VME Index: only if adjacent to higher index VMEs and Low VME Index in C-squares with low fishing pressure",
               "All VME habitats, High, Medium and Low VME Index excluding C-squares with high fishing pressure (SAR > 0.43)",
               "Scenario 2, Option 1 + selected VME elements (banks, seamounts, coral mounds, mud volcanoes) associated with any VME records.")
               
colnames(tab2) <- c("","Scenario 1 Option 1","Scenario 1 Option 2","Scenario 2 Option 1","Scenario 2 Option 2",
                    "Scenario 1 Option 2 + Scenario 2 Option 1")
