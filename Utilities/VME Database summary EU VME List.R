#############################################################################################################
#########     Summarise database contents by c-square and VME Polygon       #################################
#########     AD  24/10/2022                                                #################################
#############################################################################################################


#### --- Base data for database summaries in pop-ups and bio tab table in compilation Markdowns -------- ####


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
#colnames(vmedb)[1] <- "Sample"

source(paste(pathdir,"Utilities/coords_to_csquare_VMStools.R",sep="/"))

vmedb$CSquare <- CSquare(vmedb$MiddleLongitude,vmedb$MiddleLatitude,0.05)  

# Point datalayer of vme database records for the named list of VME indicators and habitats
vmedb_sf <- vmedb %>%
              select(Sample:HabitatType,InsDateTime,MiddleLatitude,MiddleLongitude) %>%
              filter(VME_Indicator %in% VMEindic | HabitatType %in% VMEhabs) %>%
              mutate(InputYear=as.numeric(substr(InsDateTime,1,4))) %>%
              st_as_sf(coords=c('MiddleLongitude','MiddleLatitude')) 

st_crs(vmedb_sf) <- st_crs(VMEgrid_new)
vmedb_sf <- vmedb_sf %>% 
              mutate(scen11ol=case_when(lengths(st_intersects(vmedb_sf, scen11)) > 0 ~ 1,
                                        TRUE ~0),
                     scen12ol=case_when(lengths(st_intersects(vmedb_sf, scen12)) > 0 ~ 1,
                                        TRUE ~0),
                     scen21ol=case_when(lengths(st_intersects(vmedb_sf, scen21)) > 0 ~ 1,
                                        TRUE ~0),
                     scen22ol=case_when(lengths(st_intersects(vmedb_sf, scen22)) > 0 ~ 1,
                                        TRUE ~0),
                     scen23ol=case_when(lengths(st_intersects(vmedb_sf, scen23)) > 0 ~ 1,
                                        TRUE ~0))


#### --------------------------------------------------------------------------------------------------- ####


#### --- Summarise the number of observations of each indicator and habitat type in each c-square ------ ####

# Data summaries of old and new records by indicator/habitat and c-square
# rename c-square field for future join
indCounts <- vmedb %>%
  filter(VME_Indicator %in% VMEindic)  %>%
  group_by(VME_Indicator,CSquare) %>%
  summarise(OldCount = sum(as.numeric(substr(InsDateTime,1,4))<datacallyear), 
            NewCount = sum(as.numeric(substr(InsDateTime,1,4))==datacallyear),
            TotCount = sum(!is.na(as.numeric(substr(InsDateTime,1,4))))) %>%
  rename(csquares=CSquare) %>%
  ungroup()

habCounts <- vmedb %>%
  filter(HabitatType %in% VMEhabs)  %>%
  group_by(HabitatType,CSquare) %>%
  summarise(OldCount = sum(as.numeric(substr(InsDateTime,1,4))<datacallyear), 
            NewCount = sum(as.numeric(substr(InsDateTime,1,4))==datacallyear),
            TotCount = sum(!is.na(as.numeric(substr(InsDateTime,1,4))))) %>%
  rename(csquares=CSquare) %>%
  ungroup()


# Combine tables - this table will be subset to region by selecting the relevant 
# c-squares and merging to the base bio table to summarise counts for all VME habitats and indicators
VMECounts <- rbind(habCounts %>%
                     rename(Name=HabitatType) %>% 
                     mutate(VMET='Habitat'),
                   indCounts  %>%
                     rename(Name=VME_Indicator) %>% 
                     mutate(VMET='Indic'))

#### --------------------------------------------------------------------------------------------------- ####

sf::sf_use_s2(FALSE)


#### --- Summarise by VME Grid ------------------------------------------------------------------------- ####

## Old VME grid
VMEgrid_old
# Check there are no duplicates in the c-squares
sum(duplicated(VMEgrid_old$csquares))
## New VME grid
VMEgrid_new
# Check there are no duplicates in the c-squares
sum(duplicated(VMEgrid_new$csquares))

# List of scenario feature names
grids <- c('VMEgrid_old','VMEgrid_new')
gridLabs <- data.frame(Grid=c('VMEgrid_old','VMEgrid_new'),ScenarioOpt=c('Previous VME Grid',paste('VME Grid',datacallyear)))

for (i in grids) {
  
  # Select scenario feature
  grd <- get(i)
  
  # polygon-specific html table for polygon ids
  idTbl <- NULL

for (j in unique(grd$csquares)){
  
  idTbl0 <- matrix(rbind(c(gridLabs[gridLabs$Grid==i,2],""),c('C-Square:', j)),ncol = 2)
  
  tempTbl <- idTbl0 %>%
    addHtmlTableStyle(align = "lr",css.cell = c("width: 400; font-weight: bold;","width: 150; font-weight: bold;")) %>%
    htmlTable()
  
  
  idTbl <- rbind(idTbl,data.frame(csquares=j,idTbl=tempTbl))
  
}


# polygon-specific html table for indicator species records
indTbl <- NULL

for (j in unique(grd$csquares)){
  
  indTbl0 <- indCounts[indCounts$csquares==j,-1]
  names(indTbl0) <- c('VME Indicator', 
                      paste('No. Rec prior to',datacallyear),
                      paste('No. Rec in',datacallyear),
                      'Total No. Rec')
  if(nrow(indTbl0)==0){
    indTbl0[1,'VME Indicator'] <- 'None'
    tempTbl <- indTbl0 %>%
      select(`VME Indicator`) %>%
      addHtmlTableStyle(align = "lcc",align.header= "lcc",css.cell = c("width: 550;")) %>%
      htmlTable(rnames = FALSE)
  } else {
    tempTbl <- indTbl0 %>%
    addHtmlTableStyle(align = "lcc",
                      align.header= "lcc",
                      css.cell = c("width: 250;","width: 150;","width: 150;","width: 150;")) %>%
    htmlTable(rnames = FALSE)
  }
  
  indTbl <- rbind(indTbl,data.frame(csquares=j,indTbl=tempTbl))
  
}

# polygon-specific html table for habitat records
habTbl <- NULL

for (j in unique(grd$csquares)){
  
  habTbl0 <- habCounts[habCounts$csquares==j,-1]
  names(habTbl0) <- c('VME Habitat', 
                      paste('No. Rec prior to',datacallyear),
                      paste('No. Rec in',datacallyear),
                      'Total No. Rec')
  if(nrow(habTbl0)==0){
    habTbl0[1,'VME Habitat'] <- 'None'
    tempTbl <- habTbl0 %>%
      select(`VME Habitat`) %>%
      addHtmlTableStyle(align = "lcc",align.header= "lcc",css.cell = c("width: 550;")) %>%
      htmlTable(rnames = FALSE)
  } else {
    tempTbl <- habTbl0 %>%
      addHtmlTableStyle(align = "lcc",
                        align.header= "lcc",
                        css.cell = c("width: 250;","width: 150;","width: 150;","width: 150;")) %>%
      htmlTable(rnames = FALSE)
  }
  
  habTbl <- rbind(habTbl,data.frame(csquares=j,habTbl=tempTbl))
  
}


# Add html tables to polygonn
grd <- grd %>%
        left_join(idTbl) %>%
        left_join(habTbl,) %>%
        left_join(indTbl)

# Compile final popup for each id 
grd$popTbl <- paste0(grd$idTbl,"<br>",grd$habTbl,"<br>",grd$indTbl)


# Replace NA 
grd <- grd %>% 
  replace(is.na(.), "") %>%
  select(-c(idTbl,habTbl,indTbl))

# Rename to replace original feature
assign(i,grd)

}


#### --- Summarise by scenario polygons ---------------------------------------------------------------- ####

# Prepare VME grid
# get all c-sq with closures old + new
VMEgrid <- VMEgrid_new %>%
              select(csquares)

# List of scenario feature names
scens <- c('scen11','scen12','scen21','scen22','scen23'
           #,'scen11_prev','scen12_prev','scen21_prev','scen22_prev','scen23_prev'
           )
scenLabs <- data.frame(Scen=c('scen11','scen12','scen21','scen22','scen23'
                              #,'scen11_prev','scen12_prev','scen21_prev','scen22_prev','scen23_prev'
                              ),
                       ScenarioOpt=c('Scenario: 1, Option: 1','Scenario: 1, Option: 2',
                                     'Scenario: 2, Option: 1','Scenario: 2, Option: 2',
                                     'Scenario: 1, Option: 2 with Scenario: 2, Option: 1',
                                     'Scenario: 1, Option: 1','Scenario: 1, Option: 2',
                                     'Scenario: 2, Option: 1','Scenario: 2, Option: 2',
                                     'Scenario: 1, Option: 2 with Scenario: 2, Option: 1'))



# Loop through all scenario feature sets to add needed columns
for (i in scens) {
    
    # Select scenario feature
    scen <- get(i)[,1]
    
    # polygon-specific html table for polygon ids
    idTbl <- NULL
    
    for (j in unique(scen$id)){
      
      idTbl0 <- matrix(rbind(c(scenLabs[scenLabs$Scen==i,2],""),c('Polygon ID:', j)),ncol = 2)
      
      tempTbl <- idTbl0 %>%
        addHtmlTableStyle(align = "lr",css.cell = c("width: 400; font-weight: bold;","width: 150; font-weight: bold;")) %>%
        htmlTable()
      
      
      idTbl <- rbind(idTbl,data.frame(id=j,idTbl=tempTbl))
      
    }
    

    # Calculate polygon areas
    scen <- scen %>%
      st_make_valid() %>% 
              mutate(PolyAr=round(st_area(scen)/1000000,1)) %>% 
              units::drop_units()

    # Calculate overlap with fisheries
    # Mobile gears
    overlapM <- scen %>% 
      st_intersection(New_mobile) %>% # Intersect with mobile gear area
      st_make_valid() %>% 
      mutate(intArMobile =round(100*(st_area(.)/1000000/PolyAr),1)) %>%  
      units::drop_units() %>% 
      dplyr::select(id,intArMobile) %>%   # only select columns needed to merge
      st_drop_geometry() # drop geometry as we don't need it
    # Static gears
    overlapS <- scen %>% 
      st_intersection(New_static) %>% # Intersect with PolyAr gear area
      st_make_valid() %>% 
      mutate(intArStatic =round(100*(st_area(.)/1000000/PolyAr),1)) %>%  
      units::drop_units() %>% 
      dplyr::select(id, intArStatic) %>%   # only select columns needed to merge
      st_drop_geometry() # drop geometry as we don't need it
    # Combined gears
    overlapC <- scen %>% 
      st_intersection(New_comb) %>% # Intersect with mobile gear area
      st_make_valid() %>% 
      mutate(intArComb =round(100*(st_area(.)/1000000/PolyAr),1)) %>%  
      units::drop_units() %>% 
      dplyr::select(id,intArComb) %>%   # only select columns needed to merge
      st_drop_geometry() # drop geometry as we don't need itop geometry as we don't need it
    
    # Add area overlaps to polygons
    scen <- scen %>%
              left_join(overlapM) %>%
              left_join(overlapS) %>%
              left_join(overlapC) %>%
              replace(is.na(.), 0)
    
    # polygon-specific html table for polygon area and fishing overlaps
    arTbl <- NULL
    
    for (j in unique(scen$id)){
      
      arTbl0 <- scen %>%
                  st_drop_geometry() %>%
                  filter(id==j) %>%
                  select(PolyAr,intArMobile,intArStatic,intArComb) %>%
                  rename(c('Polygon area (km<sup>2</sup>)'= 'PolyAr',
                           'Proportion fished with mobile gears (%)'='intArMobile',
                           'Proportion fished with static gears (%)'='intArStatic',
                           'Proportion fished with any gear (%)'='intArComb')) %>%
                  pivot_longer(cols=everything(), names_to = "RN", values_to = "value") %>%
                  tibble::add_column(space = ' ', .after = 1)
      
      arTbl0 <- as.matrix(arTbl0)
      colnames(arTbl0)<-NULL

      tempTbl <- arTbl0 %>%
        addHtmlTableStyle(align = "lr",css.cell = c("width: 400;font-weight: bold;","width: 150;","width: 150;")) %>%
        htmlTable(rnames = FALSE)
      
      arTbl <- rbind(arTbl,data.frame(id=j,arTbl=tempTbl))
      
    }
    
    
    # Assign VME grid cells to closure
    intVME <- st_join(VMEgrid,scen,join = st_within,left=FALSE)
    
    # Which C-Squares are closures made of
    scenSq <- intVME %>%
      st_drop_geometry() %>%
      pull(csquares)
    # Add field to VMECounts indicating if square is part of scenario polygons
    VMECounts <- VMECounts %>%
      mutate({{i}} := case_when(csquares %in% scenSq ~1))
    
    # Calculate the number of records of each indicator and habitat in the polygons
    intVMEInd <- intVME %>%
                    st_drop_geometry() %>%
                    inner_join(indCounts) %>%
                    group_by(id,VME_Indicator) %>%
                    summarise(TotCount = sum(TotCount),
                              TotSq = n()) %>%
                    ungroup()
    
    intVMEHab <- intVME %>%
                    st_drop_geometry() %>%
                    inner_join(habCounts) %>%
                    group_by(id,HabitatType) %>%
                    summarise(TotCount = sum(TotCount),
                              TotSq = n()) %>%
                    ungroup()
    
    # polygon-specific html table for indicator species records
    indTbl <- NULL
    
    for (j in unique(scen$id)){
      
      indTbl0 <- intVMEInd[intVMEInd$id==j,-1]
      names(indTbl0) <- c('VME Indicator', 'No. Records','No. C-Sq with Records')
      if(nrow(indTbl0)==0){
        indTbl0[1,'VME Indicator'] <- 'None'
      }
      
      
      tempTbl <- indTbl0 %>%
                  addHtmlTableStyle(align = "lcc",align.header= "lcc",css.cell = c("width: 250;","width: 150;","width: 150;")) %>%
                  htmlTable(rnames = FALSE)
        
        
      indTbl <- rbind(indTbl,data.frame(id=j,indTbl=tempTbl))
      
    }
    
    # polygon-specific html table for habitat records
    habTbl <- NULL
    
    for (j in unique(scen$id)){
      
      habTbl0 <- intVMEHab[intVMEHab$id==j,-1]
      names(habTbl0) <- c('VME Habitat', 'No. Records','No. C-Sq with Records')
      if(nrow(habTbl0)==0){
        habTbl0[1,'VME Habitat'] <- 'None'
      }
      
      tempTbl <- habTbl0 %>%
        addHtmlTableStyle(align = "lcc",align.header= "lcc",css.cell = c("width: 250;","width: 150;","width: 150;")) %>%
        htmlTable(rnames = FALSE)
      
      
      habTbl <- rbind(habTbl,data.frame(id=j,habTbl=tempTbl))
      
    }

    
    # Add html tables to scenario polygonn
    scen <- scen %>%
      merge(idTbl,all.x=TRUE) %>%
      merge(arTbl,all.x=TRUE) %>%
      merge(habTbl,all.x=TRUE) %>%
      merge(indTbl,all.x=TRUE)
    
    # Compile final popup for each id 
    scen$popTbl <- paste0(scen$idTbl,"<br>",scen$arTbl,"<br>",scen$habTbl,"<br>",scen$indTbl)
    
    
    # Replace NA 
    scen <- scen %>% 
      replace(is.na(.), "") %>% 
      units::drop_units()  %>%
      select(-c(idTbl,habTbl,indTbl))
    

    
    # Rename to replace original scenario feature
    assign(i,scen)
    
}

rm(i, j, scen, scens,scenLabs,arTbl,arTbl0,habCounts,habTbl,habTbl0,idTbl,idTbl0,indCounts,
   indTbl,indTbl0,intVME,intVMEHab,intVMEInd,overlapC,overlapM,overlapS,vmedb,VMEhabs,VMEindic,tempTbl,
   CSquare)

#### --------------------------------------------------------------------------------------------------- ####
