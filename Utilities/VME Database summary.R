#############################################################################################################
#########     Summarise database contents by c-square and VME Polygon       #################################
#########     AD  11/05/2022                                                #################################
#############################################################################################################

# load the VME database extraction
vmedb <-  read.csv(paste(pathdir_nogit,paste(
                       "VME data repository/VME observations and csquares/VME_observations_datacall_",
                        datacallyear,"_eu.csv",sep=""),sep="/"), header=T,sep=",",row.names = NULL)

source(paste(pathdir,"Utilities/coords_to_csquare_VMStools.R",sep="/"))
vmedb$CSquare <- CSquare(vmedb$MiddleLongitude,vmedb$MiddleLatitude,0.05)   

#### --------------------------------------------------------------------------------------------------- ####


#### --- Base table for  tables in bio tab in compilation Markdowns ------------------------------------ ####

# Current lists of VME indicators and VME habitats
VMEindic <- c('Anemones','Black coral','Cup coral','Chemosynthetic species (seeps and vents)','Gorgonian','Soft coral','Stalked crinoids (sea lilies)','Sponge','Sea-pen','Stylasterids','Stony coral','Xenophyophores')
VMEhabs <- c('Anemone aggregations','Bryozoan patches','Cold-water coral reef','Coral garden','Cold seeps','Deep-sea sponge aggregations','Hydrothermal vents/fields','Mud and sand emergent fauna','Stalked crinoid aggregations','Sea-pen fields','Tube-dwelling anemone aggregations','Xenophyophore aggregations')

# Set up table with all indicators and habitats
bioTblBase <- rbind(data.frame(VMEclass='VME Habitat',Name=VMEhabs),
                    data.frame(VMEclass='VME Indicators',Name=VMEindic))

#### --- Summarise the number of observations of each indicator and habitat type in each c-square ------ ####

# Data summaries of old and new records by indicator/habitat and c-square
indCounts <- vmedb %>%
  filter(VME_Indicator!='NULL')  %>%
  group_by(VME_Indicator,CSquare) %>%
  summarise(OldCount = sum(as.numeric(substr(InsDateTime,1,4))<datacallyear), 
            NewCount = sum(as.numeric(substr(InsDateTime,1,4))==datacallyear),
            TotCount = sum(!is.na(as.numeric(substr(InsDateTime,1,4)))))

habCounts <- vmedb %>%
  filter(HabitatType!='NULL')  %>%
  group_by(HabitatType,CSquare) %>%
  summarise(OldCount = sum(as.numeric(substr(InsDateTime,1,4))<datacallyear), 
            NewCount = sum(as.numeric(substr(InsDateTime,1,4))==datacallyear),
            TotCount = sum(!is.na(as.numeric(substr(InsDateTime,1,4)))))


# Combine tables - this table will be subset to region by selecting the relevant 
# c-squares and merging to the base bio table to summarise counts for all VME habitats and indicators
VMECounts <- rbind(habCounts %>%
                     rename(Name=HabitatType),
                   indCounts  %>%
                     rename(Name=VME_Indicator))

#### --------------------------------------------------------------------------------------------------- ####


#### --- Summarise by scenario polygons ---------------------------------------------------------------- ####

# Prepare VME grid
# get all c-sq with closures old + new
# VMEgrid <- subset(VMEgrid_old,!(VMEgrid_old$csquares %in% VMEgrid_new$csquares))
# VMEgrid <- rbind(VMEgrid,VMEgrid_new)
 VMEgrid <- VMEgrid_new

# List of scenario feature names
scens <- c('scen11','scen12','scen21','scen22','scen23')
scenLabs <- data.frame(Scen=c('scen11','scen12','scen21','scen22','scen23'),ScenarioOpt=c('Scenario: 1, Option: 1','Scenario: 1, Option: 2','Scenario: 2, Option: 1','Scenario: 2, Option: 2','Scenario: 1, Option: 2 with Scenario: 2, Option: 1'))



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
      dplyr::select(id,intArStatic) %>%   # only select columns needed to merge
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
              merge(overlapM,all.x=TRUE) %>%
              merge(overlapS,all.x=TRUE) %>%
              merge(overlapC,all.x=TRUE)
    
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
    intVME <- st_join(VMEgrid,scen)
    
    # Calculate the number of records of each indicator and habitat in the polygons
    intVMEInd <- intVME %>%
                    st_drop_geometry() %>%
                    filter(!is.na(id)) %>%
                    merge(indCounts,by.x="csquares",by.y="CSquare") %>%
                    group_by(id,VME_Indicator) %>%
                    summarise(TotCount = sum(TotCount),
                              TotSq = n())
    
    intVMEHab <- intVME %>%
                    st_drop_geometry() %>%
                    filter(!is.na(id)) %>%
                    merge(habCounts,by.x="csquares",by.y="CSquare") %>%
                    group_by(id,HabitatType) %>%
                    summarise(TotCount = sum(TotCount),
                              TotSq = n())
    
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
      units::drop_units() 
    

    
    # Rename to replace original scenario feature
    assign(i,scen)
    
}

rm(i, j, scen, scens,scenLabs,arTbl,arTbl0,habCounts,habTbl,habTbl0,idTbl,idTbl0,indCounts,
   indTbl,indTbl0,intVME,intVMEHab,intVMEInd,overlapC,overlapM,overlapS,vmedb,VMEhabs,VMEindic,tempTbl,
   CSquare)

#### --------------------------------------------------------------------------------------------------- ####


#### --- Save the outputs ------------------------------------------------------------------------------ ####
#
#save(bioTblBase,VMECounts,scen11,scen12,scen21,scen22,scen23,
#     file=paste(pathdir,"2-Data processing",paste("VME_polygons",datacallyear,sep="_"),
#     "BioTabAndScenarioFeaturesWithAttributes.RData",sep="/"))

#### --------------------------------------------------------------------------------------------------- ####
  