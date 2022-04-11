####################################################################
#### make the two leaflet maps - based on the scientific template ####
####################################################################

# load footprint workspace
  load(paste(pathdir,"2-Data processing/Map_layer_workspace.RData",sep="/"))  

# select all new c-squares
# does this work when c-squares are being removed? / if updated it works!
  VMEgrid_old$uni  <- paste(VMEgrid_old$csquares,VMEgrid_old$VME_Class)  
  VMEgrid_new$uni  <- paste(VMEgrid_new$csquares,VMEgrid_new$VME_Class)
  VMEgrid_new      <- VMEgrid_new[!(VMEgrid_new$uni %in% VMEgrid_old$uni ),]

# names for the leaflet
  nam <- c("Ecoregion boundaries","EEZ boundaries","Depth zone 400-800m",
           "Existing VME C-sq.","New VME C-sq.","EU fishable domain (prelim.)",
           "Fished area (S+M)","Reference fished area (S)","Fished area (S)",
           "Reference fished area (M)","Fished area (M)",
           "Existing VME physical elements","Updated VME physical elements (prelim.)",
           "NEAFC Convention Area","Existing NEAFC Closures","NEAFC fishable domain")
 
# Create colour palette for VMEs
  VMEcolours <- c("#2E8AC6","#F40000","#F67E11","#FDF100")
  VMEpal <- colorFactor(VMEcolours, VMEgrid_old$VME_Class_Lab)  
  
  # set region
  mxt <- st_bbox(ICESEcReg)
  
# 
  mfs <- leaflet() %>%
    fitBounds(mxt[[1]],mxt[[2]],mxt[[3]],mxt[[4]]-20) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
   # boundaries
    addPolygons(data = ICESEcReg, group = nam[1],
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    addPolygons(data = shapeEEZ, group = nam[2],
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    addPolygons(data = Reg_depth, group = nam[3],
                stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
    
    # VME c-sqs
    addPolygons(data = VMEgrid_old, group = nam[4],
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,
                color = ~VMEpal(VME_Class_Lab)) %>%
    
    addPolygons(data = VMEgrid_new, group = nam[5],
                stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,
                color = ~VMEpal(VME_Class_Lab)) %>%
    
    # EU footprint and fishing areas
    addPolygons(data = EUFootp, group = nam[6],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "white") %>%
    
    addPolygons(data = New_comb, group = nam[7],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "red") %>%
    
    addPolygons(data = Ref_static, group = nam[8],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "white") %>%
    
    addPolygons(data = New_static, group = nam[9],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "red") %>%
    
    addPolygons(data = Ref_mobile, group = nam[10],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "white") %>%
    
    addPolygons(data = New_mobile, group = nam[11],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "red") %>%
    
    # elements
    addPolygons(data = Elements, group = nam[12],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "white") %>%
    
    addPolygons(data = Elements, group = nam[13],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "red") %>%
    
    # NEAFC information
    addPolygons(data = NEAFCReg, group = nam[14],
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "red") %>%
    addPolygons(data = clos_neafc, group = nam[15],
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    addPolygons(data = NEAFCFootp, group = nam[16],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "white") %>%
    
    addLegend(group = "VME Class",
              position = "bottomright",
              colors = VMEcolours, labels= levels(VMEgrid_old$VME_Class_Lab)) %>%
    
    # Layers control
    addLayersControl(
      overlayGroups = nam[1:16],
      options = layersControlOptions(collapsed = FALSE))%>%
    
    # hide 
    hideGroup(nam[5:16])
    
   
# save output
  outdir <- paste(pathdir,"Output",sep="/") 
  setwd(outdir)
  saveWidget(mfs, file="Leaflet map figure 1.html")

#### figure 2
  # names for the leaflet
  nam <- c("Ecoregion boundaries","EEZ boundaries", 
           "Depth zone 400-800m (prelim.)", "Existing NEAFC closures",
           "Existing VME polygon S1 O1","New VME polygon S1 O1",
           "Existing VME polygon S1 O2","New VME polygon S1 O2",
           "Existing VME polygon S2 O1","New VME polygon S2 O1",
           "Existing VME polygon S2 O2","New VME polygon S2 O2",
           "Existing VME polygon S2 O3","New VME polygon S2 O3",
           "NEAFC fishable domain","NEAFC Convention Area",
           "EU fishable domain (prelim.)")
  
  # add information for scenario 1 option 1 closures
  scen <- scen11
  
  # get all c-sq with closures old + new
  VMEgrid <- subset(VMEgrid_old,!(VMEgrid_old$csquares %in% VMEgrid_new$csquares))
  VMEgrid <- rbind(VMEgrid,VMEgrid_new)
  
  # Assign VME grid cells to closure
  intVME <- st_join(VMEgrid,scen)
  # Add area (m2)
  intVME$aream2 <- st_area(intVME)
  
  # Create table with sum of the area in each VME category by closure
  VMEareas <- intVME %>%
    st_drop_geometry() %>%
    filter(!is.na(id)) %>%
    pivot_wider(id_cols = id,
                names_from = VME_Class,
                values_from = aream2,
                values_fn = sum) %>%
    mutate(across(2:5,~as.integer(.)))  %>%
    mutate(across(everything(),~replace_na(.x, 0)))
  
  # Add the area to closure polygons attribute table
  scen <- merge(scen,VMEareas,by='id')
  # Calculate area of closure
  scen$closureA <- st_area(scen)
  # Calculate percent area of closure in each VME class
  scen$pctHab <- round(100*(scen$`3`/scen$closureA))
  scen$pctHigh <- round(100*(scen$`2`/scen$closureA))
  scen$pctMed <- round(100*(scen$`1`/scen$closureA))
  scen$pctLow <- round(100*(scen$`0`/scen$closureA))
 
  # set region
  mxt <- st_bbox(ICESEcReg)
   
  # 
  mfs2 <- leaflet() %>%
    fitBounds(mxt[[1]],mxt[[2]],mxt[[3]],mxt[[4]]-20) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addMapPane("scens", zIndex = 420) %>%
    # boundaries
    addPolygons(data = ICESEcReg, group = nam[1],
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    addPolygons(data = shapeEEZ, group = nam[2],
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    addPolygons(data = Reg_depth, group = nam[3],
                stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "yellow") %>%
    
    # existing closures
    addPolygons(data = clos_neafc, group = nam[4],
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "white") %>%
    
    addPolygons(data = scen11_prev, group=nam[5],
                stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 2, color = "orange") %>%
    
    # potential closures
    addPolygons(data = scen,
                group = nam[6],
                layerId = scen$id,
                stroke = FALSE, 
                fillOpacity = 0.5,
                #smoothFactor = 0.5,
                fillColor =  "white",
                color = "transparent",
                highlightOptions = highlightOptions(stroke='white',
                                                    color = "white",
                                                    fillOpacity = 1,
                                                    weight = 2,),
                popup = paste0("<b>Closure area: </b>", round(scen$closureA/1000000,1), " Km<sup>2</sup>",
                               "<br><br><b>Proportion of VME:</b>",
                               "<table>
                                      <tr>
                                      <th>Category</th>
                                      <th>% of Area</th>
                                      </tr>
                                      <tr>
                                      <td>VME Habitat</td>
                                      <td align='right'>", scen$pctHab, "</td>
                                      </tr>
                                      <tr>
                                      <td>VME Index - High</td>
                                      <td align='right'>",scen$pctHigh,"</td>
                                      </tr>
                                      <tr>
                                      <td>VME Index - Mod</td>
                                      <td align='right'>",scen$pctMed,"</td>
                                      </tr>
                                      <tr>
                                      <td>VME Index - Mod</td>
                                      <td align='right'>",scen$pctLow,"</td>
                                      </tr>
                                      </table>"),
                options = pathOptions(pane = "scens")) %>%
    
    addPolygons(data = scen12_prev, group=nam[7],
                stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 2, color = "red") %>%
    addPolygons(data = scen12, group=nam[8],fillColor =  "white", color = "transparent",stroke = FALSE, fillOpacity = 0.5) %>%
    addPolygons(data = scen21_prev, group=nam[9],
                stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 2, color = "#bcbddc") %>%
    addPolygons(data = scen21, group=nam[10],fillColor =  "white", color = "transparent",stroke = FALSE, fillOpacity = 0.5) %>%
        addPolygons(data = scen22_prev, group=nam[11],
                stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 2, color = "#c7e9c0") %>%
    addPolygons(data = scen22, group=nam[12],fillColor =  "white", color = "transparent",stroke = FALSE, fillOpacity = 0.5) %>%
    #addPolygons(data = NULL, group=nam[13],
    #            stroke = TRUE, fillOpacity = 0.1, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "#c7e9c0") %>%
    addPolygons(data = scen23, group=nam[14],fillColor =  "white", color = "transparent",stroke = FALSE, fillOpacity = 0.5) %>%
    
    
    # existing footprints
    addPolygons(data = NEAFCFootp, group = nam[15],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "white") %>%
    
    addPolygons(data = NEAFCReg, group = nam[16],
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.5, opacity = 0.5, weight = 1, color = "red") %>%
    
      addPolygons(data = EUFootp, group = nam[17],
                stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor =  "white") %>%
    

    # Layers control
    addLayersControl(
      overlayGroups = nam[c(1:12,14:17)],
      options = layersControlOptions(collapsed = FALSE)) %>%
  
  # hide 
  hideGroup(nam[c(6:12,14:17)])
  
  
  # save output
  outdir <- paste(pathdir,"Output",sep="/") 
  setwd(outdir)
  saveWidget(mfs2, file="Leaflet map figure 2.html")
  
  