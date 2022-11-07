
#-------------------------------------------------------------------------------------
# Assessment script ICES VME advice
# last update May 2022 (Anna Downie, Daniel van Denderen)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Notes:
# The outputs are generated following the ICES VME benchmark
# workshop. The workshop report can be found here: xxx 

# Before running the script, please download all  
# VME and VMS restricted data - these data are on the ICES Sharepoint
# they can be obtained by contacting the chairs of the ICES working  
# group on Deep Water Ecology: 
# https://www.ices.dk/community/groups/pages/wgdec.aspx

#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
# step 1 - assessment year, rlibraries and folder structure
#------------------------------------------------------------------------------------- 
 
  datacallyear     <- 2022   # VME data call year with latest data
  datacallyear_VMS <- 2021   # VMS data call year with latest data
  
  # set path to the VME advice folder
  pathdir <- "C:/Users/neilm/Documents/projects/VME-Advice/"
  
  # set path to the folder with VMS and VME data (restricted data - outside github)
  pathdir_nogit <- "C:/Users/neilm/Documents/VME-advice_noGIT/EU" 
  
  # R libraries used
  source(paste(pathdir,"Utilities/Libraries_VMEadvice.R",sep="/"))  
  
  
#-------------------------------------------------------------------------------------  

  
  
#-------------------------------------------------------------------------------------
# step 2 - run scenarios and options to obtain VME polygons
#-------------------------------------------------------------------------------------
    
  # create folder for the VME datacall year
  dir.create(paste(pathdir,"2-Data processing",paste("VME_polygons",datacallyear,sep="_"),sep="/"))

  # run scenario 1 - option 1 & 2 (warnings are okay)
  source(paste(pathdir,"Utilities/Scenario_1_option_1.R",sep="/"))  
  source(paste(pathdir,"Utilities/Scenario_1_option_2.R",sep="/")) 
  
  # run scenario 2 - option 1& 2 (warnings are okay)
  
  
  refyear       <- 2009:(datacallyear-1)   # specify years to estimate sar threshold
  SAR_threshold <- 0.43                  # SAR threshold value
  source(paste(pathdir,"Utilities/Scenario_2_option_1.R",sep="/")) 
  source(paste(pathdir,"Utilities/Scenario_2_option_2.R",sep="/"))
  
  # run the combined S1-O2 + S2-O1 scenario (warnings are okay)
  # termed S2-O3 in all scripts
  source(paste(pathdir,"Utilities/Scenario_2_option_3.R",sep="/")) 
  
  rm(list=setdiff(ls(), c("datacallyear","datacallyear_VMS","pathdir","pathdir_nogit")))
  
#-------------------------------------------------------------------------------------

    
#-------------------------------------------------------------------------------------
# step 3 - load fishable domains and obtain fishing layers
#-------------------------------------------------------------------------------------

  # load EU + NEAFC polygons with fishable domain
  source(paste(pathdir,"Utilities/Footprint_current.R",sep="/"))  

  # obtain fished area based on the latest VMS datacall
  refyear_fished <- 2009:2011
  newyear_fished <- (datacallyear_VMS-1):(datacallyear_VMS-3)
  source(paste(pathdir,"Utilities/Fishedarea_newVMS.R",sep="/")) 
  
  # create new folder for the fishing layers based on latest VMS datacall
  dir.create(paste(pathdir,"2-Data processing",paste("Fishing_layers",datacallyear_VMS,sep="_"),sep="/"))

  # save    save(EUFootp,NEAFCFootp,New_comb,New_mobile,New_static, Ref_comb,Ref_mobile,Ref_static,
       file=paste(pathdir,"2-Data processing",paste("Fishing_layers",datacallyear_VMS,sep="_"),
                  "Fishing_workspace.RData",sep="/")

#------------------------------------------------------------------------------------
# Note on fishable domain: 
# if ICES is requested to update the EU fishing footprint
# the Footprint_update_newVMS.R can be run (see folder utilities)
       # Need to specify the refyear_footprint to run update with

#------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# step 4 - prepare all outputs to run the rmarkdowns 
#-------------------------------------------------------------------------------------
# Note:
# The script will provide all outputs based on the data call year
# it will overwrite any previous version 
# to return to a previous year, change the data call year above and re-run the below
       
  source(paste(pathdir,"Utilities/Compile_spatial_data_layers.R",sep="/"))  
  save.image(file = paste(pathdir,"2-Data processing/Map_layers_workspace.RData",sep="/"))

  # compile output for table 1 of the RMarkdown
  source(paste(pathdir,"Utilities/Compile_table1_underlyingdata.R",sep="/"))  
  
#------------------------------------------------------------------------------------

  
#------------------------------------------------------------------------------------
# step 5 - run the RMD files from the output folder
#------------------------------------------------------------------------------------

  #  EU_VME.RMD
  #  ICES_Ecoregions_VME.RMD
  #  NEAFC_VME.RMD (not yet available)
  
  
  
  
